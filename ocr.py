"""
AutoCAD Engineering Drawing — Dimension Highlighter v3
======================================================
Features:
  • Multi-angle OCR (0°, 90° CW, 90° CCW) — catches vertical dimensions
  • Radius (R / RAD) and Diameter (Ø / DIA / D) dimension detection
  • Tolerance detection: +X.XXX/-Y.YYY each with [metric] counterpart
  • ±X.XXX [±Y.YY] symmetric tolerance detection
  • Image preprocessing: contrast + sharpen for fine text accuracy
  • 400 DPI rendering
  • Improved highlight positioning — tight per-token bboxes, padded cleanly
  • Color logic (simple and consistent):
      🟡 Yellow = dimension pair CORRECT (covers both inch + metric values)
      🔴 Red = dimension pair WRONG (inch→metric conversion mismatch)
      🟡 Yellow = tolerance pair CORRECT
      🔴 Red = tolerance pair WRONG

Usage:
  python3 highlight_dimensions_v3.py input.pdf output_highlighted.pdf
"""

import sys, re
from PIL import Image, ImageEnhance, ImageFilter
import pytesseract
from pdf2image import convert_from_path
from pypdf import PdfReader, PdfWriter
from pypdf.generic import (
    ArrayObject, FloatObject, NameObject, NumberObject, DictionaryObject
)
import pdfplumber

# ── Config ────────────────────────────────────────────────────────────────────
DPI = 400
TOLERANCE_PCT = 0.025 # 2.5% rounding tolerance (covers drawing rounding)
ANGLES = [0, 90, 270]
HIGHLIGHT_PAD = 2 # px padding around each highlight box


# ══════════════════════════════════════════════════════════════════════════════
# PATTERNS
# ══════════════════════════════════════════════════════════════════════════════

# Opening/closing bracket variants (OCR sometimes reads [ as ( or {)
OB = r'[\[({]'
CB = r'[\])}]'

# Signed number (handles: 1.5 | .005 | +.005 | -0.12 | 1,234)
NUM = r'[+-]?\s*\d*\.?\d[\d,]*'

# ── 1. Standard linear pair: 1.234 [31.34] ───────────────────────────────────
LINEAR_RE = re.compile(
    rf'({NUM})' # group1: inch value
    rf'\s*{OB}\s*'
    rf'({NUM})' # group2: metric value
    rf'\s*{CB}',
    re.IGNORECASE
)

# ── 2. Radius: R1.250 [31.75] or RAD 1.250 [31.75] ──────────────────────────
RADIUS_RE = re.compile(
    rf'(?:R|RAD\.?)\s*({NUM})' # group1: inch radius value
    rf'\s*{OB}\s*'
    rf'({NUM})' # group2: metric radius value
    rf'\s*{CB}',
    re.IGNORECASE
)

# ── 3. Diameter: Ø1.250 [31.75] or DIA 1.250 [31.75] or ⌀1.250 [31.75] ─────
DIA_RE = re.compile(
    rf'(?:[ØO⌀]|DIA\.?|DIAM\.?)\s*({NUM})' # group1: inch diameter value
    rf'\s*{OB}\s*'
    rf'({NUM})' # group2: metric diameter value
    rf'\s*{CB}',
    re.IGNORECASE
)

# ── 4. Stacked tolerance pair: +.005 [+.13] followed by -.000 [-.00] ─────────
# Matches two consecutive signed dimension+bracket pairs on adjacent lines
TOL_STACK_RE = re.compile(
    rf'([+]\s*{NUM})' # group1: positive inch tol
    rf'\s*{OB}\s*'
    rf'([+]?\s*{NUM})' # group2: positive metric tol
    rf'\s*{CB}'
    rf'\s*'
    rf'([-]\s*{NUM})' # group3: negative inch tol
    rf'\s*{OB}\s*'
    rf'([-]?\s*{NUM})' # group4: negative metric tol
    rf'\s*{CB}',
    re.IGNORECASE
)

# ── 5. Symmetric tolerance: ±0.005 [±0.13] or +/-0.005 [+/-0.13] ────────────
TOL_SYM_RE = re.compile(
    rf'(?:[±]|[+][/-])\s*({NUM})' # group1: inch symmetric tol
    rf'\s*{OB}\s*'
    rf'(?:[±]|[+][/-])?\s*({NUM})' # group2: metric symmetric tol
    rf'\s*{CB}',
    re.IGNORECASE
)

# ── 6. Explicit inch marker without metric: 2.5" | 1/4 in ───────────────────
INCH_ONLY_RE = re.compile(
    r'(\d+(?:\.\d+)?|\d+/\d+)\s*(?:"|in(?:ch(?:es)?)?)\b',
    re.IGNORECASE
)


# ══════════════════════════════════════════════════════════════════════════════
# HELPERS
# ══════════════════════════════════════════════════════════════════════════════

def clean_num(s):
    """Strip spaces OCR inserted inside a number, strip sign chars, then parse."""
    return float(re.sub(r'[\s,]', '', s))

def clean_abs(s):
    return abs(clean_num(re.sub(r'[^0-9.,]', '', s)))

def verify(inch_val, metric_val):
    """Return True if metric_val is within TOLERANCE_PCT of inch_val * 25.4."""
    if abs(inch_val) < 1e-9:
        return abs(metric_val) < 0.1
    expected = abs(inch_val) * 25.4
    return abs(expected - abs(metric_val)) / expected <= TOLERANCE_PCT


# ══════════════════════════════════════════════════════════════════════════════
# IMAGE PREPROCESSING
# ══════════════════════════════════════════════════════════════════════════════

def preprocess(img):
    """Boost contrast and sharpen for cleaner OCR on engineering drawings."""
    img = img.convert("L")
    img = ImageEnhance.Contrast(img).enhance(2.2)
    img = ImageEnhance.Sharpness(img).enhance(2.5)
    img = img.filter(ImageFilter.SHARPEN)
    return img.convert("RGB")


# ══════════════════════════════════════════════════════════════════════════════
# COORDINATE TRANSFORMS (rotated-image pixel → original-image pixel)
# ══════════════════════════════════════════════════════════════════════════════

def unrotate_bbox(rx0, ry0, rx1, ry1, angle, orig_W, orig_H):
    """
    Map a bounding box found in a rotated image back to the original image space.
    angle: degrees CCW that the original was rotated to produce the OCR image.
    """
    if angle == 0:
        return rx0, ry0, rx1, ry1

    elif angle == 90:
        # Rotated 90° CCW: rotated(rx, ry) → original(ry, orig_W - rx)
        # rotated image size = (orig_H, orig_W)
        ox0 = ry0
        oy0 = orig_W - rx1
        ox1 = ry1
        oy1 = orig_W - rx0
        return max(0, ox0), max(0, oy0), min(orig_W, ox1), min(orig_H, oy1)

    elif angle == 270:
        # Rotated 90° CW: rotated(rx, ry) → original(orig_H - ry, rx)
        # rotated image size = (orig_H, orig_W)
        ox0 = orig_H - ry1
        oy0 = rx0
        ox1 = orig_H - ry0
        oy1 = rx1
        return max(0, ox0), max(0, oy0), min(orig_W, ox1), min(orig_H, oy1)

    return rx0, ry0, rx1, ry1 # fallback


# ══════════════════════════════════════════════════════════════════════════════
# PDF ANNOTATION
# ══════════════════════════════════════════════════════════════════════════════

def make_highlight(x0, y0, x1, y1, color):
    """Create a PDF highlight annotation at the given PDF-space coordinates."""
    p = HIGHLIGHT_PAD
    x0 -= p; y0 -= p; x1 += p; y1 += p
    return DictionaryObject({
        NameObject("/Type"): NameObject("/Annot"),
        NameObject("/Subtype"): NameObject("/Highlight"),
        NameObject("/Rect"): ArrayObject([FloatObject(v) for v in [x0, y0, x1, y1]]),
        NameObject("/QuadPoints"): ArrayObject([
            FloatObject(x0), FloatObject(y1),
            FloatObject(x1), FloatObject(y1),
            FloatObject(x0), FloatObject(y0),
            FloatObject(x1), FloatObject(y0),
        ]),
        NameObject("/C"): ArrayObject([FloatObject(c) for c in color]),
        NameObject("/CA"): FloatObject(0.6),
        NameObject("/F"): NumberObject(4),
    })

YELLOW = (1.0, 0.90, 0.0) # correct
RED = (1.0, 0.10, 0.10) # mismatch


# ══════════════════════════════════════════════════════════════════════════════
# TOKEN EXTRACTION
# ══════════════════════════════════════════════════════════════════════════════

def extract_tokens_at_angle(base_img, angle, orig_W, orig_H):
    """
    Rotate base_img by `angle` degrees CCW, run OCR, then unrotate all bboxes
    back into original image pixel space. Returns list of token dicts.
    """
    if angle == 0:
        rot = base_img
    else:
        rot = base_img.rotate(angle, expand=True)

    proc = preprocess(rot)

    # psm 11 = sparse text (any orientation), oem 3 = best LSTM engine
    cfg = "--oem 3 --psm 11"
    ocr = pytesseract.image_to_data(proc, config=cfg,
                                     output_type=pytesseract.Output.DICT)

    tokens = []
    for i in range(len(ocr['text'])):
        txt = ocr['text'][i].strip()
        if not txt:
            continue
        try:
            conf = float(ocr['conf'][i])
        except (ValueError, TypeError):
            conf = 0
        if conf < 15: # drop very low-confidence noise
            continue

        rx0 = ocr['left'][i]
        ry0 = ocr['top'][i]
        rx1 = rx0 + max(ocr['width'][i], 1)
        ry1 = ry0 + max(ocr['height'][i], 1)

        bx0, by0, bx1, by1 = unrotate_bbox(rx0, ry0, rx1, ry1,
                                             angle, orig_W, orig_H)
        tokens.append({
            'text': txt,
            'x0': bx0, 'y0': by0,
            'x1': bx1, 'y1': by1,
            'conf': conf,
            'angle': angle,
        })

    return tokens


def build_stream(tokens):
    """
    Concatenate all token texts into one searchable string.
    Returns (stream, char_to_token_index mapping).
    """
    stream = ""
    c2t = {}
    for ti, tok in enumerate(tokens):
        start = len(stream)
        for ci in range(len(tok['text'])):
            c2t[start + ci] = ti
        stream += tok['text']
        c2t[len(stream)] = ti # also map the space
        stream += " "
    return stream, c2t


def bbox_for_span(s, e, tokens, c2t):
    """
    Return the union bounding box of all tokens whose characters overlap
    the char range [s, e) in the stream. Returns None if no tokens found.
    """
    tis = set()
    for ci in range(s, min(e, len(c2t))):
        if ci in c2t:
            tis.add(c2t[ci])
    if not tis:
        return None
    return (
        min(tokens[i]['x0'] for i in tis),
        min(tokens[i]['y0'] for i in tis),
        max(tokens[i]['x1'] for i in tis),
        max(tokens[i]['y1'] for i in tis),
    )


# ══════════════════════════════════════════════════════════════════════════════
# MAIN PROCESSING
# ══════════════════════════════════════════════════════════════════════════════

def process(input_path, output_path):
    print(f"\n📄 {input_path}")
    print(f" DPI={DPI} Angles={ANGLES} Tolerance={TOLERANCE_PCT*100:.1f}%\n")

    # Get PDF page dimensions in points (1 point = 1/72 inch)
    with pdfplumber.open(input_path) as pdf:
        pdf_dims = [(p.width, p.height) for p in pdf.pages]

    images = convert_from_path(input_path, dpi=DPI)
    print(f" Pages: {len(images)}\n")

    all_page_annots = []
    stats = {'correct': 0, 'mismatch': 0, 'tol_correct': 0, 'tol_mismatch': 0,
             'radius': 0, 'diameter': 0}

    for page_idx, (base_img, (pdf_w, pdf_h)) in enumerate(zip(images, pdf_dims)):
        orig_W, orig_H = base_img.size

        # Scale: image pixels → PDF points
        sx = pdf_w / orig_W
        sy = pdf_h / orig_H

        def img_to_pdf(ix0, iy0, ix1, iy1):
            """Convert image-pixel bbox to PDF-point bbox (Y flipped)."""
            return (
                ix0 * sx,
                pdf_h - iy1 * sy,
                ix1 * sx,
                pdf_h - iy0 * sy,
            )

        # ── Collect and deduplicate tokens from all angles ────────────────────
        raw_tokens = []
        for angle in ANGLES:
            toks = extract_tokens_at_angle(base_img, angle, orig_W, orig_H)
            raw_tokens.extend(toks)
            print(f" Page {page_idx+1} | {angle:3d}° → {len(toks):4d} tokens")

        # Deduplicate: same text within 8px of each other → keep highest conf
        seen = {}
        for t in raw_tokens:
            key = (t['text'].lower(),
                   round(t['x0'] / 8) * 8,
                   round(t['y0'] / 8) * 8)
            if key not in seen or t['conf'] > seen[key]['conf']:
                seen[key] = t
        tokens = list(seen.values())
        # Sort by reading order (top→bottom, left→right) for stream coherence
        tokens.sort(key=lambda t: (round(t['y0'] / 10) * 10, t['x0']))

        print(f" Page {page_idx+1} | unique tokens after dedup: {len(tokens)}\n")

        stream, c2t = build_stream(tokens)

        page_annots = []
        matched_spans = [] # list of (start, end) to avoid double-matching

        def overlaps_matched(s, e):
            for ms, me in matched_spans:
                if s < me and e > ms:
                    return True
            return False

        def register(annots_list, bb, color):
            """Convert img bbox → PDF bbox and append to annots list."""
            if bb:
                page_annots.append((*img_to_pdf(*bb), color))

        def log_result(label, inch_val, metric_val, ok, kind=""):
            exp = abs(inch_val) * 25.4
            sym = "✅" if ok else "❌ MISMATCH"
            tag = f" [{kind}]" if kind else ""
            print(f" p{page_idx+1} {sym}{tag} | "
                  f"{inch_val}\" → {exp:.3f} mm | stated [{metric_val:.3f}] mm")

        # ══════════════════════════════════════════════════════════════════════
        # 1. RADIUS dimensions
        # ══════════════════════════════════════════════════════════════════════
        for m in RADIUS_RE.finditer(stream):
            if overlaps_matched(m.start(), m.end()):
                continue
            try:
                inch_val = clean_abs(m.group(1))
                metric_val = clean_abs(m.group(2))
            except (ValueError, AttributeError):
                continue
            if inch_val == 0:
                continue

            ok = verify(inch_val, metric_val)
            color = YELLOW if ok else RED
            matched_spans.append((m.start(), m.end()))

            bb = bbox_for_span(m.start(), m.end(), tokens, c2t)
            register(page_annots, bb, color)

            log_result("R", inch_val, metric_val, ok, "RADIUS")
            stats['radius'] += 1
            if ok: stats['correct'] += 1
            else: stats['mismatch'] += 1

        # ══════════════════════════════════════════════════════════════════════
        # 2. DIAMETER dimensions
        # ══════════════════════════════════════════════════════════════════════
        for m in DIA_RE.finditer(stream):
            if overlaps_matched(m.start(), m.end()):
                continue
            try:
                inch_val = clean_abs(m.group(1))
                metric_val = clean_abs(m.group(2))
            except (ValueError, AttributeError):
                continue
            if inch_val == 0:
                continue

            ok = verify(inch_val, metric_val)
            color = YELLOW if ok else RED
            matched_spans.append((m.start(), m.end()))

            bb = bbox_for_span(m.start(), m.end(), tokens, c2t)
            register(page_annots, bb, color)

            log_result("Ø", inch_val, metric_val, ok, "DIAMETER")
            stats['diameter'] += 1
            if ok: stats['correct'] += 1
            else: stats['mismatch'] += 1

        # ══════════════════════════════════════════════════════════════════════
        # 3. STACKED TOLERANCES: +X [+Y] / -X [-Y]
        # ══════════════════════════════════════════════════════════════════════
        for m in TOL_STACK_RE.finditer(stream):
            if overlaps_matched(m.start(), m.end()):
                continue
            try:
                pos_inch = clean_abs(m.group(1))
                pos_met = clean_abs(m.group(2))
                neg_inch = clean_abs(m.group(3))
                neg_met = clean_abs(m.group(4))
            except (ValueError, AttributeError):
                continue

            ok_pos = verify(pos_inch, pos_met)
            ok_neg = verify(neg_inch, neg_met)
            ok = ok_pos and ok_neg
            color = YELLOW if ok else RED
            matched_spans.append((m.start(), m.end()))

            bb = bbox_for_span(m.start(), m.end(), tokens, c2t)
            register(page_annots, bb, color)

            sym = "✅" if ok else "❌ MISMATCH"
            print(f" p{page_idx+1} {sym} [TOL STACK] | "
                  f"+{pos_inch}/−{neg_inch}\" → [{pos_met}/{neg_met}] mm")
            if ok: stats['tol_correct'] += 1
            else: stats['tol_mismatch'] += 1

        # ══════════════════════════════════════════════════════════════════════
        # 4. SYMMETRIC TOLERANCES: ±X.XXX [±Y.YY]
        # ══════════════════════════════════════════════════════════════════════
        for m in TOL_SYM_RE.finditer(stream):
            if overlaps_matched(m.start(), m.end()):
                continue
            try:
                inch_val = clean_abs(m.group(1))
                metric_val = clean_abs(m.group(2))
            except (ValueError, AttributeError):
                continue
            if inch_val == 0:
                continue

            ok = verify(inch_val, metric_val)
            color = YELLOW if ok else RED
            matched_spans.append((m.start(), m.end()))

            bb = bbox_for_span(m.start(), m.end(), tokens, c2t)
            register(page_annots, bb, color)

            sym = "✅" if ok else "❌ MISMATCH"
            print(f" p{page_idx+1} {sym} [TOL ±] | ±{inch_val}\" → [±{metric_val}] mm")
            if ok: stats['tol_correct'] += 1
            else: stats['tol_mismatch'] += 1

        # ══════════════════════════════════════════════════════════════════════
        # 5. STANDARD LINEAR DIMENSIONS
        # ══════════════════════════════════════════════════════════════════════
        for m in LINEAR_RE.finditer(stream):
            if overlaps_matched(m.start(), m.end()):
                continue
            try:
                inch_val = clean_num(m.group(1))
                metric_val = clean_num(m.group(2))
            except (ValueError, AttributeError):
                continue
            if abs(inch_val) < 1e-9:
                continue

            ok = verify(inch_val, metric_val)
            color = YELLOW if ok else RED
            matched_spans.append((m.start(), m.end()))

            # Highlight the FULL match (inch value + brackets + metric value)
            # as one unified highlight so it's clear what was checked
            bb_full = bbox_for_span(m.start(), m.end(), tokens, c2t)
            register(page_annots, bb_full, color)

            log_result("", inch_val, metric_val, ok)
            if ok: stats['correct'] += 1
            else: stats['mismatch'] += 1

        all_page_annots.append(page_annots)

    # ══════════════════════════════════════════════════════════════════════════
    # WRITE ANNOTATED PDF
    # ══════════════════════════════════════════════════════════════════════════
    reader = PdfReader(input_path)
    writer = PdfWriter()
    for page in reader.pages:
        writer.add_page(page)

    total_highlights = 0
    for page_idx, annots in enumerate(all_page_annots):
        if not annots:
            continue
        page_obj = writer.pages[page_idx]
        annot_list = ArrayObject()
        for (x0, y0, x1, y1, color) in annots:
            annot_list.append(make_highlight(x0, y0, x1, y1, color))
        page_obj[NameObject("/Annots")] = annot_list
        total_highlights += len(annots)

    with open(output_path, "wb") as f:
        writer.write(f)

    total_dims = stats['correct'] + stats['mismatch']
    total_tols = stats['tol_correct'] + stats['tol_mismatch']

    print(f"""
╔══════════════════════════════════════════╗
║ SCAN COMPLETE v3                         ║
╠══════════════════════════════════════════╣
║ LINEAR DIMENSIONS                        ║
║ ✅ Correct : {stats['correct']:<20}      ║
║ ❌ Mismatches : {stats['mismatch']:<20}  ║
║ 📐 Radius found : {stats['radius']:<20}   ║
║ ⭕ Diameter found: {stats['diameter']:<20}║
╠══════════════════════════════════════════  ╣
║ TOLERANCES ║
║ ✅ Correct : {stats['tol_correct']:<20}║
║ ❌ Mismatches : {stats['tol_mismatch']:<20}║
╠══════════════════════════════════════════╣
║ 📌 Total highlights: {total_highlights:<20}║
╠══════════════════════════════════════════╣
║ 🟡 Yellow = Conversion OK ║
║ 🔴 Red = Conversion WRONG ║
╚══════════════════════════════════════════╝
Output → {output_path}
""")


# ══════════════════════════════════════════════════════════════════════════════
if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 highlight_dimensions_v3.py input.pdf output.pdf")
        sys.exit(1)
    process(sys.argv[1], sys.argv[2])