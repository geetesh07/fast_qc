"""
AutoCAD Engineering Drawing — Dimension Highlighter v4
======================================================
Key fix vs v3: each OCR angle is processed INDEPENDENTLY.
Matches found at 0° use only 0° token bboxes → tight horizontal highlights.
Matches found at 90° use only 90° token bboxes → tight vertical highlights.
No cross-angle bbox unions → no giant red bands.

Color logic:
  🟡 Yellow = dimension checked and conversion is CORRECT
  🔴 Red    = dimension checked and conversion is WRONG

Detects:
  • Linear pairs:         1.234 [31.34]
  • Radius:               R1.250 [31.75]  |  RAD 1.250 [31.75]
  • Diameter:             Ø1.250 [31.75]  |  DIA 1.250 [31.75]  |  ⌀
  • Stacked tolerances:   +.005 [+.13]  (next line)  -.000 [-.00]
  • Symmetric tolerances: ±0.005 [±0.13]  |  +/-0.005 [+/-0.13]

Usage:
  python3 highlight_dimensions_v4.py input.pdf output_highlighted.pdf
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
DPI           = 400
TOLERANCE_PCT = 0.025   # 2.5% — covers drawing rounding
ANGLES        = [0, 90, 270]
HIGHLIGHT_PAD = 1       # points padding around highlight

# ── Pattern building blocks ───────────────────────────────────────────────────
OB  = r'[\[({]'                        # opening bracket variant
CB  = r'[\])}]'                        # closing bracket variant
NUM = r'[+-]?\s*\d*\.?\d[\d,\s]*'     # signed number, allows OCR spaces

# ── Compiled patterns ─────────────────────────────────────────────────────────
LINEAR_RE = re.compile(
    rf'({NUM})\s*{OB}\s*({NUM})\s*{CB}', re.I)

RADIUS_RE = re.compile(
    rf'(?:R|RAD\.?)\s*({NUM})\s*{OB}\s*({NUM})\s*{CB}', re.I)

DIA_RE = re.compile(
    rf'(?:[ØO⌀]|DIA\.?|DIAM\.?)\s*({NUM})\s*{OB}\s*({NUM})\s*{CB}', re.I)

TOL_STACK_RE = re.compile(
    rf'([+]\s*\d*\.?\d[\d,]*)\s*{OB}\s*([+]?\s*\d*\.?\d[\d,]*)\s*{CB}'
    rf'\s*'
    rf'([-]\s*\d*\.?\d[\d,]*)\s*{OB}\s*([+\-]?\s*\d*\.?\d[\d,]*)\s*{CB}', re.I)

TOL_SYM_RE = re.compile(
    rf'(?:[±]|[+][/\-])\s*(\d*\.?\d[\d,]*)\s*{OB}\s*(?:[±]|[+][/\-])?\s*(\d*\.?\d[\d,]*)\s*{CB}', re.I)


# ── Helpers ───────────────────────────────────────────────────────────────────
def clean_num(s):
    return float(re.sub(r'[\s,]', '', s))

def clean_abs(s):
    return abs(clean_num(re.sub(r'[^0-9.,\s]', '', s)))

def verify(inch_val, metric_val):
    if abs(inch_val) < 1e-9:
        return abs(metric_val) < 0.1
    expected = abs(inch_val) * 25.4
    return abs(expected - abs(metric_val)) / expected <= TOLERANCE_PCT


# ── Image preprocessing ───────────────────────────────────────────────────────
def preprocess(img):
    img = img.convert("L")
    img = ImageEnhance.Contrast(img).enhance(2.2)
    img = ImageEnhance.Sharpness(img).enhance(2.5)
    img = img.filter(ImageFilter.SHARPEN)
    return img.convert("RGB")


# ── Coordinate unrotation ─────────────────────────────────────────────────────
def unrotate_bbox(rx0, ry0, rx1, ry1, angle, orig_W, orig_H):
    """
    Map a bbox from a rotated image back to original image pixel space.
    angle: degrees CCW the original was rotated to produce the OCR image.
    """
    if angle == 0:
        return rx0, ry0, rx1, ry1

    elif angle == 90:
        # rotate(90) in PIL = 90° CCW
        # rotated image shape: W_rot = orig_H, H_rot = orig_W
        # forward:  (ox, oy) → (oy,  orig_W - ox)    [rotated coords]
        # inverse:  (rx, ry) → (orig_W - ry, rx)
        ox0 = orig_W - ry1
        oy0 = rx0
        ox1 = orig_W - ry0
        oy1 = rx1
        return max(0, ox0), max(0, oy0), min(orig_W, ox1), min(orig_H, oy1)

    elif angle == 270:
        # rotate(270) in PIL = 90° CW
        # rotated image shape: W_rot = orig_H, H_rot = orig_W
        # forward:  (ox, oy) → (orig_H - oy, ox)    [rotated coords]
        # inverse:  (rx, ry) → (ry, orig_H - rx)
        ox0 = ry0
        oy0 = orig_H - rx1
        ox1 = ry1
        oy1 = orig_H - rx0
        return max(0, ox0), max(0, oy0), min(orig_W, ox1), min(orig_H, oy1)

    return rx0, ry0, rx1, ry1


# ── PDF annotation ────────────────────────────────────────────────────────────
YELLOW = (1.0, 0.90, 0.0)
RED    = (1.0, 0.10, 0.10)

def make_highlight(x0, y0, x1, y1, color):
    p = HIGHLIGHT_PAD
    x0 -= p; y0 -= p; x1 += p; y1 += p
    return DictionaryObject({
        NameObject("/Type"):    NameObject("/Annot"),
        NameObject("/Subtype"): NameObject("/Highlight"),
        NameObject("/Rect"):    ArrayObject([FloatObject(v) for v in [x0, y0, x1, y1]]),
        NameObject("/QuadPoints"): ArrayObject([
            FloatObject(x0), FloatObject(y1),
            FloatObject(x1), FloatObject(y1),
            FloatObject(x0), FloatObject(y0),
            FloatObject(x1), FloatObject(y0),
        ]),
        NameObject("/C"):  ArrayObject([FloatObject(c) for c in color]),
        NameObject("/CA"): FloatObject(0.55),
        NameObject("/F"):  NumberObject(4),
    })


# ── Per-angle OCR + stream builder ───────────────────────────────────────────
def run_ocr_angle(base_img, angle, orig_W, orig_H):
    """
    Rotate base_img, run OCR, unrotate all bboxes back to original pixel space.
    Returns list of token dicts with 'text', 'x0','y0','x1','y1', 'conf'.
    """
    rot = base_img.rotate(angle, expand=True) if angle != 0 else base_img
    proc = preprocess(rot)
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
        if conf < 15:
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
        })
    return tokens


def build_stream(tokens):
    """Return (stream_str, char→token_index dict)."""
    stream = ""
    c2t = {}
    for ti, tok in enumerate(tokens):
        s = len(stream)
        for ci in range(len(tok['text'])):
            c2t[s + ci] = ti
        stream += tok['text']
        c2t[len(stream)] = ti
        stream += " "
    return stream, c2t


def bbox_for_span(s, e, tokens, c2t):
    """
    Tight union bbox of tokens whose chars overlap [s, e).
    Returns None if no tokens found.
    Rejects absurdly large spans (likely cross-line OCR merge artefacts).
    """
    tis = set()
    for ci in range(s, min(e, len(c2t))):
        if ci in c2t:
            tis.add(c2t[ci])
    if not tis:
        return None

    x0 = min(tokens[i]['x0'] for i in tis)
    y0 = min(tokens[i]['y0'] for i in tis)
    x1 = max(tokens[i]['x1'] for i in tis)
    y1 = max(tokens[i]['y1'] for i in tis)

    # Reject if the resulting box is suspiciously tall or wide
    # (indicates cross-line token merge; height cap = 3 × median token height)
    heights = [tokens[i]['y1'] - tokens[i]['y0'] for i in tis]
    med_h = sorted(heights)[len(heights) // 2]
    if (y1 - y0) > max(med_h * 4, 60):   # allow up to 4 token-heights
        return None

    return x0, y0, x1, y1


# ── Per-angle dimension finder ────────────────────────────────────────────────
Match = dict   # {inch_val, metric_val, ok, bb_orig_px, kind, label}

def find_matches_in_stream(stream, c2t, tokens, angle_label):
    """
    Run all patterns against the token stream for one OCR angle.
    Returns list of Match dicts.
    """
    matches = []
    used_spans = []

    def overlaps(s, e):
        for us, ue in used_spans:
            if s < ue and e > us:
                return True
        return False

    def record(pattern, stream, kind, parse_fn):
        for m in pattern.finditer(stream):
            if overlaps(m.start(), m.end()):
                continue
            result = parse_fn(m)
            if result is None:
                continue
            inch_val, metric_val, full_span = result
            if abs(inch_val) < 1e-9:
                continue
            bb = bbox_for_span(full_span[0], full_span[1], tokens, c2t)
            if bb is None:
                continue
            ok = verify(inch_val, metric_val)
            used_spans.append((m.start(), m.end()))
            matches.append({
                'inch_val':   inch_val,
                'metric_val': metric_val,
                'ok':         ok,
                'bb':         bb,
                'kind':       kind,
                'angle':      angle_label,
            })

    # Radius
    def parse_radius(m):
        try:
            return clean_abs(m.group(1)), clean_abs(m.group(2)), (m.start(), m.end())
        except (ValueError, AttributeError):
            return None
    record(RADIUS_RE, stream, "RADIUS", parse_radius)

    # Diameter
    def parse_dia(m):
        try:
            return clean_abs(m.group(1)), clean_abs(m.group(2)), (m.start(), m.end())
        except (ValueError, AttributeError):
            return None
    record(DIA_RE, stream, "DIAMETER", parse_dia)

    # Symmetric tolerance
    def parse_tol_sym(m):
        try:
            return clean_abs(m.group(1)), clean_abs(m.group(2)), (m.start(), m.end())
        except (ValueError, AttributeError):
            return None
    record(TOL_SYM_RE, stream, "TOL±", parse_tol_sym)

    # Stacked tolerance — two pairs; highlight full span
    for m in TOL_STACK_RE.finditer(stream):
        if overlaps(m.start(), m.end()):
            continue
        try:
            pos_i = clean_abs(m.group(1)); pos_m = clean_abs(m.group(2))
            neg_i = clean_abs(m.group(3)); neg_m = clean_abs(m.group(4))
        except (ValueError, AttributeError):
            continue
        bb = bbox_for_span(m.start(), m.end(), tokens, c2t)
        if bb is None:
            continue
        ok = verify(pos_i, pos_m) and verify(neg_i, neg_m)
        used_spans.append((m.start(), m.end()))
        matches.append({
            'inch_val':   pos_i,
            'metric_val': pos_m,
            'ok':         ok,
            'bb':         bb,
            'kind':       "TOL+/-",
            'angle':      angle_label,
        })

    # Linear (catch-all last)
    def parse_linear(m):
        try:
            return clean_num(m.group(1)), clean_num(m.group(2)), (m.start(), m.end())
        except (ValueError, AttributeError):
            return None
    record(LINEAR_RE, stream, "LINEAR", parse_linear)

    return matches


# ── Deduplicate matches across angles ─────────────────────────────────────────
def deduplicate(all_matches, orig_W, orig_H):
    """
    If two matches from different angles have nearly identical inch values
    and overlapping bboxes, keep the one whose bbox aspect ratio best fits
    its angle (horizontal for 0°, vertical for 90°/270°).
    """
    kept = []
    for m in all_matches:
        duplicate = False
        bx0, by0, bx1, by1 = m['bb']
        for k in kept:
            kx0, ky0, kx1, ky1 = k['bb']
            # Check value similarity
            if abs(m['inch_val'] - k['inch_val']) > 0.01:
                continue
            # Check bbox overlap
            overlap_x = max(0, min(bx1, kx1) - max(bx0, kx0))
            overlap_y = max(0, min(by1, ky1) - max(by0, ky0))
            if overlap_x > 0 and overlap_y > 0:
                duplicate = True
                break
        if not duplicate:
            kept.append(m)
    return kept


# ── Main ──────────────────────────────────────────────────────────────────────
def process(input_path, output_path):
    print(f"\n📄  {input_path}")
    print(f"    DPI={DPI}  Angles={ANGLES}  Tolerance={TOLERANCE_PCT*100:.1f}%\n")

    with pdfplumber.open(input_path) as pdf:
        pdf_dims = [(p.width, p.height) for p in pdf.pages]

    images = convert_from_path(input_path, dpi=DPI)
    print(f"    Pages: {len(images)}\n")

    all_page_annots = []
    stats = {'correct': 0, 'mismatch': 0}

    for page_idx, (base_img, (pdf_w, pdf_h)) in enumerate(zip(images, pdf_dims)):
        orig_W, orig_H = base_img.size
        sx = pdf_w / orig_W
        sy = pdf_h / orig_H

        def img_to_pdf(ix0, iy0, ix1, iy1):
            return (
                ix0 * sx,
                pdf_h - iy1 * sy,
                ix1 * sx,
                pdf_h - iy0 * sy,
            )

        # ── Run OCR and find matches for each angle INDEPENDENTLY ─────────────
        all_matches = []

        for angle in ANGLES:
            tokens = run_ocr_angle(base_img, angle, orig_W, orig_H)
            # Sort tokens in reading order for this angle
            if angle == 0:
                tokens.sort(key=lambda t: (round(t['y0'] / 8) * 8, t['x0']))
            elif angle == 90:
                # After unrotation, vertical text: sort by x then y
                tokens.sort(key=lambda t: (round(t['x0'] / 8) * 8, t['y0']))
            else:
                tokens.sort(key=lambda t: (round(t['x1'] / 8) * 8, -t['y0']))

            stream, c2t = build_stream(tokens)
            matches = find_matches_in_stream(stream, c2t, tokens, angle)
            all_matches.extend(matches)
            print(f"    Page {page_idx+1} | {angle:3d}° → {len(tokens):4d} tokens, "
                  f"{len(matches)} dimensions found")

        # ── Deduplicate across angles ─────────────────────────────────────────
        unique_matches = deduplicate(all_matches, orig_W, orig_H)
        print(f"    Page {page_idx+1} | unique dimensions after dedup: {len(unique_matches)}\n")

        page_annots = []
        for m in unique_matches:
            color = YELLOW if m['ok'] else RED
            bb_pdf = img_to_pdf(*m['bb'])
            page_annots.append((*bb_pdf, color))

            exp = abs(m['inch_val']) * 25.4
            sym = "✅" if m['ok'] else "❌"
            print(f"    p{page_idx+1} {sym} [{m['kind']:8s}] @{m['angle']:3d}° | "
                  f"{m['inch_val']:.4f}\" → {exp:.3f} mm | stated [{m['metric_val']:.3f}] mm")
            if m['ok']: stats['correct'] += 1
            else:       stats['mismatch'] += 1

        all_page_annots.append(page_annots)

    # ── Write annotated PDF ───────────────────────────────────────────────────
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

    print(f"""
╔══════════════════════════════════════════╗
║        SCAN COMPLETE  v4                 ║
╠══════════════════════════════════════════╣
║  ✅ Correct conversions : {stats['correct']:<16}║
║  ❌ Mismatches          : {stats['mismatch']:<16}║
║  📌 Total highlights    : {total_highlights:<16}║
╠══════════════════════════════════════════╣
║  🟡 Yellow = Conversion OK               ║
║  🔴 Red    = Conversion WRONG            ║
╚══════════════════════════════════════════╝
Output → {output_path}
""")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 highlight_dimensions_v4.py input.pdf output.pdf")
        sys.exit(1)
    process(sys.argv[1], sys.argv[2])
