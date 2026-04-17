"""
AutoCAD Dimension Highlighter v6
Usage: python3 highlight_dimensions_v6.py input.pdf output.pdf

Fixes over v5:
  - Yellow highlights now appear (normalization no longer corrupts c2t positions)
  - Vertical (90°/270°) dimensions detected via token merging before stream build
  - Diameter symbol: catches OCR misreads O / 0 / Φ with negative lookbehind
  - verify() uses relative + absolute tolerance → small dims no longer all-red
  - Annotation opacity raised to 0.75 for better visibility
"""
coding: utf-8
import sys, re
from PIL import Image, ImageEnhance, ImageFilter
import pytesseract
from pdf2image import convert_from_path
from pypdf import PdfReader, PdfWriter
from pypdf.generic import ArrayObject, FloatObject, NameObject, NumberObject, DictionaryObject
import pdfplumber

DPI = 400
TOLERANCE = 0.030 # 3.0 % relative tolerance
ABS_TOL = 0.08 # 0.08 mm absolute fallback (handles tiny dims)
YELLOW = (1.0, 0.85, 0.0)
RED = (1.0, 0.10, 0.10)

# ─── PATTERNS ────────────────────────────────────────────────────────────────
_N = r'[+-]?\d*\.?\d+'

# Diameter: real Ø chars + OCR misreads (O, 0, Φ).
# Negative lookbehind (?<![A-Za-z\d]) prevents matching mid-word "O"
# e.g. "BORE" won't trigger, but "O1.234 [31.34]" will.
DIAMETER = re.compile(
    rf'(?<![A-Za-z\d])[φøØ⌀ϕΦOo0]\s*({_N})\s{{0,3}}\[({_N})\]'
)

RADIUS = re.compile(rf'R\s*({_N})\s{{0,3}}\[({_N})\]', re.IGNORECASE)
TOL_SYM = re.compile(rf'[±]\s*({_N})\s{{0,3}}\[[±]?\s*({_N})\]')
TOL_COMPOUND = re.compile(rf'({_N})[±]\s*({_N})\s{{0,3}}\[\s*({_N})[±]\s*({_N})\s*\]')
TOL_STACK = re.compile(
    rf'[+]({_N})\s{{0,3}}\[[+]?({_N})\]\s*'
    rf'[-]({_N})\s{{0,3}}\[[-]?({_N})\]'
)
# Linear last — most general, must not shadow the others
LINEAR = re.compile(rf'({_N})\s{{0,3}}\[({_N})\]')


# ─── VERIFY ──────────────────────────────────────────────────────────────────
def verify(inch, mm):
    if abs(inch) < 1e-9:
        return abs(mm) < 0.1
    expected = abs(inch) * 25.4
    diff = abs(expected - abs(mm))
    return (diff / expected <= TOLERANCE) or (diff <= ABS_TOL)


# ─── TOKEN-LEVEL NORMALISATION ───────────────────────────────────────────────
# Applied to each token's text BEFORE building the stream so that c2t
# character positions remain consistent with the normalized text.
_NORM_MAP = [
    ('+/-', '±'), ('+f-', '±'), ('t/-', '±'), # ± OCR variants
    ('φ', 'Ø'), ('ϕ', 'Ø'), ('⌀', 'Ø'), # diameter Unicode variants
    ('Φ', 'Ø'),
]

def normalize_token(text):
    for src, dst in _NORM_MAP:
        text = text.replace(src, dst)
    return text


# ─── IMAGE PREPROCESSING ─────────────────────────────────────────────────────
def preprocess(img):
    img = img.convert("L")
    img = ImageEnhance.Contrast(img).enhance(2.0)
    img = ImageEnhance.Sharpness(img).enhance(2.0)
    img = img.filter(ImageFilter.SHARPEN)
    return img.convert("RGB")


# ─── COORDINATE UNROTATION ───────────────────────────────────────────────────
def unrotate(rx0, ry0, rx1, ry1, angle, W, H):
    if angle == 0:
        return rx0, ry0, rx1, ry1
    if angle == 90: # PIL CCW → inverse maps (rx,ry)→(W-ry, rx)
        return max(0, W - ry1), max(0, rx0), min(W, W - ry0), min(H, rx1)
    if angle == 270: # PIL CW → inverse maps (rx,ry)→(ry, H-rx)
        return max(0, ry0), max(0, H - rx1), min(W, ry1), min(H, H - rx0)
    return rx0, ry0, rx1, ry1


# ─── OCR ─────────────────────────────────────────────────────────────────────
def ocr_tokens(base_img, angle, orig_W, orig_H):
    rot = base_img.rotate(angle, expand=True) if angle else base_img
    data = pytesseract.image_to_data(
        preprocess(rot),
        config="--oem 3 --psm 11",
        output_type=pytesseract.Output.DICT,
    )
    tokens = []
    for i, txt in enumerate(data['text']):
        txt = txt.strip()
        if not txt:
            continue
        try:
            conf = float(data['conf'][i])
        except (ValueError, TypeError):
            conf = 0
        if conf < 15:
            continue
        rx0 = data['left'][i]
        ry0 = data['top'][i]
        rx1 = rx0 + max(data['width'][i], 1)
        ry1 = ry0 + max(data['height'][i], 1)
        bx0, by0, bx1, by1 = unrotate(rx0, ry0, rx1, ry1, angle, orig_W, orig_H)
        tokens.append({
            'text': normalize_token(txt), # ← normalise here, not on stream
            'x0': bx0, 'y0': by0, 'x1': bx1, 'y1': by1,
            'conf': conf,
        })
    return tokens


# ─── TOKEN MERGING FOR ROTATED TEXT ──────────────────────────────────────────
# For 90°/270° text, OCR often splits a single dimension string across several
# tokens (e.g. "1.234" + "[31.34]" as separate tokens with a small gap).
# Merging them ensures build_stream assembles a matchable pattern.

def merge_inline_tokens(tokens, axis='x', gap=28):
    """
    Merge tokens that lie on the same reading line within `gap` pixels.
    axis='x' → merge tokens close in x (horizontal reading lines, used for
               rotated 90°/270° text after coordinate unrotation).
    axis='y' → merge tokens close in y (normal horizontal text — not used).
    """
    if not tokens:
        return tokens

    if axis == 'x':
        sort_key = lambda t: t['x0']
        group_key = lambda t: round(t['y0'] / 10) * 10 # same horizontal band
        end_key = lambda t: t['x1']
        next_key = lambda t: t['x0']
    else:
        sort_key = lambda t: t['y0']
        group_key = lambda t: round(t['x0'] / 10) * 10
        end_key = lambda t: t['y1']
        next_key = lambda t: t['y0']

    # Group by row/column band
    from collections import defaultdict
    bands = defaultdict(list)
    for tok in tokens:
        bands[group_key(tok)].append(tok)

    merged = []
    for band_toks in bands.values():
        band_toks.sort(key=sort_key)
        cur = dict(band_toks[0])
        for tok in band_toks[1:]:
            if next_key(tok) - end_key(cur) <= gap:
                # Merge: join text with no separator (OCR already has spaces
                # inside compound tokens), expand bbox, take min confidence
                cur['text'] += tok['text']
                cur['x0'] = min(cur['x0'], tok['x0'])
                cur['y0'] = min(cur['y0'], tok['y0'])
                cur['x1'] = max(cur['x1'], tok['x1'])
                cur['y1'] = max(cur['y1'], tok['y1'])
                cur['conf'] = min(cur['conf'], tok['conf'])
            else:
                merged.append(cur)
                cur = dict(tok)
        merged.append(cur)

    return merged


# ─── STREAM + SPAN→BBOX ──────────────────────────────────────────────────────
def build_stream(tokens):
    stream, c2t = "", {}
    for ti, tok in enumerate(tokens):
        s = len(stream)
        for ci in range(len(tok['text'])):
            c2t[s + ci] = ti
        stream += tok['text']
        c2t[len(stream)] = ti
        stream += " "
    return stream, c2t


def span_bbox(s, e, tokens, c2t):
    tis = {c2t[ci] for ci in range(s, min(e, len(c2t))) if ci in c2t}
    if not tis:
        return None
    x0 = min(tokens[i]['x0'] for i in tis)
    y0 = min(tokens[i]['y0'] for i in tis)
    x1 = max(tokens[i]['x1'] for i in tis)
    y1 = max(tokens[i]['y1'] for i in tis)
    med_h = sorted(tokens[i]['y1'] - tokens[i]['y0'] for i in tis)[len(tis) // 2]
    if (y1 - y0) > max(med_h * 3.5, 50):
        return None
    return x0, y0, x1, y1


# ─── FIND DIMENSIONS IN STREAM ───────────────────────────────────────────────
def find_dims(stream, c2t, tokens):
    results, used = [], []

    def overlaps(s, e):
        return any(s < ue and e > us for us, ue in used)

    def add(pattern, kind, parse):
        for m in pattern.finditer(stream):
            if overlaps(m.start(), m.end()):
                continue
            parsed = parse(m)
            if parsed is None:
                continue
            inch, mm, (ms, me) = parsed
            if abs(inch) < 1e-9:
                continue
            bb = span_bbox(ms, me, tokens, c2t)
            if bb is None:
                continue
            used.append((m.start(), m.end()))
            results.append({
                'inch': inch, 'mm': mm,
                'ok': verify(inch, mm),
                'bb': bb, 'kind': kind,
            })

    def p2(m):
        try: return float(m.group(1)), float(m.group(2)), (m.start(), m.end())
        except: return None

    # Most-specific first so LINEAR doesn't consume them
    add(DIAMETER, "DIA", p2)
    add(RADIUS, "RAD", p2)
    add(TOL_SYM, "TOL±", p2)

    for m in TOL_COMPOUND.finditer(stream):
        if overlaps(m.start(), m.end()): continue
        try:
            nom_i, tol_i = float(m.group(1)), float(m.group(2))
            nom_m, tol_m = float(m.group(3)), float(m.group(4))
        except: continue
        bb = span_bbox(m.start(), m.end(), tokens, c2t)
        if bb is None: continue
        ok = verify(nom_i, nom_m) and verify(tol_i, tol_m)
        used.append((m.start(), m.end()))
        results.append({'inch': nom_i, 'mm': nom_m, 'ok': ok, 'bb': bb, 'kind': 'TOL±±'})

    add(LINEAR, "LINEAR", p2)

    for m in TOL_STACK.finditer(stream):
        if overlaps(m.start(), m.end()): continue
        try:
            pi_, pm = float(m.group(1)), float(m.group(2))
            ni, nm = float(m.group(3)), float(m.group(4))
        except: continue
        bb = span_bbox(m.start(), m.end(), tokens, c2t)
        if bb is None: continue
        ok = verify(pi_, pm) and verify(ni, nm)
        used.append((m.start(), m.end()))
        results.append({'inch': pi_, 'mm': pm, 'ok': ok, 'bb': bb, 'kind': 'TOL+/-'})

    return results


# ─── PDF ANNOTATION ──────────────────────────────────────────────────────────
def make_annot(x0, y0, x1, y1, color):
    x0 -= 2; y0 -= 2; x1 += 2; y1 += 2
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
        NameObject("/CA"): FloatObject(0.75), # raised from 0.55
        NameObject("/F"): NumberObject(4),
    })


# ─── MAIN ────────────────────────────────────────────────────────────────────
def process(inp, out):
    print(f"\n📄 {inp} DPI={DPI} TOL={TOLERANCE*100:.1f}% ABS={ABS_TOL}mm\n")

    with pdfplumber.open(inp) as pdf:
        pdf_dims = [(p.width, p.height) for p in pdf.pages]

    pages = convert_from_path(inp, dpi=DPI)
    all_annots = []
    stats = {'ok': 0, 'bad': 0}

    for pi, (img, (pw, ph)) in enumerate(zip(pages, pdf_dims)):
        W, H = img.size
        sx, sy = pw / W, ph / H

        def to_pdf(x0, y0, x1, y1):
            return x0 * sx, ph - y1 * sy, x1 * sx, ph - y0 * sy

        all_dims, seen_vals = [], set()

        for angle in [0, 90, 270]:
            tokens = ocr_tokens(img, angle, W, H)

            if angle == 0:
                tokens.sort(key=lambda t: (round(t['y0'] / 8) * 8, t['x0']))
                # No merging for horizontal text — individual tokens work fine

            elif angle == 90:
                # After unrotation, this text reads left-to-right along x axis
                tokens.sort(key=lambda t: (round(t['x0'] / 8) * 8, -t['y1']))
                tokens = merge_inline_tokens(tokens, axis='x', gap=28)

            else: # 270
                tokens.sort(key=lambda t: (round(t['x1'] / 8) * 8, t['y0']))
                tokens = merge_inline_tokens(tokens, axis='x', gap=28)

            stream, c2t = build_stream(tokens)
            dims = find_dims(stream, c2t, tokens)

            for d in dims:
                key = (
                    round(d['inch'], 3),
                    round(d['bb'][0] / 20) * 20,
                    round(d['bb'][1] / 20) * 20,
                )
                if key in seen_vals:
                    continue
                seen_vals.add(key)
                all_dims.append(d)

            print(f" p{pi+1} {angle:3d}° → {len(tokens):4d} tokens, {len(dims)} dims")

        page_annots = []
        for d in all_dims:
            color = YELLOW if d['ok'] else RED
            page_annots.append((*to_pdf(*d['bb']), color))
            sym = "✅" if d['ok'] else "❌"
            exp = abs(d['inch']) * 25.4
            print(
                f" p{pi+1} {sym} [{d['kind']:6}] "
                f"{d['inch']:.4f}\" → {exp:.3f} mm | stated {d['mm']:.3f} mm"
            )
            if d['ok']: stats['ok'] += 1
            else: stats['bad'] += 1

        all_annots.append(page_annots)

    reader = PdfReader(inp)
    writer = PdfWriter()
    for page in reader.pages:
        writer.add_page(page)

    total = 0
    for pi, annots in enumerate(all_annots):
        if not annots:
            continue
        al = ArrayObject()
        for x0, y0, x1, y1, color in annots:
            al.append(make_annot(x0, y0, x1, y1, color))
        writer.pages[pi][NameObject("/Annots")] = al
        total += len(annots)

    with open(out, "wb") as f:
        writer.write(f)

    print(f"""
╔══════════════════════════════════════╗
║ ✅ Correct : {stats['ok']:<22}║
║ ❌ Mismatch : {stats['bad']:<22}║
║ 📌 Highlights: {total:<22}║
║ 🟡 Yellow = OK 🔴 Red = WRONG ║
╚══════════════════════════════════════╝
→ {out}""")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 highlight_dimensions_v6.py input.pdf output.pdf")
        sys.exit(1)
    process(sys.argv[1], sys.argv[2])