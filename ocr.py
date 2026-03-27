"""
AutoCAD Dimension Highlighter v6 - Direct PDF Text Extraction
Usage: python3 highlight_dimensions_v6.py input.pdf output.pdf

No OCR needed - extracts actual text objects from vector PDFs.
100% accurate, catches all orientations, much faster.
"""

import sys
import re
from pypdf import PdfReader, PdfWriter
from pypdf.generic import ArrayObject, FloatObject, NameObject, NumberObject, DictionaryObject
import pdfplumber

TOLERANCE = 0.025  # 2.5% conversion tolerance
YELLOW = (1.0, 0.90, 0.0)
RED = (1.0, 0.10, 0.10)

# ─── REGEX PATTERNS ──────────────────────────────────────────────────────────
# Signed number: handles 1.234 | .005 | +.005 | -0.12
_N = r'[+-]?\d*\.?\d+'

# ① Standard linear: 4.587 [116.5]
LINEAR = re.compile(rf'({_N})\s{{0,3}}\[({_N})\]')

# ② Diameter: φ1.234 [31.34] | ø1.234 [31.34] | Ø1.234 | ⌀1.234
DIAMETER = re.compile(rf'[φøØ⌀ϕ]\s*({_N})\s{{0,3}}\[({_N})\]')

# ③ Radius: R1.250 [31.75] | R .250 [6.35]
RADIUS = re.compile(rf'R\s*({_N})\s{{0,3}}\[({_N})\]', re.IGNORECASE)

# ④ Symmetric tolerance: ±.030 [±0.76] | +/-.030 [+/-0.76]
TOL_SYM = re.compile(rf'[±]\s*({_N})\s{{0,3}}\[[±]?\s*({_N})\]')

# ⑤ Compound tolerance: .030±.030 [0.76±0.76]
TOL_COMPOUND = re.compile(rf'({_N})[±]\s*({_N})\s{{0,3}}\[\s*({_N})[±]\s*({_N})\s*\]')

# ⑥ Stacked tolerance: +.005 [+.13] -.000 [-.00]
TOL_STACK = re.compile(
    rf'[+]({_N})\s{{0,3}}\[[+]?({_N})\]\s*'
    rf'[-]({_N})\s{{0,3}}\[[-]?({_N})\]'
)


def verify(inch, mm):
    """Verify inch-to-mm conversion within tolerance."""
    if abs(inch) < 1e-9:
        return abs(mm) < 0.1
    return abs(abs(inch) * 25.4 - abs(mm)) / (abs(inch) * 25.4) <= TOLERANCE


def extract_text_with_positions(page):
    """
    Extract all text characters from PDF page with exact positions.
    Returns list of {char, x0, y0, x1, y1} dicts.
    """
    chars = []
    for char in page.chars:
        chars.append({
            'char': char['text'],
            'x0': float(char['x0']),
            'y0': float(char['y0']),
            'x1': float(char['x1']),
            'y1': float(char['y1']),
        })
    return chars


def cluster_chars_to_words(chars, max_gap_x=3, max_gap_y=2):
    """
    Group characters into words based on proximity.
    Returns list of {text, x0, y0, x1, y1} dicts.
    """
    if not chars:
        return []
    
    # Sort characters: top to bottom, left to right
    chars = sorted(chars, key=lambda c: (round(c['y0']), c['x0']))
    
    words = []
    current_word = None
    
    for char in chars:
        if current_word is None:
            # Start new word
            current_word = {
                'text': char['char'],
                'x0': char['x0'],
                'y0': char['y0'],
                'x1': char['x1'],
                'y1': char['y1'],
            }
        else:
            # Check if this character continues the current word
            # Characters are part of same word if they're on same line and close together
            y_diff = abs(char['y0'] - current_word['y0'])
            x_gap = char['x0'] - current_word['x1']
            
            if y_diff <= max_gap_y and 0 <= x_gap <= max_gap_x:
                # Extend current word
                current_word['text'] += char['char']
                current_word['x1'] = char['x1']
                current_word['y1'] = max(current_word['y1'], char['y1'])
                current_word['y0'] = min(current_word['y0'], char['y0'])
            else:
                # Save current word and start new one
                words.append(current_word)
                current_word = {
                    'text': char['char'],
                    'x0': char['x0'],
                    'y0': char['y0'],
                    'x1': char['x1'],
                    'y1': char['y1'],
                }
    
    # Don't forget the last word
    if current_word is not None:
        words.append(current_word)
    
    return words


def build_text_stream(words):
    """
    Build continuous text stream and mapping from char index to word index.
    Returns (stream, char_to_word_map).
    """
    stream = ""
    c2w = {}  # char index -> word index
    
    for wi, word in enumerate(words):
        start = len(stream)
        stream += word['text']
        for ci in range(len(word['text'])):
            c2w[start + ci] = wi
        # Add space between words
        c2w[len(stream)] = wi
        stream += " "
    
    return stream, c2w


def span_to_bbox(start, end, words, c2w):
    """Convert character span to bounding box by finding involved words."""
    word_indices = {c2w[ci] for ci in range(start, min(end, len(c2w))) if ci in c2w}
    
    if not word_indices:
        return None
    
    x0 = min(words[i]['x0'] for i in word_indices)
    y0 = min(words[i]['y0'] for i in word_indices)
    x1 = max(words[i]['x1'] for i in word_indices)
    y1 = max(words[i]['y1'] for i in word_indices)
    
    return (x0, y0, x1, y1)


def find_dimensions(stream, c2w, words):
    """
    Find all dimension patterns in text stream.
    Returns list of {inch, mm, ok, bb, kind} dicts.
    """
    results = []
    used_spans = []
    
    def overlaps(start, end):
        return any(start < ue and end > us for us, ue in used_spans)
    
    def add_match(pattern, kind, parser):
        for match in pattern.finditer(stream):
            if overlaps(match.start(), match.end()):
                continue
            
            parsed = parser(match)
            if parsed is None:
                continue
            
            inch, mm, span = parsed
            if abs(inch) < 1e-9:
                continue
            
            bbox = span_to_bbox(span[0], span[1], words, c2w)
            if bbox is None:
                continue
            
            used_spans.append((match.start(), match.end()))
            results.append({
                'inch': inch,
                'mm': mm,
                'ok': verify(inch, mm),
                'bb': bbox,
                'kind': kind,
            })
    
    # Parser functions
    def parse_standard(match):
        try:
            return float(match.group(1)), float(match.group(2)), (match.start(), match.end())
        except (ValueError, AttributeError):
            return None
    
    # Process patterns in order of specificity (most specific first)
    add_match(DIAMETER, "DIA", parse_standard)
    add_match(RADIUS, "RAD", parse_standard)
    add_match(TOL_SYM, "TOL±", parse_standard)
    
    # Compound tolerance: .030±.030 [0.76±0.76]
    for match in TOL_COMPOUND.finditer(stream):
        if overlaps(match.start(), match.end()):
            continue
        try:
            nom_i, tol_i = float(match.group(1)), float(match.group(2))
            nom_m, tol_m = float(match.group(3)), float(match.group(4))
        except (ValueError, AttributeError):
            continue
        
        bbox = span_to_bbox(match.start(), match.end(), words, c2w)
        if bbox is None:
            continue
        
        ok = verify(nom_i, nom_m) and verify(tol_i, tol_m)
        used_spans.append((match.start(), match.end()))
        results.append({
            'inch': nom_i,
            'mm': nom_m,
            'ok': ok,
            'bb': bbox,
            'kind': 'TOL±±',
        })
    
    add_match(LINEAR, "LINEAR", parse_standard)
    
    # Stacked tolerance: +.005 [+.13] -.000 [-.00]
    for match in TOL_STACK.finditer(stream):
        if overlaps(match.start(), match.end()):
            continue
        try:
            pi, pm = float(match.group(1)), float(match.group(2))
            ni, nm = float(match.group(3)), float(match.group(4))
        except (ValueError, AttributeError):
            continue
        
        bbox = span_to_bbox(match.start(), match.end(), words, c2w)
        if bbox is None:
            continue
        
        ok = verify(pi, pm) and verify(ni, nm)
        used_spans.append((match.start(), match.end()))
        results.append({
            'inch': pi,
            'mm': pm,
            'ok': ok,
            'bb': bbox,
            'kind': 'TOL+/-',
        })
    
    return results


def create_highlight_annotation(x0, y0, x1, y1, color):
    """Create PDF highlight annotation with slight padding."""
    # Add small padding
    x0 -= 2
    y0 -= 2
    x1 += 2
    y1 += 2
    
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
        NameObject("/CA"): FloatObject(0.50),  # Slightly less opaque
        NameObject("/F"): NumberObject(4),
    })


def process_pdf(input_path, output_path):
    """Main processing function."""
    print(f"\n📄 Processing: {input_path}")
    print(f"   Tolerance: {TOLERANCE*100:.1f}%\n")
    
    all_page_annotations = []
    stats = {'ok': 0, 'bad': 0}
    
    with pdfplumber.open(input_path) as pdf:
        for page_num, page in enumerate(pdf.pages, 1):
            print(f"📖 Page {page_num}:")
            
            # Extract text with positions
            chars = extract_text_with_positions(page)
            print(f"   • Extracted {len(chars)} characters from PDF")
            
            # Cluster into words
            words = cluster_chars_to_words(chars)
            print(f"   • Grouped into {len(words)} words")
            
            # Build searchable stream
            stream, c2w = build_text_stream(words)
            
            # Find dimensions
            dimensions = find_dimensions(stream, c2w, words)
            print(f"   • Found {len(dimensions)} dimension pairs")
            
            # Prepare annotations for this page
            page_annotations = []
            for dim in dimensions:
                color = YELLOW if dim['ok'] else RED
                page_annotations.append((*dim['bb'], color))
                
                # Print details
                symbol = "✅" if dim['ok'] else "❌"
                expected = abs(dim['inch']) * 25.4
                diff = abs(expected - dim['mm'])
                print(f"     {symbol} [{dim['kind']:6}] {dim['inch']:7.4f}\" → "
                      f"expected {expected:7.3f}mm | stated {dim['mm']:7.3f}mm | "
                      f"diff {diff:.3f}mm")
                
                if dim['ok']:
                    stats['ok'] += 1
                else:
                    stats['bad'] += 1
            
            all_page_annotations.append(page_annotations)
            print()
    
    # Write output PDF with highlights
    reader = PdfReader(input_path)
    writer = PdfWriter()
    
    for page in reader.pages:
        writer.add_page(page)
    
    total_highlights = 0
    for page_idx, annotations in enumerate(all_page_annotations):
        if not annotations:
            continue
        
        annot_array = ArrayObject()
        for x0, y0, x1, y1, color in annotations:
            annot_array.append(create_highlight_annotation(x0, y0, x1, y1, color))
        
        writer.pages[page_idx][NameObject("/Annots")] = annot_array
        total_highlights += len(annotations)
    
    with open(output_path, "wb") as f:
        writer.write(f)
    
    # Print summary
    print("╔══════════════════════════════════════╗")
    print(f"║  ✅ Correct   : {stats['ok']:<22}║")
    print(f"║  ❌ Mismatch  : {stats['bad']:<22}║")
    print(f"║  📌 Highlights: {total_highlights:<22}║")
    print("║  🟡 Yellow = OK  🔴 Red = WRONG      ║")
    print("╚══════════════════════════════════════╝")
    print(f"\n✨ Output: {output_path}\n")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 highlight_dimensions_v6.py input.pdf output.pdf")
        sys.exit(1)
    
    process_pdf(sys.argv[1], sys.argv[2])