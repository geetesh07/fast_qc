# fast_qc — Engineering Dimension QC Tool

Checks inch ↔ mm conversion correctness in AutoCAD drawings **directly inside AutoCAD** — zero OCR, zero LLM, zero PDF parsing. Reads the actual dimension database, does the math, colours the result, and exports a highlighted PDF.

- 🟡 **Yellow** = conversion is correct  
- 🔴 **Red** = conversion is wrong (or text couldn't be parsed)

---

## How it works

AutoCAD stores every dimension as an entity with exact values in its drawing database. This tool reads those values directly — no image/PDF involved until the very final export step.

```
dim_qc.lsp
  │
  ├── Iterates all AcDbDim* entities (Model + every paper-space layout)
  ├── Reads TextOverride / TextString from each dimension
  ├── Parses the  inch [mm]  dual-unit pair from the text
  ├── Verifies:  mm ≈ inch × 25.4  (within tolerance)
  ├── Sets entity colour  → 2=Yellow (pass) / 1=Red (fail)
  └── Export to PDF via AutoCAD's built-in EXPORTPDF command
```

---

## Supported conversion types

The unit type is **auto-detected from the label** in the dimension text. No configuration needed — just label your dimensions correctly and run `DIMQC`.

| Conversion | Primary label examples | Alt label examples | Factor |
|---|---|---|---|
| Linear (inch → mm) | `""` `IN` `INCH` | `""` `MM` | × 25.4 |
| Torque (in-lb → N-m) | `IN-LB` `LB-IN` `LBF-IN` | `N-M` `NM` | × 0.112985 |
| Torque (N-mm → N-m) | `N-MM` `NMM` | `N-M` `NM` | × 0.001 |
| Pressure (PSI → kPa) | `PSI` `LB/IN2` `LBF/IN2` | `KPA` `KN/M2` | × 6.89476 |
| MOI (lb-in² → kg-m²) | `LB-IN2` `LB-IN^2` | `KG-M2` `KG-M^2` | × 2.9264×10⁻⁴ |
| Imbalance (g-in → g-mm) | `G-IN` `G.IN` | `G-MM` `G.MM` | × 25.4 |

## Dimension text format

All types use the same bracket convention — the converted value goes in `[square brackets]`:

```
primary_value UNIT [alt_value UNIT]
```

| Example | Type |
|---------|------|
| `1.234 [31.34]` | Linear — no labels needed |
| `Ø0.750 [19.05]` | Diameter |
| `R0.500 [12.70]` | Radius |
| `1.234±0.005 [31.34±0.127]` | With symmetric tolerance |
| `12.5 IN-LB [1.412 N-M]` | Torque |
| `1500 PSI [10342.1 KPA]` | Pressure |
| `0.025 LB-IN2 [7.29E-6 KG-M2]` | Moment of inertia |
| `5.0 G-IN [127.0 G-MM]` | Imbalance |

> **No label on linear dimensions?** That's fine — `1.234 [31.34]` with no unit text is assumed to be inch → mm automatically.

---

## Install

1. Copy `dim_qc.lsp` anywhere accessible (e.g. same folder as your `.dwg`, or your AutoCAD support path).
2. In AutoCAD, type:
   ```
   (load "C:/full/path/to/dim_qc.lsp")
   ```
   Replace the path with the actual location. Use forward slashes.

   To auto-load on every AutoCAD session, add the `(load ...)` call to your `ACADDOC.LSP`.

---

## Usage

Open your `.dwg` file in AutoCAD, then type these commands in the command bar:

### `DIMQC`
Scans every dimension in the drawing (Model Space + all Paper Space layouts).  
- Sets each dual-unit dimension to **yellow** (correct) or **red** (wrong)  
- Prints a detailed pass/fail report in the command bar  
- Prints a summary count at the end  

### `DIMQC-PDF`
Exports the current drawing to a colour-annotated PDF.  
- Asks for an output path (defaults to `<drawing name>_QC.pdf` next to the `.dwg`)  
- Uses AutoCAD's built-in `EXPORTPDF` command (AutoCAD 2017+)

> **Older AutoCAD?** Run `PLOT` manually after `DIMQC`, choose plotter **DWG To PDF.pc3**, and plot style **acad.ctb** (preserves entity colours). The coloured dimensions will appear in the PDF.

### `DIMQC-RESET`
Resets every dimension colour back to **ByLayer**, undoing the QC colouring.

---

## Tolerance

Defaults are defined at the top of `dim_qc.lsp`:

```lisp
(setq DQC:REL-TOL 0.03)   ; 3% relative  e.g. 1.234" → 31.34 ± 0.94 mm
(setq DQC:ABS-TOL 0.08)   ; 0.08 mm absolute fallback (for tiny dimensions)
```

Change these values and reload the file to adjust.

---

## Requirements

- AutoCAD (any version with Visual LISP / `vl-load-com`)
- No additional software, no Python, no OCR

---

## Typical workflow

```
1. Open drawing.dwg in AutoCAD
2. (load "dim_qc.lsp")
3. DIMQC          ← dimensions light up yellow or red
4. Review reds, fix the wrong conversions in the drawing
5. DIMQC          ← rerun to confirm fixes
6. DIMQC-PDF      ← export final annotated PDF for QC sign-off
7. DIMQC-RESET    ← clean up colours before sending the drawing
```