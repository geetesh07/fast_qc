;;; ============================================================================
;;;  dim_qc.lsp  -  Engineering Dimension QC Tool  (AutoLISP / Visual LISP)
;;;
;;;  Reads every DIMENSION entity directly from AutoCAD's drawing database,
;;;  verifies inch <-> mm conversion math, colour-codes the result, then
;;;  exports a highlighted PDF - zero OCR, zero LLM.
;;;
;;;  INSTALL
;;;    In AutoCAD command bar:
;;;      (load "C:/path/to/dim_qc.lsp")
;;;    Or place in AutoCAD support path and:
;;;      (load "dim_qc.lsp")
;;;    Or add to ACAD.LSP / ACADDOC.LSP to auto-load.
;;;
;;;  COMMANDS  (type in AutoCAD command bar)
;;;    DIMQC        - Scan all dimensions; colour-code yellow=OK / red=FAIL
;;;    DIMQC-PDF    - Export the current drawing as a colour-annotated PDF
;;;    DIMQC-RESET  - Restore all dimension colours to ByLayer
;;;
;;;  COLOUR LEGEND
;;;    2  (Yellow)  = conversion correct
;;;    1  (Red)     = conversion wrong or text could not be parsed
;;;    (untouched)  = dimension has no dual unit syntax -> skipped
;;;
;;;  TOLERANCE  (edit here)
;;;    DQC:REL-TOL  - relative tolerance, default 3%   (0.03)
;;;    DQC:ABS-TOL  - absolute tolerance mm, default 0.08 mm
;;; ============================================================================

(vl-load-com)   ; ensure Visual LISP COM support is loaded

;;; ---------------------------------------------------------------------------
;;;  CONFIGURATION
;;; ---------------------------------------------------------------------------
(setq DQC:REL-TOL    0.03)    ; 3 % relative tolerance
(setq DQC:ABS-TOL    0.08)    ; 0.08 mm absolute tolerance fallback
(setq DQC:MM/INCH    25.4)    ; mm per inch (exact)


;;; ---------------------------------------------------------------------------
;;;  DQC:TRIM  - Remove leading and trailing spaces from a string.
;;; ---------------------------------------------------------------------------
(defun DQC:trim (s / i)
  ;; Trim leading
  (setq i 1)
  (while (and (<= i (strlen s)) (= (substr s i 1) " "))
    (setq i (1+ i)))
  (setq s (substr s i))
  ;; Trim trailing
  (setq i (strlen s))
  (while (and (> i 0) (= (substr s i 1) " "))
    (setq i (1- i)))
  (if (> i 0) (substr s 1 i) "")
)


;;; ---------------------------------------------------------------------------
;;;  DQC:FIND-CHAR  - Return 1-based index of first occurrence of single-char
;;;  needle c in string s, starting search at position pos (1-based).
;;;  Returns 0 if not found.
;;; ---------------------------------------------------------------------------
(defun DQC:find-char (s c pos / i)
  (setq i pos)
  (while (and (<= i (strlen s))
              (not (= (substr s i 1) c)))
    (setq i (1+ i)))
  (if (<= i (strlen s)) i 0)
)


;;; ---------------------------------------------------------------------------
;;;  DQC:FIND-BRACKET  - Locate the first [inner] block in string s,
;;;  starting search at position pos (1-based).
;;;  Returns (open-pos  close-pos  inner-string) or nil.
;;; ---------------------------------------------------------------------------
(defun DQC:find-bracket (s pos / open close inner)
  (setq open (DQC:find-char s "[" pos))
  (if (= open 0)
    nil
    (progn
      (setq close (DQC:find-char s "]" (1+ open)))
      (if (= close 0)
        nil
        (list open close (DQC:trim (substr s (1+ open) (- close open 1))))
      )
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:CLEAN-NUM  - Strip leading non-numeric prefix characters (Ø ⌀ R r ±
;;;  + spaces) from token, then parse as a floating-point number.
;;;  Returns a real number, or nil if no numeric content found.
;;; ---------------------------------------------------------------------------
(defun DQC:clean-num (tok / i)
  (setq tok (DQC:trim tok))
  (if (= (strlen tok) 0)
    nil
    (progn
      ;; Advance i past any leading characters that can't start a number.
      ;; A number can start with a digit (# in wcmatch), a minus, or a period.
      (setq i 1)
      (while (and (<= i (strlen tok))
                  (not (wcmatch (substr tok i 1) "#"))    ; not a digit
                  (not (= (substr tok i 1) "-"))
                  (not (= (substr tok i 1) ".")))
        (setq i (1+ i)))
      ;; If we consumed the whole string, nothing numeric was found
      (if (> i (strlen tok))
        nil
        ;; atof reads as far as it can - stops cleanly at Ø ± etc.
        (atof (substr tok i))
      )
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:VERIFY  - Return T if mm is within tolerance of inch * 25.4.
;;; ---------------------------------------------------------------------------
(defun DQC:verify (inch mm / expected diff)
  (if (< (abs inch) 1e-9)
    ;; Zero inch -> mm should also be near zero
    (< (abs mm) 0.1)
    (progn
      (setq expected (* (abs inch) DQC:MM/INCH))
      (setq diff     (abs (- expected (abs mm))))
      ;; Pass if within relative OR absolute tolerance
      (or (<= (/ diff expected) DQC:REL-TOL)
          (<= diff DQC:ABS-TOL))
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:PARSE-DUAL  - Extract an (inch mm) pair from a dimension text string.
;;;
;;;  Supported formats:
;;;    "1.234 [31.34]"                      linear
;;;    "Ø1.234 [31.34]"                     diameter prefix
;;;    "R0.500 [12.70]"                     radius prefix
;;;    "1.234+0.005 [31.34] -0.003 ..."     stacked tol (nominal only extracted)
;;;    "1.234±0.005 [31.34±0.127]"          symmetric tol (nominal extracted via atof)
;;;    "±0.005 [±0.127]"                    tolerance-only
;;;
;;;  Strategy:
;;;    - Find the first [...] bracket -> that is the mm side
;;;    - Everything before the bracket is the inch side
;;;    - Strip leading prefix symbols; rely on atof stopping at ± + etc.
;;;
;;;  Returns (inch mm) list, or nil if no dual-unit pair found.
;;; ---------------------------------------------------------------------------
(defun DQC:parse-dual (txt / bracket inch-side mm-side inch mm)
  ;; Step 1: Find bracket block
  (setq bracket (DQC:find-bracket txt 1))
  (if (null bracket)
    ;; No [...] found -> not a dual-unit dimension, skip
    nil
    (progn
      (setq mm-side   (nth 2 bracket))
      (setq inch-side (DQC:trim (substr txt 1 (- (car bracket) 1))))
      ;; Step 2: Parse numbers
      ;; DQC:clean-num skips Ø R ± + prefix chars
      ;; atof naturally stops at ± in "1.234±0.005" -> gives us the nominal
      (setq inch (DQC:clean-num inch-side))
      (setq mm   (DQC:clean-num mm-side))
      ;; Step 3: Return pair or nil
      (if (and inch mm)
        (list inch mm)
        nil)
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:DIM-TEXT  - Get the display text from a dimension VLA object.
;;;  Priority: TextOverride (user-typed) > TextString (DIMALT formatted)
;;; ---------------------------------------------------------------------------
(defun DQC:dim-text (obj / ovr ts)
  ;; Try TextOverride first
  (setq ovr (vl-catch-all-apply 'vla-get-TextOverride (list obj)))
  (if (and (not (vl-catch-all-error-p ovr))
           (= (type ovr) 'STR)
           (> (strlen (DQC:trim ovr)) 0))
    ovr
    (progn
      ;; Fall back to TextString (the fully formatted display text)
      (setq ts (vl-catch-all-apply 'vla-get-TextString (list obj)))
      (if (and (not (vl-catch-all-error-p ts)) (= (type ts) 'STR))
        ts
        "")
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:PROCESS-DIM  - Process a single dimension VLA object.
;;;  Reads text, parses dual units, verifies, and sets colour.
;;;
;;;  Returns: (status inch mm text)
;;;    status = 'PASS  'FAIL  or  'SKIP  (no dual units)
;;; ---------------------------------------------------------------------------
(defun DQC:process-dim (obj / txt pair inch mm ok)
  (setq txt  (DQC:dim-text obj))
  (setq pair (DQC:parse-dual txt))
  (if (null pair)
    ;; No dual-unit syntax -> leave colour untouched
    (list 'SKIP nil nil txt)
    (progn
      (setq inch (car  pair))
      (setq mm   (cadr pair))
      (setq ok   (DQC:verify inch mm))
      ;; Colour: 2 = Yellow (ACI), 1 = Red (ACI)
      (vl-catch-all-apply 'vla-put-Color
        (list obj (if ok 2 1)))
      (list (if ok 'PASS 'FAIL) inch mm txt)
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:ALL-SPACES  - Return a list of all block-reference objects to scan:
;;;  the Model layout and all paper-space layouts.
;;;  Using (vla-get-Layouts) covers Model + every paper space sheet.
;;; ---------------------------------------------------------------------------
(defun DQC:all-spaces (doc / spaces blk)
  (setq spaces '())
  (vlax-for layout (vla-get-Layouts doc)
    (setq blk (vl-catch-all-apply 'vla-get-Block (list layout)))
    (if (not (vl-catch-all-error-p blk))
      (setq spaces (cons blk spaces))))
  spaces
)


;;; ---------------------------------------------------------------------------
;;;  C:DIMQC  - Main QC command.  Type  DIMQC  at the AutoCAD command bar.
;;; ---------------------------------------------------------------------------
(defun C:DIMQC ( / doc spaces oname obj res
                   total pass fail skip inch mm)

  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spaces (DQC:all-spaces doc))
  (setq total 0  pass 0  fail 0  skip 0)

  (princ "\n")
  (princ "==========================================================\n")
  (princ " DIM QC  -  Engineering Dimension Checker\n")
  (princ (strcat " Tolerance: "
                 (rtos (* DQC:REL-TOL 100) 2 1) "% relative  |  "
                 (rtos DQC:ABS-TOL 2 3) " mm absolute\n"))
  (princ "==========================================================\n")

  ;; ── Iterate all entities across all spaces ────────────────────────────────
  (foreach space spaces
    (vlax-for obj space
      (setq oname (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
      (if (and (not (vl-catch-all-error-p oname))
               (wcmatch oname "AcDbDim*"))
        (progn
          (setq res (DQC:process-dim obj))
          (setq total (1+ total))
          ;; Tally
          (cond
            ((= (car res) 'PASS) (setq pass (1+ pass)))
            ((= (car res) 'FAIL) (setq fail (1+ fail)))
            (T                   (setq skip (1+ skip)))
          )
          ;; Print non-skip results
          (if (not (= (car res) 'SKIP))
            (progn
              (setq inch (cadr res))
              (setq mm   (caddr res))
              (princ
                (strcat
                  (if (= (car res) 'PASS) " [PASS] " " [FAIL] ")
                  (rtos inch 2 4) "\"  ["
                  (rtos mm   2 3) " mm]"
                  "   expected: " (rtos (* (abs inch) DQC:MM/INCH) 2 3) " mm"
                  "   -> " (nth 3 res) "\n"
                )
              )
            )
          )
        )
      )
    )
  )

  ;; ── Refresh display ───────────────────────────────────────────────────────
  (vla-Regen doc acAllViewports)

  ;; ── Summary ───────────────────────────────────────────────────────────────
  (princ "==========================================================\n")
  (princ (strcat " Total dimensions found   : " (itoa total) "\n"))
  (princ (strcat " Dual-unit checked        : " (itoa (+ pass fail)) "\n"))
  (princ (strcat "   PASS  (yellow)         : " (itoa pass)  "\n"))
  (princ (strcat "   FAIL  (red)            : " (itoa fail)  "\n"))
  (princ (strcat " No dual unit  (skipped)  : " (itoa skip)  "\n"))
  (princ "==========================================================\n")

  (if (> fail 0)
    (princ " WARNING: Conversion errors found - check RED dimensions.\n")
    (if (> pass 0)
      (princ " All dual-unit dimensions PASS.\n")
      (princ " No dual-unit dimensions detected in this drawing.\n")
    )
  )
  (princ "\n Run DIMQC-PDF to export highlighted PDF.\n")
  (princ " Run DIMQC-RESET to restore original colours.\n\n")
  (princ)
)


;;; ---------------------------------------------------------------------------
;;;  C:DIMQC-PDF  - Export the current drawing to a colour-annotated PDF.
;;;  Uses AutoCAD's built-in EXPORTPDF command (AutoCAD 2017+).
;;;  For older versions, use PLOT -> DWG To PDF.pc3 manually.
;;; ---------------------------------------------------------------------------
(defun C:DIMQC-PDF ( / doc dwg pdf ans)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq dwg (vla-get-FullName doc))

  ;; Guard: drawing must be saved to have a path
  (if (= (strlen (DQC:trim dwg)) 0)
    (progn
      (princ "\nPlease SAVE the drawing first, then run DIMQC-PDF.\n")
      (princ)
      (exit)
    )
  )

  ;; Build default PDF path next to the DWG
  (setq pdf (strcat (vl-filename-directory dwg)
                    "\\"
                    (vl-filename-base dwg)
                    "_QC.pdf"))

  (princ (strcat "\nDefault output: " pdf "\n"))
  (setq ans (getstring T "Enter PDF path (or ENTER to use default): "))
  (if (> (strlen (DQC:trim ans)) 0)
    (setq pdf (DQC:trim ans)))

  (princ (strcat "\nExporting to: " pdf "\n"))

  ;; Run EXPORTPDF command  (AutoCAD 2017+)
  ;; The command accepts the file path as the first argument.
  (command "._EXPORTPDF" pdf)

  ;; If EXPORTPDF doesn't exist in the running version, AutoCAD will report
  ;; "Unknown command". In that case, run PLOT manually selecting:
  ;;   Plotter: DWG To PDF.pc3
  ;;   Plot style: acad.ctb  (preserves entity colours)
  (princ (strcat "\nDone.  PDF saved to: " pdf "\n\n"))
  (princ)
)


;;; ---------------------------------------------------------------------------
;;;  C:DIMQC-RESET  - Restore all dimension entity colours to ByLayer (256).
;;; ---------------------------------------------------------------------------
(defun C:DIMQC-RESET ( / doc spaces oname obj n)
  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spaces (DQC:all-spaces doc))
  (setq n 0)

  (foreach space spaces
    (vlax-for obj space
      (setq oname (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
      (if (and (not (vl-catch-all-error-p oname))
               (wcmatch oname "AcDbDim*"))
        (progn
          (vl-catch-all-apply 'vla-put-Color (list obj 256)) ; 256 = ByLayer
          (setq n (1+ n))
        )
      )
    )
  )

  (vla-Regen doc acAllViewports)
  (princ (strcat "\nReset " (itoa n) " dimension(s) to ByLayer.\n\n"))
  (princ)
)


;;; ---------------------------------------------------------------------------
;;;  Load confirmation message
;;; ---------------------------------------------------------------------------
(princ "\n")
(princ "==========================================================\n")
(princ " DIM QC loaded.  Available commands:\n")
(princ "   DIMQC        - Check all dimensions (yellow=OK, red=FAIL)\n")
(princ "   DIMQC-PDF    - Export colour-annotated PDF\n")
(princ "   DIMQC-RESET  - Restore original colours\n")
(princ "==========================================================\n\n")
(princ)
