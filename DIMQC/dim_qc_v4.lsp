;;; ============================================================================
;;;  dim_qc.lsp  -  Engineering Dimension QC Tool  (AutoLISP / Visual LISP)
;;;
;;;  Reads every DIMENSION entity from AutoCAD's drawing database, automatically
;;;  detects the unit type from labels in the dimension text, verifies the
;;;  conversion math, colour-codes the result, and exports an annotated PDF.
;;;  Zero OCR. Zero LLM. Reads directly from the drawing database.
;;;
;;;  SUPPORTED CONVERSION TYPES  (detected automatically from unit labels)
;;;    in  -> mm        linear dimensions  (no label or explicit IN/MM)
;;;    IN-LB  -> N-M    torque
;;;    N-MM   -> N-M    torque (metric input)
;;;    PSI    -> KPA    pressure
;;;    LB-IN2 -> KG-M2  moment of inertia
;;;    G-IN   -> G-MM   imbalance / mass-radius
;;;
;;;  DIMENSION TEXT FORMAT  (bracket convention)
;;;    primary_value UNIT [alt_value UNIT]
;;;    Examples:
;;;      1.234 [31.34]                    in -> mm   (no labels needed)
;;;      Ø0.750 [19.05]                   diameter
;;;      R0.500 [12.70]                   radius
;;;      12.5 IN-LB [1.412 N-M]           torque
;;;      1500 PSI [10342.1 KPA]           pressure
;;;      0.025 LB-IN2 [7.29E-6 KG-M2]    MOI
;;;      5.0 G-IN [127.0 G-MM]            imbalance
;;;
;;;  COMMANDS
;;;    DIMQC        - Scan all dimensions; colour yellow=OK / red=FAIL
;;;    DIMQC-PDF    - Export colour-annotated PDF
;;;    DIMQC-RESET  - Restore all dimension colours to ByLayer
;;;
;;;  COLOUR LEGEND
;;;    2 (Yellow) = conversion correct
;;;    1 (Red)    = conversion wrong, or text could not be parsed
;;;    (untouched) = no dual-unit bracket found  ->  skipped
;;;
;;;  INSTALL
;;;    In the AutoCAD command bar:
;;;      (load "C:/Users/Geeteshh/fast_qc/dim_qc.lsp")
;;;    Or drag-and-drop the .lsp file onto the AutoCAD window.
;;; ============================================================================

(vl-load-com)

;;; ---------------------------------------------------------------------------
;;;  TOLERANCE  (edit here to adjust pass/fail threshold)
;;; ---------------------------------------------------------------------------
(setq DQC:REL-TOL 0.03)    ; 3%  relative tolerance  (applies to all unit types)
(setq DQC:ABS-TOL 0.08)    ; 0.08 mm absolute fallback (prevents false fails on
                            ; very small linear values where 3% is sub-micron)


;;; ---------------------------------------------------------------------------
;;;  CONVERSION TABLE
;;;  Each entry:  (description  factor  primary-label-list  alt-label-list)
;;;
;;;  - "primary" is the unit written BEFORE the bracket  (e.g. inches, PSI)
;;;  - "alt"     is the unit written INSIDE the bracket  (e.g. mm, kPa)
;;;  - factor    is the multiplier:  alt = primary * factor
;;;
;;;  Rules:
;;;    - Labels are matched UPPERCASE after stripping spaces
;;;    - Empty string "" matches dimensions that have NO unit label
;;;    - Order matters: first match wins. Put specific entries BEFORE the
;;;      generic linear fallback at the bottom.
;;; ---------------------------------------------------------------------------
(setq DQC:CONVERSIONS
  (list

    ;; ── Torque : inch-pound-force -> Newton-metre ─────────────────────────
    (list "in-lb -> N-m"   0.112985
      '("IN-LB" "IN-LBS" "LB-IN" "LBF-IN" "IN.LB" "INLB" "IN*LB" "LB.IN")
      '("N-M" "NM" "N.M" "N*M"))

    ;; ── Torque : Newton-millimetre -> Newton-metre ────────────────────────
    (list "N-mm -> N-m"    0.001
      '("N-MM" "NMM" "N.MM" "N*MM" "NMILLIMETER")
      '("N-M" "NM" "N.M" "N*M"))

    ;; ── Pressure : pound per square inch -> kilopascal ───────────────────
    (list "PSI -> kPa"     6.89476
      '("PSI" "PSIA" "PSIG" "LB/IN2" "LBF/IN2" "LB/IN^2" "LBF/IN^2")
      '("KPA" "KN/M2" "KPASCAL" "KILOPASCAL"))

    ;; ── Moment of inertia : lb-in^2 -> kg-m^2 ────────────────────────────
    ;;   NOTE: LB-IN2 (no slash) = MOI.   LB/IN2 (with slash) = pressure (PSI).
    (list "lb-in2 -> kg-m2"  2.9264e-4
      '("LB-IN2" "LB-IN^2" "LBF-IN2" "LB.IN2" "LBIN2" "LBM-IN2")
      '("KG-M2" "KG-M^2" "KG.M2" "KGM2" "KG*M2"))

    ;; ── Imbalance : gram-inch -> gram-millimetre ──────────────────────────
    (list "g-in -> g-mm"   25.4
      '("G-IN" "G.IN" "GIN" "G*IN" "GRAM-IN" "OZ-IN")
      '("G-MM" "G.MM" "GMM" "G*MM" "GRAM-MM"))

    ;; ── Linear fallback : inch -> mm ─────────────────────────────────────
    ;;   Matches dimensions with NO label, or explicit IN / MM labels.
    ;;   MUST be last so specific entries above take priority.
    (list "in -> mm"       25.4
      '("" "IN" "INCH" "INCHES" "\"" "INS")
      '("" "MM" "MILLIMETER" "MILLIMETERS" "MILLI"))

  )
)


;;; ===========================================================================
;;;  UTILITY FUNCTIONS
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;;  DQC:TRIM  - Remove leading and trailing spaces.
;;; ---------------------------------------------------------------------------
(defun DQC:trim (s / i)
  (setq i 1)
  (while (and (<= i (strlen s)) (= (substr s i 1) " "))
    (setq i (1+ i)))
  (setq s (substr s i))
  (setq i (strlen s))
  (while (and (> i 0) (= (substr s i 1) " "))
    (setq i (1- i)))
  (if (> i 0) (substr s 1 i) "")
)


;;; ---------------------------------------------------------------------------
;;;  DQC:FIND-CHAR  - First occurrence of single char C in string S at or
;;;  after position POS (1-based).  Returns index or 0 if not found.
;;; ---------------------------------------------------------------------------
(defun DQC:find-char (s c pos / i)
  (setq i pos)
  (while (and (<= i (strlen s)) (not (= (substr s i 1) c)))
    (setq i (1+ i)))
  (if (<= i (strlen s)) i 0)
)


;;; ---------------------------------------------------------------------------
;;;  DQC:FIND-BRACKET  - Find first [inner] block in S starting at POS.
;;;  Returns (open-pos  close-pos  trimmed-inner) or nil.
;;; ---------------------------------------------------------------------------
(defun DQC:find-bracket (s pos / open close)
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
;;;  DQC:CLEAN-NUM  - Strip non-numeric prefix (Ø ⌀ R r ± + spaces) then
;;;  parse a floating-point number.  atof stops naturally at ± ° etc., so
;;;  "1.234±0.005" correctly yields 1.234.
;;;  Returns a real number, or nil if no numeric content found.
;;; ---------------------------------------------------------------------------
(defun DQC:clean-num (tok / i)
  (setq tok (DQC:trim tok))
  (if (= (strlen tok) 0)
    nil
    (progn
      ;; Advance past leading non-numeric prefix chars.
      ;; A number begins with a digit (#), a minus (-), or a period (.)
      (setq i 1)
      (while (and (<= i (strlen tok))
                  (not (wcmatch (substr tok i 1) "#"))  ; not a digit
                  (not (= (substr tok i 1) "-"))
                  (not (= (substr tok i 1) ".")))
        (setq i (1+ i)))
      (if (> i (strlen tok))
        nil                                ; nothing numeric
        (atof (substr tok i))              ; atof stops at first invalid char
      )
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:EXTRACT-UNIT  - Extract the unit label from a token like
;;;  "12.5 IN-LB" or "1.234" or "Ø0.750".
;;;
;;;  Strategy:
;;;    1. Skip any leading non-numeric prefix (Ø R ±)
;;;    2. Skip optional minus sign
;;;    3. Skip all numeric digits and decimal point
;;;    4. Skip whitespace
;;;    5. Collect alphabetic + digit + hyphen + slash + caret + dot chars
;;;       as the unit label  (stops at ± + - which indicate a tolerance suffix)
;;;
;;;  Returns a trimmed, uppercased string, or "" if no label present.
;;; ---------------------------------------------------------------------------
(defun DQC:extract-unit (tok / i unit-start c)
  (setq tok (DQC:trim tok))

  ;; Step 1: skip non-numeric prefix
  (setq i 1)
  (while (and (<= i (strlen tok))
              (not (wcmatch (substr tok i 1) "#"))
              (not (= (substr tok i 1) "."))
              (not (= (substr tok i 1) "-")))
    (setq i (1+ i)))

  ;; Step 2: skip optional leading minus  (for negative numbers like -0.5)
  (if (and (<= i (strlen tok)) (= (substr tok i 1) "-"))
    (setq i (1+ i)))

  ;; Step 3: skip digits and decimal point
  (while (and (<= i (strlen tok))
              (or (wcmatch (substr tok i 1) "#")
                  (= (substr tok i 1) ".")))
    (setq i (1+ i)))

  ;; Step 4: skip whitespace
  (while (and (<= i (strlen tok)) (= (substr tok i 1) " "))
    (setq i (1+ i)))

  ;; Step 5: collect unit characters
  ;; Valid chars: letters (@), digits (#), - / . ^ * (for compound units)
  ;; Stop at ± + or a second - that looks like a tolerance sign
  (setq unit-start i)
  (while (and (<= i (strlen tok))
              (progn
                (setq c (substr tok i 1))
                (or (wcmatch c "@")   ; letter
                    (wcmatch c "#")   ; digit
                    (= c "-")
                    (= c "/")
                    (= c ".")
                    (= c "^")
                    (= c "*"))))
    (setq i (1+ i)))

  (if (< unit-start i)
    (strcase (DQC:trim (substr tok unit-start (- i unit-start))))
    "")
)


;;; ---------------------------------------------------------------------------
;;;  DQC:FIND-FACTOR  - Look up (factor description) for a (primary / alt)
;;;  unit label pair against DQC:CONVERSIONS.
;;;  Returns (factor description) or nil if no match.
;;; ---------------------------------------------------------------------------
(defun DQC:find-factor (prim-unit alt-unit / entry result)
  (setq result nil)
  (foreach entry DQC:CONVERSIONS
    (if (null result)
      (if (and (member prim-unit (caddr  entry))   ; primary list
               (member alt-unit  (cadddr entry)))  ; alt list
        (setq result (list (cadr entry)   ; factor
                           (car  entry))) ; description
      )
    )
  )
  result
)


;;; ---------------------------------------------------------------------------
;;;  DQC:VERIFY  - Return T if alt value is within tolerance of primary * factor.
;;; ---------------------------------------------------------------------------
(defun DQC:verify (primary alt factor / expected diff)
  (if (< (abs primary) 1e-9)
    ;; Zero primary -> alt should also be near zero
    (< (abs alt) 0.1)
    (progn
      (setq expected (* (abs primary) factor))
      (setq diff     (abs (- expected (abs alt))))
      ;; Pass when within relative OR absolute tolerance
      (or (<= (/ diff expected) DQC:REL-TOL)
          (<= diff DQC:ABS-TOL))
    )
  )
)


;;; ===========================================================================
;;;  CORE PARSING AND PROCESSING
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;;  DQC:PARSE-DUAL  - Extract a verified pair from a dimension text string.
;;;
;;;  Returns (primary  alt  factor  description) or nil if:
;;;    - No [...] bracket found  (not a dual-unit dimension)
;;;    - Numeric parse fails
;;;    - No matching conversion found for the detected unit labels
;;; ---------------------------------------------------------------------------
(defun DQC:parse-dual (txt / bracket prim-side alt-side primary alt
                              prim-unit alt-unit fentry)
  (setq bracket (DQC:find-bracket txt 1))
  (if (null bracket)
    nil   ; no [...] bracket -> not a dual-unit dimension
    (progn
      (setq alt-side  (nth 2 bracket))
      (setq prim-side (DQC:trim (substr txt 1 (- (car bracket) 1))))

      ;; Parse the numeric values
      (setq primary (DQC:clean-num prim-side))
      (setq alt     (DQC:clean-num alt-side))

      (if (or (null primary) (null alt))
        nil   ; could not parse numbers
        (progn
          ;; Extract unit labels (uppercased)
          (setq prim-unit (DQC:extract-unit prim-side))
          (setq alt-unit  (DQC:extract-unit alt-side))

          ;; Look up conversion factor
          (setq fentry (DQC:find-factor prim-unit alt-unit))

          (if (null fentry)
            nil   ; unrecognised unit pair
            (list primary alt (car fentry) (cadr fentry))
          )
        )
      )
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:DIM-TEXT  - Get the display text from a dimension VLA object.
;;;  Priority: TextOverride (user-typed) then TextString (DIMALT formatted).
;;; ---------------------------------------------------------------------------
(defun DQC:dim-text (obj / ovr ts)
  (setq ovr (vl-catch-all-apply 'vla-get-TextOverride (list obj)))
  (if (and (not (vl-catch-all-error-p ovr))
           (= (type ovr) 'STR)
           (> (strlen (DQC:trim ovr)) 0))
    ovr
    (progn
      (setq ts (vl-catch-all-apply 'vla-get-TextString (list obj)))
      (if (and (not (vl-catch-all-error-p ts)) (= (type ts) 'STR))
        ts
        "")
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:PROCESS-DIM  - Process a single dimension VLA object.
;;;  Reads text, parses, verifies, and applies colour.
;;;  Returns (status  primary  alt  factor  conv-name  text)
;;;    status = 'PASS | 'FAIL | 'SKIP
;;; ---------------------------------------------------------------------------
(defun DQC:process-dim (obj / txt pair primary alt factor cname ok)
  (setq txt  (DQC:dim-text obj))
  (setq pair (DQC:parse-dual txt))
  (if (null pair)
    ;; No recognised dual-unit syntax -> leave colour untouched
    (list 'SKIP nil nil nil nil txt)
    (progn
      (setq primary (nth 0 pair))
      (setq alt     (nth 1 pair))
      (setq factor  (nth 2 pair))
      (setq cname   (nth 3 pair))
      (setq ok      (DQC:verify primary alt factor))
      ;; 2 = Yellow (ACI index), 1 = Red (ACI index)
      (vl-catch-all-apply 'vla-put-Color (list obj (if ok 2 1)))
      (list (if ok 'PASS 'FAIL) primary alt factor cname txt)
    )
  )
)


;;; ---------------------------------------------------------------------------
;;;  DQC:ALL-SPACES  - Return a list of all block objects to scan:
;;;  Model layout + every paper-space layout.
;;; ---------------------------------------------------------------------------
(defun DQC:ALL-SPACES (doc / spaces blk)
  (setq spaces '())
  (vlax-for layout (vla-get-Layouts doc)
    (setq blk (vl-catch-all-apply 'vla-get-Block (list layout)))
    (if (not (vl-catch-all-error-p blk))
      (setq spaces (cons blk spaces))))
  spaces
)


;;; ===========================================================================
;;;  PUBLIC COMMANDS
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;;  C:DIMQC  - Main QC command.  Type  DIMQC  at the AutoCAD command bar.
;;; ---------------------------------------------------------------------------
(defun C:DIMQC ( / doc spaces obj oname res
                   total pass fail skip
                   primary alt factor cname txt expected)

  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spaces (DQC:ALL-SPACES doc))
  (setq total 0  pass 0  fail 0  skip 0)

  (princ "\n")
  (princ "============================================================\n")
  (princ " DIM QC  -  Engineering Dimension Checker\n")
  (princ (strcat " Tolerance : "
                 (rtos (* DQC:REL-TOL 100) 2 1) "%  relative  |  "
                 (rtos DQC:ABS-TOL 2 3) " mm  absolute\n"))
  (princ "============================================================\n")

  (foreach space spaces
    (vlax-for obj space
      (setq oname (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
      (if (and (not (vl-catch-all-error-p oname))
               (wcmatch oname "AcDbDim*"))
        (progn
          (setq res (DQC:process-dim obj))
          (setq total (1+ total))
          (cond
            ((= (car res) 'PASS) (setq pass (1+ pass)))
            ((= (car res) 'FAIL) (setq fail (1+ fail)))
            (T                   (setq skip (1+ skip)))
          )
          ;; Print detail line for checked (non-skipped) dimensions only
          (if (not (= (car res) 'SKIP))
            (progn
              (setq primary (nth 1 res))
              (setq alt     (nth 2 res))
              (setq factor  (nth 3 res))
              (setq cname   (nth 4 res))
              (setq txt     (nth 5 res))
              (setq expected (* (abs primary) factor))
              (princ
                (strcat
                  (if (= (car res) 'PASS) " [PASS]  " " [FAIL]  ")
                  "[" cname "]  "
                  (rtos primary 2 4) " -> "
                  (rtos alt 2 4) "  "
                  "(expected " (rtos expected 2 4) ")  "
                  "\"" txt "\"\n"
                )
              )
            )
          )
        )
      )
    )
  )

  ;; Refresh display so colours show immediately
  (vla-Regen doc acAllViewports)

  ;; Summary
  (princ "============================================================\n")
  (princ (strcat " Dimensions scanned      : " (itoa total) "\n"))
  (princ (strcat " Dual-unit checked       : " (itoa (+ pass fail)) "\n"))
  (princ (strcat "   PASS  (yellow)        : " (itoa pass)  "\n"))
  (princ (strcat "   FAIL  (red)           : " (itoa fail)  "\n"))
  (princ (strcat " Skipped (no dual unit)  : " (itoa skip)  "\n"))
  (princ "============================================================\n")
  (cond
    ((> fail 0) (princ " WARNING: Errors found - inspect RED dimensions.\n"))
    ((> pass 0) (princ " All dual-unit dimensions PASS.\n"))
    (T          (princ " No dual-unit dimensions detected.\n"))
  )
  (princ "\n Run  DIMQC-PDF   to export highlighted PDF.\n")
  (princ " Run  DIMQC-RESET to restore original colours.\n\n")
  (princ)
)


;;; ---------------------------------------------------------------------------
;;;  C:DIMQC-PDF  - Export the drawing to a colour-annotated PDF.
;;;  Uses AutoCAD's EXPORTPDF command (AutoCAD 2017+).
;;; ---------------------------------------------------------------------------
(defun C:DIMQC-PDF ( / doc dwg pdf ans)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq dwg (vla-get-FullName doc))

  ;; Drawing must be saved first
  (if (= (strlen (DQC:trim dwg)) 0)
    (progn
      (princ "\nPlease SAVE the drawing first, then run DIMQC-PDF.\n\n")
      (princ)
      (exit)
    )
  )

  ;; Build default output path: same folder, same name + _QC.pdf
  (setq pdf (strcat (vl-filename-directory dwg)
                    "\\"
                    (vl-filename-base dwg)
                    "_QC.pdf"))

  (princ (strcat "\nDefault output: " pdf "\n"))
  (setq ans (getstring T "Enter PDF path (or press Enter for default): "))
  (if (> (strlen (DQC:trim ans)) 0)
    (setq pdf (DQC:trim ans)))

  (princ (strcat "\nExporting to: " pdf "\n"))
  (princ "(Use acad.ctb as plot style to preserve entity colours)\n")

  ;; EXPORTPDF is available in AutoCAD 2017+
  ;; If your version doesn't have it, run PLOT -> DWG To PDF.pc3 manually.
  (command "._EXPORTPDF" pdf)

  (princ (strcat "\nDone.  PDF saved: " pdf "\n\n"))
  (princ)
)


;;; ---------------------------------------------------------------------------
;;;  C:DIMQC-RESET  - Restore all dimension entity colours to ByLayer (256).
;;; ---------------------------------------------------------------------------
(defun C:DIMQC-RESET ( / doc spaces obj oname n)
  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq spaces (DQC:ALL-SPACES doc))
  (setq n 0)

  (foreach space spaces
    (vlax-for obj space
      (setq oname (vl-catch-all-apply 'vla-get-ObjectName (list obj)))
      (if (and (not (vl-catch-all-error-p oname))
               (wcmatch oname "AcDbDim*"))
        (progn
          (vl-catch-all-apply 'vla-put-Color (list obj 256))  ; 256 = ByLayer
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
;;;  Load confirmation
;;; ---------------------------------------------------------------------------
(princ "\n")
(princ "============================================================\n")
(princ " DIM QC loaded successfully.\n")
(princ "\n")
(princ " Supported unit conversions:\n")
(foreach entry DQC:CONVERSIONS
  (princ (strcat "   " (car entry) "\n")))
(princ "\n")
(princ " Commands:\n")
(princ "   DIMQC        - Check all dimensions (yellow=OK, red=FAIL)\n")
(princ "   DIMQC-PDF    - Export colour-annotated PDF\n")
(princ "   DIMQC-RESET  - Restore original colours\n")
(princ "============================================================\n\n")
(princ)
