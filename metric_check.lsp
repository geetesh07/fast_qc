;;; =====================================================================
;;; METRIC_CHECK.LSP  v2
;;; Command : METRIC_CHECK
;;;
;;; With the metric drawing open, type METRIC_CHECK.
;;; A file picker lets you select the inch source drawing.
;;; The routine checks THREE kinds of content:
;;;
;;;   1. Dimension entities  (AcDb*Dimension, angular excluded)
;;;   2. TEXT entities       (AcDbText)
;;;   3. MTEXT entities      (AcDbMText, formatting codes stripped)
;;;
;;; Conversion rule:  metric = inch x 25.4   (tolerance +/- 0.1 mm)
;;;
;;; For text/mtext the routine extracts every number from the string.
;;; Numbers that contain a decimal point are treated as dimension
;;; annotation values and checked against the 25.4 rule.
;;; Plain integers (note numbers, counts, etc.) that are unchanged
;;; between drawings are silently skipped to avoid false positives.
;;; =====================================================================

(vl-load-com)


;;; -------------------------------------------------------------------
;;; mc:is-digit
;;; Returns T if single character C is an ASCII digit 0-9.
;;; -------------------------------------------------------------------
(defun mc:is-digit (c)
  (and (>= (ascii c) 48) (<= (ascii c) 57))
)


;;; -------------------------------------------------------------------
;;; mc:fmt
;;; Format real number VAL to PREC decimal places as a string.
;;; -------------------------------------------------------------------
(defun mc:fmt (val prec /)
  (rtos val 2 prec)
)


;;; -------------------------------------------------------------------
;;; mc:dashes
;;; Return a string containing N dash characters.
;;; -------------------------------------------------------------------
(defun mc:dashes (n / s)
  (setq s "")
  (repeat n
    (setq s (strcat s "-")))
  s
)


;;; -------------------------------------------------------------------
;;; mc:strip-mtext
;;; Remove AutoCAD MTEXT formatting codes from string S and return
;;; the plain readable content.
;;;
;;; Handles:
;;;   {\Hx.x;text}  {\fFont|...;text}  {\Cx;text}  -- format blocks
;;;   \P \~ \N                                       -- paragraph/space
;;;   %%c  %%d  %%p                                  -- special symbols
;;; -------------------------------------------------------------------
(defun mc:strip-mtext (s / res i len c nc depth skipSemi)
  (setq res      ""
        len      (strlen s)
        i        1
        depth    0
        skipSemi nil)
  (while (<= i len)
    (setq c (substr s i 1))
    (cond
      ;;-- Opening brace: if next char is \ it starts a format block
      ((= c "{")
       (setq depth (1+ depth))
       (if (and (< i len) (= (substr s (1+ i) 1) "\\"))
         (setq skipSemi T))
      )
      ;;-- Closing brace: drop depth, clear any lingering skipSemi
      ((= c "}")
       (if (> depth 0) (setq depth (- depth 1)))
       (setq skipSemi nil)
      )
      ;;-- Inside format header: skip everything until semicolon
      ((and skipSemi (not (= c ";")))
       nil
      )
      ;;-- Semicolon ends the format header; content follows
      ((and skipSemi (= c ";"))
       (setq skipSemi nil)
      )
      ;;-- Backslash escape sequence
      ((= c "\\")
       (if (<= (1+ i) len)
         (progn
           (setq nc (substr s (1+ i) 1))
           (if (wcmatch nc "PpNn~")
             (setq res (strcat res " ")))
           (setq i (1+ i))
         )
       )
      )
      ;;-- %% special symbol (%%c %%d %%p): skip 3 chars total
      ;;   After this cond arm the main loop adds 1 more, landing +3.
      ((and (= c "%")
            (<= (1+ i) len)
            (= (substr s (1+ i) 1) "%"))
       (setq i (+ i 2))
      )
      ;;-- Normal character: keep it
      (T
       (setq res (strcat res c))
      )
    )
    (setq i (1+ i))
  )
  res
)


;;; -------------------------------------------------------------------
;;; mc:extract-numbers
;;; Parse string STR and return a list of  (numericValue  isDecimal)
;;; pairs in left-to-right order.
;;;
;;; isDecimal is T when the token contained a "." (i.e. it looks like
;;; a dimension annotation such as .20 or 1.500).
;;; Plain integers like 3 or 42 return isDecimal = nil.
;;; -------------------------------------------------------------------
(defun mc:extract-numbers (str / result i len c numStr inNum hadDot)
  (setq result nil
        len    (strlen str)
        i      1
        inNum  nil
        numStr ""
        hadDot nil)
  (while (<= i len)
    (setq c (substr str i 1))
    (cond
      ;;-- Digit: accumulate
      ((mc:is-digit c)
       (setq numStr (strcat numStr c)
             inNum  T)
      )
      ;;-- Decimal point
      ((= c ".")
       (cond
         ;;- Already have a decimal: flush current number and reset
         (hadDot
          (if (and inNum (> (strlen numStr) 0))
            (setq result (cons (list (atof numStr) T) result)))
          (setq numStr "" inNum nil hadDot nil)
         )
         ;;- Can extend or start a decimal number
         ((or inNum
              (and (<= (1+ i) len)
                   (mc:is-digit (substr str (1+ i) 1))))
          (setq numStr (strcat numStr c)
                hadDot T
                inNum  T)
         )
         ;;- Stray dot: flush any integer that was building
         (T
          (if (and inNum (> (strlen numStr) 0))
            (setq result (cons (list (atof numStr) nil) result)))
          (setq numStr "" inNum nil hadDot nil)
         )
       )
      )
      ;;-- Any other character: flush the number we were building
      (T
       (if (and inNum (> (strlen numStr) 0))
         (setq result (cons (list (atof numStr) hadDot) result)))
       (setq numStr "" inNum nil hadDot nil)
      )
    )
    (setq i (1+ i))
  )
  ;;-- Flush the last number if the string ended while building one
  (if (and inNum (> (strlen numStr) 0))
    (setq result (cons (list (atof numStr) hadDot) result)))
  (reverse result)
)


;;; -------------------------------------------------------------------
;;; mc:linear-dim-p
;;; Returns T for linear/radial dimension entity names.
;;; Excludes angular dimension types.
;;; -------------------------------------------------------------------
(defun mc:linear-dim-p (oname)
  (and (wcmatch oname "*Dimension*")
       (not (wcmatch oname "*Angular*")))
)


;;; -------------------------------------------------------------------
;;; mc:get-dims
;;; Walk ModelSpace of DOC and collect all qualifying dimension values.
;;; Returns list of  (measurement (x y))
;;; Bad entities are caught and skipped.
;;; -------------------------------------------------------------------
(defun mc:get-dims (doc / ms cnt i obj oname measRes posRes pos result)
  (setq result nil
        ms     (vla-get-ModelSpace doc)
        cnt    (vla-get-Count ms)
        i      0)
  (while (< i cnt)
    (setq obj   (vla-item ms i)
          oname (vla-get-ObjectName obj))
    (if (mc:linear-dim-p oname)
      (progn
        (setq measRes
          (vl-catch-all-apply 'vla-get-Measurement (list obj)))
        (setq posRes
          (vl-catch-all-apply 'vla-get-TextPosition (list obj)))
        (if (and (not (vl-catch-all-error-p measRes))
                 (not (vl-catch-all-error-p posRes)))
          (progn
            (setq pos
              (vl-catch-all-apply 'vlax-safearray->list
                (list (vlax-variant-value posRes))))
            (if (and (not (vl-catch-all-error-p pos))
                     (listp pos)
                     (>= (length pos) 2))
              (setq result
                (cons (list measRes (list (car pos) (cadr pos)))
                      result))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  result
)


;;; -------------------------------------------------------------------
;;; mc:get-texts
;;; Walk ModelSpace of DOC and collect TEXT and MTEXT content.
;;; MTEXT formatting codes are stripped before storing.
;;; Returns list of  (plainString (x y))
;;; -------------------------------------------------------------------
(defun mc:get-texts (doc / ms cnt i obj oname txtRes posRes str pos result)
  (setq result nil
        ms     (vla-get-ModelSpace doc)
        cnt    (vla-get-Count ms)
        i      0)
  (while (< i cnt)
    (setq obj   (vla-item ms i)
          oname (vla-get-ObjectName obj))
    (cond
      ;;-- Plain TEXT entity
      ((wcmatch oname "AcDbText")
       (setq txtRes (vl-catch-all-apply 'vla-get-TextString    (list obj)))
       (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
       (if (and (not (vl-catch-all-error-p txtRes))
                (not (vl-catch-all-error-p posRes)))
         (progn
           (setq str txtRes)
           (setq pos
             (vl-catch-all-apply 'vlax-safearray->list
               (list (vlax-variant-value posRes))))
           (if (and (not (vl-catch-all-error-p pos))
                    (listp pos)
                    (>= (length pos) 2))
             (setq result
               (cons (list str (list (car pos) (cadr pos)))
                     result))
           )
         )
       )
      )
      ;;-- MTEXT entity: strip formatting first
      ((wcmatch oname "AcDbMText")
       (setq txtRes (vl-catch-all-apply 'vla-get-TextString    (list obj)))
       (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
       (if (and (not (vl-catch-all-error-p txtRes))
                (not (vl-catch-all-error-p posRes)))
         (progn
           (setq str (mc:strip-mtext txtRes))
           (setq pos
             (vl-catch-all-apply 'vlax-safearray->list
               (list (vlax-variant-value posRes))))
           (if (and (not (vl-catch-all-error-p pos))
                    (listp pos)
                    (>= (length pos) 2))
             (setq result
               (cons (list str (list (car pos) (cadr pos)))
                     result))
           )
         )
       )
      )
    )
    (setq i (1+ i))
  )
  result
)


;;; -------------------------------------------------------------------
;;; mc:sort-by-pos
;;; Sort a list whose elements are  (anything (x y))
;;; by X coordinate ascending, then Y ascending on ties.
;;; Works for both dimension lists and text lists.
;;; -------------------------------------------------------------------
(defun mc:sort-by-pos (lst /)
  (vl-sort lst
    (function
      (lambda (a b)
        (cond
          ((< (caadr a) (caadr b))
           T)
          ((and (equal (caadr a) (caadr b) 0.01)
                (< (cadadr a) (cadadr b)))
           T)
          (T nil)
        )
      )
    )
  )
)


;;; ===================================================================
;;; c:metric_check  --  the AutoCAD command
;;; ===================================================================
(defun c:metric_check
    (/ acadObj metricDoc inchFile openRes inchDoc
       ;;-- dimension vars
       metricDims inchDims mDimLen iDimLen dimN
       ;;-- text vars
       metricTexts inchTexts mTxtLen iTxtLen txtN
       ;;-- shared loop vars
       i j iVal mVal expected diff tolerance
       ;;-- text-specific loop vars
       iEntry mEntry iStr mStr iNums mNums iNLen
       iNumPair mNumPair iNum mNum isDecimal
       ;;-- error accumulators
       dimErrors txtErrors errLine
       dimPass dimFail txtPass txtFail
       ;;-- report
       report)

  (vl-load-com)

  ;;----------------------------------------------------------------
  ;; 1. Store the currently active (metric) document reference first
  ;;----------------------------------------------------------------
  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj))

  ;;----------------------------------------------------------------
  ;; 2. Read all data from metric drawing BEFORE opening anything else
  ;;----------------------------------------------------------------
  (princ "\nReading metric drawing...")
  (setq metricDims  (mc:get-dims  metricDoc))
  (setq metricTexts (mc:get-texts metricDoc))
  (princ
    (strcat " "
            (itoa (length metricDims))  " dim(s), "
            (itoa (length metricTexts)) " text/mtext entity(s) found."))

  ;;----------------------------------------------------------------
  ;; 3. Prompt the user to pick the inch source drawing
  ;;----------------------------------------------------------------
  (setq inchFile (getfiled "Select Inch Source Drawing" "" "dwg" 4))
  (if (not inchFile)
    (progn
      (princ "\nmetric_check: Cancelled by user.")
      (princ)
      (exit)
    )
  )

  ;;----------------------------------------------------------------
  ;; 4. Open inch drawing read-only
  ;;----------------------------------------------------------------
  (princ (strcat "\nOpening: " inchFile " ..."))
  (setq openRes
    (vl-catch-all-apply 'vla-open
      (list (vla-get-Documents acadObj) inchFile vlax-true)))

  (if (vl-catch-all-error-p openRes)
    (progn
      (alert
        (strcat "ERROR: Could not open the file.\n"
                (vl-catch-all-error-message openRes)))
      (princ)
      (exit)
    )
  )
  (setq inchDoc openRes)

  ;;----------------------------------------------------------------
  ;; 5. Read all data from the inch drawing
  ;;----------------------------------------------------------------
  (princ "\nReading inch drawing...")
  (setq inchDims  (mc:get-dims  inchDoc))
  (setq inchTexts (mc:get-texts inchDoc))
  (princ
    (strcat " "
            (itoa (length inchDims))  " dim(s), "
            (itoa (length inchTexts)) " text/mtext entity(s) found."))

  ;;----------------------------------------------------------------
  ;; 6. Close inch drawing and restore metric document
  ;;----------------------------------------------------------------
  (vla-close inchDoc vlax-false)
  (vla-Activate metricDoc)

  ;;----------------------------------------------------------------
  ;; 7. Sort all four lists by XY position
  ;;----------------------------------------------------------------
  (setq metricDims  (mc:sort-by-pos metricDims))
  (setq inchDims    (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts   (mc:sort-by-pos inchTexts))

  ;;----------------------------------------------------------------
  ;; 8. Initialise accumulators
  ;;----------------------------------------------------------------
  (setq tolerance 0.1
        dimErrors nil   txtErrors nil
        dimPass   0     dimFail   0
        txtPass   0     txtFail   0)

  ;;----------------------------------------------------------------
  ;; 9. DIMENSION CHECK
  ;;----------------------------------------------------------------
  (setq mDimLen (length metricDims)
        iDimLen (length inchDims)
        dimN    (min mDimLen iDimLen)
        i       0)

  (if (zerop dimN)
    (princ "\nNo dimension entities to compare.")
    (progn
      (princ (strcat "\nChecking " (itoa dimN) " dimension(s)..."))
      (repeat dimN
        (setq iVal    (car (nth i inchDims))
              mVal    (car (nth i metricDims))
              expected (* iVal 25.4)
              diff    (abs (- mVal expected)))
        (if (> diff tolerance)
          (progn
            (setq dimFail (1+ dimFail))
            (setq errLine
              (strcat
                "Dim #" (itoa (1+ i))
                "   " (mc:fmt iVal 4) "\""
                " x25.4 = " (mc:fmt expected 3) "mm"
                "   found: " (mc:fmt mVal 3) "mm"
                "   off by: " (mc:fmt diff 3) "mm  <--"))
            (setq dimErrors (cons errLine dimErrors))
          )
          (setq dimPass (1+ dimPass))
        )
        (setq i (1+ i))
      )
    )
  )

  ;;----------------------------------------------------------------
  ;; 10. TEXT / MTEXT CHECK
  ;;
  ;; For each matched text pair:
  ;;   - Extract all numbers as (value isDecimal) from both strings
  ;;   - If counts match, compare each pair:
  ;;       metric  ~= inch x 25.4          -> PASS
  ;;       metric  ~= inch  AND isDecimal  -> WARN (likely missed conversion)
  ;;       metric  ~= inch  AND NOT isDecimal -> silent skip (note/count number)
  ;;       neither condition               -> FAIL (wrong conversion)
  ;;----------------------------------------------------------------
  (setq mTxtLen (length metricTexts)
        iTxtLen (length inchTexts)
        txtN    (min mTxtLen iTxtLen)
        i       0)

  (if (zerop txtN)
    (princ "\nNo text entities to compare.")
    (progn
      (princ (strcat "\nChecking " (itoa txtN) " text/mtext pair(s)..."))
      (repeat txtN
        (setq iEntry (nth i inchTexts)
              mEntry (nth i metricTexts)
              iStr   (car iEntry)
              mStr   (car mEntry)
              iNums  (mc:extract-numbers iStr)
              mNums  (mc:extract-numbers mStr))

        ;;-- Only compare pairs where both sides have numbers
        ;;-- and the count of numbers is the same
        (if (and iNums
                 mNums
                 (= (length iNums) (length mNums)))
          (progn
            (setq iNLen (length iNums)
                  j     0)
            (repeat iNLen
              (setq iNumPair  (nth j iNums)
                    mNumPair  (nth j mNums)
                    iNum      (car  iNumPair)
                    isDecimal (cadr iNumPair)
                    mNum      (car  mNumPair)
                    expected  (* iNum 25.4)
                    diff      (abs (- mNum expected)))
              (cond
                ;;-- Correctly converted within tolerance
                ((<= diff tolerance)
                 (setq txtPass (1+ txtPass))
                )
                ;;-- Value is unchanged AND it was a decimal: likely
                ;;   the conversion was simply forgotten
                ((and isDecimal (equal mNum iNum 0.001))
                 (setq txtFail (1+ txtFail))
                 (setq errLine
                   (strcat
                     "Text #" (itoa (1+ i))
                     "  value " (mc:fmt iNum 4)
                     " is UNCHANGED  (expected "
                     (mc:fmt expected 3) "mm)"
                     "  [" iStr "]"))
                 (setq txtErrors (cons errLine txtErrors))
                )
                ;;-- Decimal number but wrong converted value
                (isDecimal
                 (setq txtFail (1+ txtFail))
                 (setq errLine
                   (strcat
                     "Text #" (itoa (1+ i))
                     "   " (mc:fmt iNum 4) "\""
                     " x25.4 = " (mc:fmt expected 3) "mm"
                     "   found: " (mc:fmt mNum 3) "mm"
                     "   off: " (mc:fmt diff 3) "mm"
                     "  [" iStr "] -> [" mStr "]"))
                 (setq txtErrors (cons errLine txtErrors))
                )
                ;;-- Integer that is unchanged: skip silently
                ;;   (e.g. note numbers, quantity counts, item tags)
                (T nil)
              )
              (setq j (1+ j))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;;----------------------------------------------------------------
  ;; 11. Build the report string
  ;;----------------------------------------------------------------
  (setq report
    (strcat "METRIC CHECK REPORT\n"
            (mc:dashes 52) "\n\n"))

  ;;-- Dimensions section
  (setq report (strcat report "[ DIMENSIONS ]\n"))
  (if (/= mDimLen iDimLen)
    (setq report
      (strcat report
        "  ! Count mismatch -- Inch: " (itoa iDimLen)
        "  Metric: " (itoa mDimLen)
        "  (comparing " (itoa dimN) ")\n"))
  )
  (if (zerop dimFail)
    (setq report
      (strcat report
        "  PASS -- All " (itoa dimPass) " dimension(s) correct.\n"))
    (progn
      (setq report
        (strcat report
          "  FAIL -- " (itoa dimFail)
          " error(s) in " (itoa dimN) " dimension(s):\n"))
      (foreach e (reverse dimErrors)
        (setq report (strcat report "  " e "\n")))
    )
  )

  ;;-- Text section
  (setq report (strcat report "\n[ TEXT / MTEXT ]\n"))
  (if (/= mTxtLen iTxtLen)
    (setq report
      (strcat report
        "  ! Count mismatch -- Inch: " (itoa iTxtLen)
        "  Metric: " (itoa mTxtLen)
        "  (comparing " (itoa txtN) ")\n"))
  )
  (if (zerop txtFail)
    (setq report
      (strcat report
        "  PASS -- All " (itoa txtPass) " text value(s) correct.\n"))
    (progn
      (setq report
        (strcat report
          "  FAIL -- " (itoa txtFail)
          " text error(s) found:\n"))
      (foreach e (reverse txtErrors)
        (setq report (strcat report "  " e "\n")))
    )
  )

  ;;-- Footer
  (setq report
    (strcat report
      "\n" (mc:dashes 52) "\n"
      "Tolerance : +/- " (mc:fmt tolerance 2) " mm\n"
      "Total errors : "
      (itoa (+ dimFail txtFail))
      "  (" (itoa dimFail) " dim + "
      (itoa txtFail) " text)"))

  ;;----------------------------------------------------------------
  ;; 12. Show report (popup + command line echo)
  ;;----------------------------------------------------------------
  (alert report)
  (princ "\n")
  (princ report)
  (princ)
)

(princ "\nMETRIC_CHECK.LSP v2 loaded. Type METRIC_CHECK to run.")
(princ)
