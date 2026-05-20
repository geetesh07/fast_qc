;;; =====================================================================
;;; METRIC_CHECK.LSP  v9
;;; Commands:
;;;   METRIC_CHECK -- run inch-to-metric check
;;;   METRIC_CLEAR -- erase QC balloons and layers
;;;
;;; Main fixes in v9:
;;;   1. File picker opens in the same folder as the active metric drawing.
;;;   2. Dimension value is read from visible override text first.
;;;      If no override exists, measurement is used with dimension's own
;;;      LinearScaleFactor so the value better matches what AutoCAD displays.
;;;   3. Expected metric value is calculated once only:
;;;        expected = inch_value x 25.4
;;;      Then compared against the metric drawing value.
;;;   4. Matching is value-first, position-second to avoid random pairing.
;;;   5. Text parser handles R.03, R0.8, (.03), (R0.8), etc.
;;;   6. Note sequence numbers like 1. 2. 3. are ignored in text.
;;;   7. Labels show max 4 decimals, trailing zeroes removed.
;;;   8. Fail balloons are white.
;;; =====================================================================

(vl-load-com)


;;; =====================================================================
;;; GLOBAL SETTINGS
;;; =====================================================================

(setq *mc-conv* 25.4)
(setq *mc-tolerance* 0.1)

;;; Position limits are only used after value matching.
;;; Increase if drawings are shifted far apart.
(setq *mc-dim-match-limit* 1000000.0)
(setq *mc-text-match-limit* 1000000.0)


;;; =====================================================================
;;; BASIC STRING / NUMBER HELPERS
;;; =====================================================================

(defun mc:is-digit (c)
  (and c
       (= (type c) 'STR)
       (= (strlen c) 1)
       (>= (ascii c) 48)
       (<= (ascii c) 57))
)


(defun mc:rtrim0 (s /)
  ;; Remove trailing zeroes and trailing decimal point.
  ;; "25.4000" -> "25.4"
  ;; "25.0000" -> "25"
  ;; "0.0300"  -> "0.03"
  (while (and (> (strlen s) 1)
              (= (substr s (strlen s) 1) "0")
              (vl-string-search "." s))
    (setq s (substr s 1 (1- (strlen s))))
  )

  (if (and (> (strlen s) 1)
           (= (substr s (strlen s) 1) "."))
    (setq s (substr s 1 (1- (strlen s))))
  )

  s
)


(defun mc:fmt (val /)
  ;; Max 4 decimals, then trim unnecessary zeroes.
  (mc:rtrim0 (rtos val 2 4))
)


(defun mc:distance (p1 p2 / dx dy)
  (setq dx (- (car p1) (car p2))
        dy (- (cadr p1) (cadr p2)))
  (sqrt (+ (* dx dx) (* dy dy)))
)


(defun mc:scale-point (p sc /)
  (list (* (car p) sc) (* (cadr p) sc))
)


(defun mc:safearray-point (variantValue / res)
  (setq res
    (vl-catch-all-apply 'vlax-safearray->list
      (list (vlax-variant-value variantValue))))

  (if (and (not (vl-catch-all-error-p res))
           (listp res)
           (>= (length res) 2))
    (list (car res) (cadr res))
    nil
  )
)


;;; =====================================================================
;;; MTEXT STRIPPING
;;; =====================================================================

(defun mc:strip-mtext (s / res i len c nc depth skipSemi)
  ;; Removes common MTEXT formatting so numbers like R.03 can be extracted.
  (setq res      ""
        len      (strlen s)
        i        1
        depth    0
        skipSemi nil)

  (while (<= i len)
    (setq c (substr s i 1))

    (cond
      ((= c "{")
       (setq depth (1+ depth))
       (if (and (< i len) (= (substr s (1+ i) 1) "\\"))
         (setq skipSemi T))
      )

      ((= c "}")
       (if (> depth 0)
         (setq depth (- depth 1)))
       (setq skipSemi nil)
      )

      ((and skipSemi (not (= c ";")))
       nil
      )

      ((and skipSemi (= c ";"))
       (setq skipSemi nil)
      )

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

      ((and (= c "%")
            (<= (1+ i) len)
            (= (substr s (1+ i) 1) "%"))
       ;; Skip AutoCAD symbols like %%c, %%d, %%p
       (setq i (+ i 2))
      )

      (T
       (setq res (strcat res c)))
    )

    (setq i (1+ i))
  )

  res
)


;;; =====================================================================
;;; NUMBER EXTRACTION
;;; =====================================================================

(defun mc:extract-all-numbers (str / result i len c token hadDot hadDigitAfterDot stopToken)
  ;; Extracts all numeric values.
  ;;
  ;; Accepts:
  ;;   25
  ;;   25.4
  ;;   .03
  ;;   R.03
  ;;   R0.8
  ;;   (R.03)
  ;;
  ;; Used for dimension override text.
  (setq result nil
        len    (strlen str)
        i      1)

  (while (<= i len)
    (setq c (substr str i 1))

    (cond
      ;; Number starting with digit.
      ((mc:is-digit c)
       (setq token c
             hadDot nil
             hadDigitAfterDot nil
             stopToken nil
             i (1+ i))

       (while (and (<= i len)
                   (not stopToken)
                   (or (mc:is-digit (substr str i 1))
                       (= (substr str i 1) ".")))
         (setq c (substr str i 1))

         (cond
           ((mc:is-digit c)
            (setq token (strcat token c))
            (if hadDot
              (setq hadDigitAfterDot T))
           )

           ((= c ".")
            (if hadDot
              (setq stopToken T)
              (progn
                (setq token (strcat token c))
                (setq hadDot T)
              )
            )
           )
         )

         (if (not stopToken)
           (setq i (1+ i)))
       )

       ;; Accept integer or decimal.
       ;; But do not accept "1." because that is usually note numbering.
       (if (not (and hadDot (not hadDigitAfterDot)))
         (setq result (cons (atof token) result))
       )

       (setq i (1- i))
      )

      ;; Number starting with dot, example .03
      ((and (= c ".")
            (< i len)
            (mc:is-digit (substr str (1+ i) 1)))
       (setq token "0."
             i (+ i 1))

       (while (and (<= i len)
                   (mc:is-digit (substr str i 1)))
         (setq token (strcat token (substr str i 1)))
         (setq i (1+ i))
       )

       (setq result (cons (atof token) result))
       (setq i (1- i))
      )
    )

    (setq i (1+ i))
  )

  (reverse result)
)


(defun mc:extract-decimal-numbers (str / result i len c token hadDot hadDigitAfterDot stopToken)
  ;; Extracts decimal-only values for notes/text.
  ;;
  ;; Ignored:
  ;;   1.
  ;;   2.
  ;;   3.
  ;;   10
  ;;
  ;; Accepted:
  ;;   1.25
  ;;   .030
  ;;   R.03
  ;;   R0.8
  ;;   (R0.8)
  ;;
  ;; This prevents note sequence numbers from being converted.
  (setq result nil
        len    (strlen str)
        i      1)

  (while (<= i len)
    (setq c (substr str i 1))

    (cond
      ;; Starts with digit.
      ((mc:is-digit c)
       (setq token c
             hadDot nil
             hadDigitAfterDot nil
             stopToken nil
             i (1+ i))

       (while (and (<= i len)
                   (not stopToken)
                   (or (mc:is-digit (substr str i 1))
                       (= (substr str i 1) ".")))
         (setq c (substr str i 1))

         (cond
           ((mc:is-digit c)
            (setq token (strcat token c))
            (if hadDot
              (setq hadDigitAfterDot T))
           )

           ((= c ".")
            (if hadDot
              (setq stopToken T)
              (progn
                (setq token (strcat token c))
                (setq hadDot T)
              )
            )
           )
         )

         (if (not stopToken)
           (setq i (1+ i)))
       )

       ;; Only accept if true decimal with digit after decimal.
       ;; This skips "1." "2." "3."
       (if (and hadDot hadDigitAfterDot)
         (setq result (cons (atof token) result))
       )

       (setq i (1- i))
      )

      ;; Starts with dot, example .03
      ((and (= c ".")
            (< i len)
            (mc:is-digit (substr str (1+ i) 1)))
       (setq token "0."
             i (+ i 1))

       (while (and (<= i len)
                   (mc:is-digit (substr str i 1)))
         (setq token (strcat token (substr str i 1)))
         (setq i (1+ i))
       )

       (setq result (cons (atof token) result))
       (setq i (1- i))
      )
    )

    (setq i (1+ i))
  )

  (reverse result)
)


(defun mc:first-number-from-string (str / nums)
  (setq nums (mc:extract-all-numbers str))
  (if nums
    (car nums)
    nil
  )
)


;;; =====================================================================
;;; DIMENSION HELPERS
;;; =====================================================================

(defun mc:linear-dim-p (oname)
  ;; Keep linear/radial/diameter dimensions.
  ;; Exclude angular dimensions.
  (and (wcmatch oname "*Dimension*")
       (not (wcmatch oname "*Angular*")))
)


(defun mc:get-dim-lfac (obj / res)
  ;; Dimension LinearScaleFactor affects displayed dimension value.
  ;; This is NOT converting inch to metric again.
  ;; It only tries to read what AutoCAD displays for that dimension.
  (setq res
    (vl-catch-all-apply 'vla-get-LinearScaleFactor (list obj)))

  (if (and (not (vl-catch-all-error-p res))
           (numberp res)
           (/= res 0.0))
    res
    1.0
  )
)


(defun mc:get-dim-override-value (obj / txtRes txt stripped val)
  ;; If dimension has visible override text like:
  ;;   R.03
  ;;   (R.03)
  ;;   %%c.9284
  ;; read that value first.
  ;;
  ;; If override is empty or contains "<>", return nil.
  (setq txtRes
    (vl-catch-all-apply 'vla-get-TextOverride (list obj)))

  (if (vl-catch-all-error-p txtRes)
    nil
    (progn
      (setq txt txtRes)

      (if (or (not txt)
              (= txt "")
              (vl-string-search "<>" txt))
        nil
        (progn
          (setq stripped (mc:strip-mtext txt))
          (setq val (mc:first-number-from-string stripped))
          val
        )
      )
    )
  )
)


(defun mc:get-dim-value (obj / overrideVal measRes lfac)
  ;; Root fix:
  ;; 1. If the dimension has override text, use the visible override value.
  ;; 2. Otherwise use measurement x dimension LinearScaleFactor.
  ;;
  ;; Important:
  ;; This is NOT:
  ;;   expected x 25.4 again
  ;;
  ;; This is only trying to read the actual value shown by the dimension.
  (setq overrideVal (mc:get-dim-override-value obj))

  (if overrideVal
    overrideVal
    (progn
      (setq measRes
        (vl-catch-all-apply 'vla-get-Measurement (list obj)))

      (if (vl-catch-all-error-p measRes)
        nil
        (progn
          (setq lfac (mc:get-dim-lfac obj))
          (* measRes lfac)
        )
      )
    )
  )
)


(defun mc:dim-geometry-point (obj / p1Res p2Res p1 p2 txtRes txtPos)
  ;; Stable geometry point for matching and balloon placement.
  ;; ExtLine midpoint first, text position fallback.
  (setq p1 nil
        p2 nil
        txtPos nil)

  (setq p1Res (vl-catch-all-apply 'vla-get-ExtLine1Point (list obj)))
  (setq p2Res (vl-catch-all-apply 'vla-get-ExtLine2Point (list obj)))

  (if (and (not (vl-catch-all-error-p p1Res))
           (not (vl-catch-all-error-p p2Res)))
    (progn
      (setq p1 (mc:safearray-point p1Res))
      (setq p2 (mc:safearray-point p2Res))
    )
  )

  (if (and p1 p2)
    (list (/ (+ (car p1) (car p2)) 2.0)
          (/ (+ (cadr p1) (cadr p2)) 2.0))
    (progn
      (setq txtRes
        (vl-catch-all-apply 'vla-get-TextPosition (list obj)))

      (if (not (vl-catch-all-error-p txtRes))
        (setq txtPos (mc:safearray-point txtRes))
      )

      txtPos
    )
  )
)


(defun mc:get-dims (doc / ms cnt i obj oname dimVal pos handle result)
  ;; Returns list:
  ;;   (value (x y) handle)
  (setq result nil
        ms     (vla-get-ModelSpace doc)
        cnt    (vla-get-Count ms)
        i      0)

  (while (< i cnt)
    (setq obj   (vla-item ms i)
          oname (vla-get-ObjectName obj))

    (if (mc:linear-dim-p oname)
      (progn
        (setq dimVal (mc:get-dim-value obj))
        (setq pos    (mc:dim-geometry-point obj))

        (setq handle
          (vl-catch-all-apply 'vla-get-Handle (list obj)))

        (if (vl-catch-all-error-p handle)
          (setq handle "")
        )

        (if (and dimVal pos)
          (setq result
            (cons (list dimVal pos handle) result))
        )
      )
    )

    (setq i (1+ i))
  )

  result
)


;;; =====================================================================
;;; TEXT HELPERS
;;; =====================================================================

(defun mc:get-texts (doc / ms cnt i obj oname txtRes posRes pos handle result)
  ;; Returns list:
  ;;   (plainText (x y) handle)
  (setq result nil
        ms     (vla-get-ModelSpace doc)
        cnt    (vla-get-Count ms)
        i      0)

  (while (< i cnt)
    (setq obj   (vla-item ms i)
          oname (vla-get-ObjectName obj))

    (cond
      ((wcmatch oname "AcDbText")
       (setq txtRes (vl-catch-all-apply 'vla-get-TextString     (list obj)))
       (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))

       (setq handle
         (vl-catch-all-apply 'vla-get-Handle (list obj)))
       (if (vl-catch-all-error-p handle)
         (setq handle "")
       )

       (if (and (not (vl-catch-all-error-p txtRes))
                (not (vl-catch-all-error-p posRes)))
         (progn
           (setq pos
             (vl-catch-all-apply 'vlax-safearray->list
               (list (vlax-variant-value posRes))))

           (if (and (not (vl-catch-all-error-p pos))
                    (listp pos)
                    (>= (length pos) 2))
             (setq result
               (cons
                 (list txtRes
                       (list (car pos) (cadr pos))
                       handle)
                 result))
           )
         )
       )
      )

      ((wcmatch oname "AcDbMText")
       (setq txtRes (vl-catch-all-apply 'vla-get-TextString     (list obj)))
       (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))

       (setq handle
         (vl-catch-all-apply 'vla-get-Handle (list obj)))
       (if (vl-catch-all-error-p handle)
         (setq handle "")
       )

       (if (and (not (vl-catch-all-error-p txtRes))
                (not (vl-catch-all-error-p posRes)))
         (progn
           (setq pos
             (vl-catch-all-apply 'vlax-safearray->list
               (list (vlax-variant-value posRes))))

           (if (and (not (vl-catch-all-error-p pos))
                    (listp pos)
                    (>= (length pos) 2))
             (setq result
               (cons
                 (list (mc:strip-mtext txtRes)
                       (list (car pos) (cadr pos))
                       handle)
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


;;; =====================================================================
;;; SORTING / MATCHING
;;; =====================================================================

(defun mc:sort-by-pos (lst /)
  (vl-sort lst
    (function
      (lambda (a b)
        (cond
          ((< (caadr a) (caadr b)) T)
          ((and (equal (caadr a) (caadr b) 0.01)
                (< (cadadr a) (cadadr b))) T)
          (T nil)
        )
      )
    )
  )
)


(defun mc:remove-first (item lst / result removed x)
  (setq result nil
        removed nil)

  (foreach x lst
    (if (and (not removed) (equal x item 1e-8))
      (setq removed T)
      (setq result (cons x result))
    )
  )

  (reverse result)
)


(defun mc:pos-distance-best (inchPos metricPos / d1 d2 sp)
  ;; Some drawings are physically scaled by 25.4, some are not.
  ;; Use whichever position comparison is closer.
  (setq sp (mc:scale-point inchPos *mc-conv*))
  (setq d1 (mc:distance sp metricPos))
  (setq d2 (mc:distance inchPos metricPos))

  (if (< d1 d2)
    d1
    d2
  )
)


(defun mc:find-best-dim-match (iEntry metricList / expected best bestScore bestVDiff bestPDist mEntry mVal vDiff pDist score)
  ;; Dimension matching is now value-first, position-second.
  ;;
  ;; For every inch dimension:
  ;;   expected = inch_value x 25.4
  ;; Find metric dimension closest to expected value.
  ;; Position is only used as a tie-breaker.
  ;;
  ;; Returns:
  ;;   (metricEntry valueDiff positionDistance)
  (setq expected (* (car iEntry) *mc-conv*)
        best nil
        bestScore nil
        bestVDiff nil
        bestPDist nil)

  (foreach mEntry metricList
    (setq mVal  (car mEntry))
    (setq vDiff (abs (- mVal expected)))
    (setq pDist (mc:pos-distance-best (cadr iEntry) (cadr mEntry)))

    ;; Score strongly prioritizes value difference.
    ;; Position only breaks ties between similar values.
    (setq score (+ (* vDiff 100000.0) pDist))

    (if (or (not bestScore) (< score bestScore))
      (setq best      mEntry
            bestScore score
            bestVDiff vDiff
            bestPDist pDist)
    )
  )

  (if best
    (list best bestVDiff bestPDist)
    nil
  )
)


(defun mc:text-entry-decimals (entry /)
  (mc:extract-decimal-numbers (car entry))
)


(defun mc:text-values-diff (inchNums metricNums / total i expected diff)
  ;; Returns nil if number count mismatch.
  ;; Otherwise returns total absolute difference between expected and metric nums.
  (if (/= (length inchNums) (length metricNums))
    nil
    (progn
      (setq total 0.0
            i 0)

      (repeat (length inchNums)
        (setq expected (* (nth i inchNums) *mc-conv*))
        (setq diff (abs (- (nth i metricNums) expected)))
        (setq total (+ total diff))
        (setq i (1+ i))
      )

      total
    )
  )
)


(defun mc:find-best-text-match (iEntry metricList / iNums best bestScore bestVDiff bestPDist mEntry mNums vDiff pDist score)
  ;; Text matching is also value-first.
  ;; It compares decimal numbers only.
  (setq iNums (mc:text-entry-decimals iEntry)
        best nil
        bestScore nil
        bestVDiff nil
        bestPDist nil)

  (foreach mEntry metricList
    (setq mNums (mc:text-entry-decimals mEntry))
    (setq vDiff (mc:text-values-diff iNums mNums))

    (if vDiff
      (progn
        (setq pDist (mc:pos-distance-best (cadr iEntry) (cadr mEntry)))
        (setq score (+ (* vDiff 100000.0) pDist))

        (if (or (not bestScore) (< score bestScore))
          (setq best      mEntry
                bestScore score
                bestVDiff vDiff
                bestPDist pDist)
        )
      )
    )
  )

  (if best
    (list best bestVDiff bestPDist)
    nil
  )
)


;;; =====================================================================
;;; LAYERS / BALLOONS
;;; =====================================================================

(defun mc:balloon-height (/ dtxt dscl sz)
  (setq dtxt (getvar "DIMTXT")
        dscl (getvar "DIMSCALE")
        sz   (* (max dtxt 0.05) (max dscl 1.0) 0.85))

  (max sz 0.5)
)


(defun mc:ensure-layer (doc lname color / layers layerRes addRes)
  (setq layers   (vla-get-Layers doc)
        layerRes (vl-catch-all-apply 'vla-item (list layers lname)))

  (if (vl-catch-all-error-p layerRes)
    (progn
      (setq addRes (vl-catch-all-apply 'vla-add (list layers lname)))
      (if (not (vl-catch-all-error-p addRes))
        (vla-put-Color addRes color))
    )
    (vla-put-Color layerRes color)
  )
)


(defun mc:delete-layer (lname / doc layers layerRes delRes)
  (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object))
        layers   (vla-get-Layers doc)
        layerRes (vl-catch-all-apply 'vla-item (list layers lname)))

  (if (not (vl-catch-all-error-p layerRes))
    (progn
      (if (= (strcase (getvar "CLAYER")) (strcase lname))
        (setvar "CLAYER" "0"))

      (setq delRes (vl-catch-all-apply 'vla-delete (list layerRes)))

      (if (vl-catch-all-error-p delRes)
        (vl-catch-all-apply 'command
          (list "._-PURGE" "_La" lname "_No"))
      )
    )
  )
)


(defun mc:clear-qc-layers (/ ss i lname)
  (foreach lname (list "MC_PASS" "MC_ERRORS")
    (setq ss (ssget "X" (list (cons 8 lname))))

    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (entdel (ssname ss i))
          (setq i (1+ i))
        )
      )
    )

    (mc:delete-layer lname)
  )
)


(defun mc:place-balloon (px py bh isPass body / ins label layer color bw ed)
  ;; Green = pass
  ;; White = fail
  (setq ins (list px (+ py (* bh 0.75)) 0.0))

  (if isPass
    (setq layer "MC_PASS"   color 3)
    (setq layer "MC_ERRORS" color 7)
  )

  (setq label (strcat "{\\fArial|b1|i0;" body "}"))
  (setq bw (max (* (strlen label) bh 0.50) (* bh 3.0)))

  (setq ed
    (list
      (cons 0   "MTEXT")
      (cons 100 "AcDbEntity")
      (cons 8   layer)
      (cons 62  color)
      (cons 100 "AcDbMText")
      (cons 10  ins)
      (cons 40  bh)
      (cons 41  bw)
      (cons 71  1)
      (cons 72  1)
      (cons 1   label)
    )
  )

  (vl-catch-all-apply 'entmake (list ed))
)


(defun mc:make-label (inchVal expected actual /)
  ;; No diff shown.
  ;; Example:
  ;;   .03" exp .762mm got .8mm
  (strcat
    (mc:fmt inchVal) "\""
    "  exp " (mc:fmt expected) "mm"
    "  got " (mc:fmt actual) "mm"
  )
)


(defun mc:make-no-match-label (inchVal expected /)
  (strcat
    "NO METRIC MATCH  "
    (mc:fmt inchVal) "\""
    "  exp " (mc:fmt expected) "mm"
  )
)


;;; =====================================================================
;;; FILE PATH
;;; =====================================================================

(defun mc:get-current-dwg-folder (/ p)
  ;; Opens file dialog in the active metric drawing folder.
  (setq p (getvar "DWGPREFIX"))

  (if (or (not p) (= p ""))
    (setq p "")
  )

  p
)


;;; =====================================================================
;;; MAIN COMMAND
;;; =====================================================================

(defun c:metric_check
    (/ acadObj metricDoc metricDir inchFile openRes inchDoc
       metricDims inchDims metricTexts inchTexts
       mDimLen iDimLen mTxtLen iTxtLen
       remainingMetricDims remainingMetricTexts
       dimMarkers txtMarkers
       dimPass dimFail txtPass txtFail
       i j iEntry mEntry matchInfo
       iVal mVal expected vDiff pDist errPos
       iNums mNums iNum mNum txtBody anyFail
       balloonH errIdx)

  (vl-load-com)

  ;; ---------------------------------------------------------------
  ;; 1. Active metric document
  ;; ---------------------------------------------------------------
  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj))

  ;; ---------------------------------------------------------------
  ;; 2. Start file picker in metric DWG folder
  ;; ---------------------------------------------------------------
  (setq metricDir (mc:get-current-dwg-folder))

  ;; ---------------------------------------------------------------
  ;; 3. Read metric drawing data
  ;; ---------------------------------------------------------------
  (princ "\nReading metric drawing...")
  (setq metricDims  (mc:get-dims  metricDoc))
  (setq metricTexts (mc:get-texts metricDoc))

  (princ
    (strcat
      " "
      (itoa (length metricDims))  " dim(s), "
      (itoa (length metricTexts)) " text/mtext found."
    )
  )

  ;; ---------------------------------------------------------------
  ;; 4. Pick inch source file
  ;; ---------------------------------------------------------------
  (setq inchFile
    (getfiled
      "Select Inch Source Drawing"
      metricDir
      "dwg"
      4
    )
  )

  (if (not inchFile)
    (progn
      (princ "\nmetric_check: Cancelled.")
      (princ)
      (exit)
    )
  )

  ;; ---------------------------------------------------------------
  ;; 5. Open inch drawing read-only
  ;; ---------------------------------------------------------------
  (princ (strcat "\nOpening: " inchFile " ..."))

  (setq openRes
    (vl-catch-all-apply 'vla-open
      (list (vla-get-Documents acadObj) inchFile vlax-true)
    )
  )

  (if (vl-catch-all-error-p openRes)
    (progn
      (princ
        (strcat
          "\nERROR opening file: "
          (vl-catch-all-error-message openRes)
        )
      )
      (princ)
      (exit)
    )
  )

  (setq inchDoc openRes)

  ;; ---------------------------------------------------------------
  ;; 6. Read inch drawing data
  ;; ---------------------------------------------------------------
  (princ "\nReading inch drawing...")
  (setq inchDims  (mc:get-dims  inchDoc))
  (setq inchTexts (mc:get-texts inchDoc))

  (princ
    (strcat
      " "
      (itoa (length inchDims))  " dim(s), "
      (itoa (length inchTexts)) " text/mtext found."
    )
  )

  ;; ---------------------------------------------------------------
  ;; 7. Close inch drawing and reactivate metric drawing
  ;; ---------------------------------------------------------------
  (vl-catch-all-apply 'vla-close (list inchDoc vlax-false))
  (vla-Activate metricDoc)

  ;; ---------------------------------------------------------------
  ;; 8. Sort only for deterministic processing
  ;; ---------------------------------------------------------------
  (setq metricDims  (mc:sort-by-pos metricDims))
  (setq inchDims    (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts   (mc:sort-by-pos inchTexts))

  ;; ---------------------------------------------------------------
  ;; 9. Initialise counters
  ;; ---------------------------------------------------------------
  (setq dimMarkers nil
        txtMarkers nil
        dimPass    0
        dimFail    0
        txtPass    0
        txtFail    0)

  (setq mDimLen (length metricDims)
        iDimLen (length inchDims)
        mTxtLen (length metricTexts)
        iTxtLen (length inchTexts))

  (if (/= mDimLen iDimLen)
    (princ
      (strcat
        "\nWARNING: dimension count mismatch: "
        (itoa iDimLen) " inch vs "
        (itoa mDimLen) " metric."
      )
    )
  )

  (if (/= mTxtLen iTxtLen)
    (princ
      (strcat
        "\nWARNING: text count mismatch: "
        (itoa iTxtLen) " inch vs "
        (itoa mTxtLen) " metric."
      )
    )
  )

  ;; ---------------------------------------------------------------
  ;; 10. DIMENSION CHECK
  ;;
  ;; Logic:
  ;;   iVal     = visible inch dimension value
  ;;   expected = iVal x 25.4
  ;;   mVal     = visible metric dimension value
  ;;   compare mVal with expected
  ;;
  ;; Matching:
  ;;   value-first, position-second.
  ;; ---------------------------------------------------------------
  (setq remainingMetricDims metricDims)

  (princ
    (strcat "\nChecking " (itoa iDimLen) " inch dimension(s)...")
  )

  (setq i 0)

  (repeat iDimLen
    (setq iEntry (nth i inchDims))
    (setq iVal   (car iEntry))
    (setq expected (* iVal *mc-conv*))

    (setq matchInfo
      (mc:find-best-dim-match iEntry remainingMetricDims))

    (if matchInfo
      (progn
        (setq mEntry (car matchInfo)
              vDiff  (cadr matchInfo)
              pDist  (caddr matchInfo)
              mVal   (car mEntry)
              errPos (cadr mEntry))

        ;; If value is outside tolerance, it is a fail.
        ;; But because matching is value-first, this should no longer
        ;; randomly pair totally unrelated dimensions unless no better
        ;; metric value exists.
        (if (> vDiff *mc-tolerance*)
          (setq dimFail (1+ dimFail))
          (setq dimPass (1+ dimPass))
        )

        (setq dimMarkers
          (cons
            (list
              (<= vDiff *mc-tolerance*)
              (mc:make-label iVal expected mVal)
              errPos)
            dimMarkers))

        ;; Remove matched metric dimension so it cannot be reused.
        (setq remainingMetricDims
          (mc:remove-first mEntry remainingMetricDims))
      )
      (progn
        ;; No metric dimensions left.
        (setq dimFail (1+ dimFail))

        (setq dimMarkers
          (cons
            (list nil
                  (mc:make-no-match-label iVal expected)
                  (cadr iEntry))
            dimMarkers))
      )
    )

    (setq i (1+ i))
  )

  ;; Extra metric dimensions left unmatched.
  ;; These may be real extra dimensions or entities not present in inch file.
  (foreach mEntry remainingMetricDims
    (setq dimFail (1+ dimFail))

    (setq dimMarkers
      (cons
        (list nil
              (strcat
                "EXTRA METRIC DIM  got "
                (mc:fmt (car mEntry)) "mm")
              (cadr mEntry))
        dimMarkers))
  )


  ;; ---------------------------------------------------------------
  ;; 11. TEXT / MTEXT CHECK
  ;;
  ;; Only decimal values are converted.
  ;; This ignores note numbering 1. 2. 3.
  ;;
  ;; Handles:
  ;;   R.03  -> 0.03 inch
  ;;   R0.8  -> 0.8 mm
  ;;   (.03) -> 0.03 inch
  ;; ---------------------------------------------------------------
  (setq remainingMetricTexts metricTexts)

  (princ
    (strcat "\nChecking " (itoa iTxtLen) " inch text/mtext item(s)...")
  )

  (setq i 0)

  (repeat iTxtLen
    (setq iEntry (nth i inchTexts))
    (setq iNums  (mc:text-entry-decimals iEntry))

    ;; Only check inch text that actually contains decimal values.
    (if iNums
      (progn
        (setq matchInfo
          (mc:find-best-text-match iEntry remainingMetricTexts))

        (if matchInfo
          (progn
            (setq mEntry (car matchInfo)
                  vDiff  (cadr matchInfo)
                  pDist  (caddr matchInfo)
                  errPos (cadr mEntry)
                  mNums  (mc:text-entry-decimals mEntry))

            (setq j       0
                  txtBody nil
                  anyFail nil)

            (repeat (length iNums)
              (setq iNum     (nth j iNums)
                    mNum     (nth j mNums)
                    expected (* iNum *mc-conv*))

              (if (> (abs (- mNum expected)) *mc-tolerance*)
                (setq anyFail T
                      txtFail (1+ txtFail))
                (setq txtPass (1+ txtPass))
              )

              (setq txtBody
                (if txtBody
                  (strcat
                    txtBody
                    "  |  "
                    (mc:make-label iNum expected mNum))
                  (mc:make-label iNum expected mNum)
                )
              )

              (setq j (1+ j))
            )

            (if txtBody
              (setq txtMarkers
                (cons
                  (list (not anyFail) txtBody errPos)
                  txtMarkers))
            )

            ;; Remove matched metric text so it cannot be reused.
            (setq remainingMetricTexts
              (mc:remove-first mEntry remainingMetricTexts))
          )
          (progn
            ;; Inch text has decimal numbers but no metric text matched.
            (setq txtFail (1+ txtFail))

            (setq txtMarkers
              (cons
                (list nil
                      "NO MATCHING METRIC TEXT"
                      (cadr iEntry))
                txtMarkers))
          )
        )
      )
    )

    (setq i (1+ i))
  )

  ;; Extra metric text is only flagged if it has decimal numbers.
  (foreach mEntry remainingMetricTexts
    (if (mc:text-entry-decimals mEntry)
      (progn
        (setq txtFail (1+ txtFail))

        (setq txtMarkers
          (cons
            (list nil
                  "EXTRA METRIC TEXT WITH DECIMAL"
                  (cadr mEntry))
            txtMarkers))
      )
    )
  )


  ;; ---------------------------------------------------------------
  ;; 12. QC layers
  ;; ---------------------------------------------------------------
  (princ "\nUpdating QC layers...")
  (mc:clear-qc-layers)
  (mc:ensure-layer metricDoc "MC_PASS"   3)
  (mc:ensure-layer metricDoc "MC_ERRORS" 7)


  ;; ---------------------------------------------------------------
  ;; 13. Place balloons
  ;; ---------------------------------------------------------------
  (setq balloonH (mc:balloon-height)
        errIdx   1)

  ;; Dimension balloons
  (foreach m (reverse dimMarkers)
    (mc:place-balloon
      (car  (caddr m))
      (cadr (caddr m))
      balloonH
      (car m)
      (cadr m))

    (if (not (car m))
      (progn
        (princ
          (strcat
            "\n  ["
            (itoa errIdx)
            "] DIM FAIL: "
            (cadr m)))
        (setq errIdx (1+ errIdx))
      )
    )
  )

  ;; Text/MText balloons
  (foreach m (reverse txtMarkers)
    (mc:place-balloon
      (car  (caddr m))
      (cadr (caddr m))
      balloonH
      (car m)
      (cadr m))

    (if (not (car m))
      (progn
        (princ
          (strcat
            "\n  ["
            (itoa errIdx)
            "] TXT FAIL: "
            (cadr m)))
        (setq errIdx (1+ errIdx))
      )
    )
  )

  (if (zerop (+ dimFail txtFail))
    (princ "\nAll checks PASSED.")
  )

  (vla-Regen metricDoc 2)


  ;; ---------------------------------------------------------------
  ;; 14. Summary
  ;; ---------------------------------------------------------------
  (princ
    (strcat
      "\n"
      "--------------------------------------------\n"
      "METRIC CHECK DONE\n"
      "  Dimensions : "
      (itoa dimPass) " pass  " (itoa dimFail) " fail\n"
      "  Text/MText : "
      (itoa txtPass) " pass  " (itoa txtFail) " fail\n"
      "  Total errors: " (itoa (+ dimFail txtFail)) "\n"
      "  Pass balloons : Green layer MC_PASS\n"
      "  Fail balloons : White layer MC_ERRORS\n"
      "  Label format  : inch / expected mm / got mm\n"
      "--------------------------------------------"
    )
  )

  (princ)
)


;;; =====================================================================
;;; CLEAR COMMAND
;;; =====================================================================

(defun c:metric_clear (/)
  (vl-load-com)

  (mc:clear-qc-layers)

  (vla-Regen
    (vla-get-ActiveDocument (vlax-get-acad-object))
    2)

  (princ "\nMC_PASS and MC_ERRORS balloons and layers removed.")
  (princ)
)


(princ "\nMETRIC_CHECK.LSP v9 loaded.")
(princ "\n  METRIC_CHECK -- run check, green=pass  white=fail")
(princ "\n  METRIC_CLEAR -- erase all QC balloons and layers")
(princ)