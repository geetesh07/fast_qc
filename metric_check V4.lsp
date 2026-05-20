;;; =====================================================================
;;; METRIC_CHECK.LSP  v7
;;; Commands: METRIC_CHECK   -- run the check, draw pass/fail balloons
;;;           METRIC_CLEAR   -- erase all QC balloons + layers
;;;
;;; With the metric drawing open, type METRIC_CHECK.
;;; The inch-file picker now opens in the current metric drawing folder.
;;;
;;; PASS entities get a green balloon  (layer MC_PASS)
;;; FAIL entities get a white balloon  (layer MC_ERRORS)
;;;
;;; Conversion rule:  metric = inch x 25.4   tolerance +/- 0.1 mm
;;; Angular dims are excluded.
;;;
;;; v7 fixes:
;;;   1. getfiled opens in metric drawing folder.
;;;   2. TEXT/MTEXT sequence numbers like 1. 2. 3. are ignored.
;;;   3. DIMENSION displayed value uses LinearScaleFactor / DIMLFAC.
;;;      This helps when values are halved/doubled by dim style scale.
;;;   4. Dimension/text matching compares metric position to inch position x 25.4.
;;;   5. Labels show max 4 decimals and trim trailing zeroes.
;;;   6. Labels do not show diff anymore.
;;;   7. Fail balloons are white.
;;; =====================================================================

(vl-load-com)


;;; -------------------------------------------------------------------
;;; Global settings
;;; -------------------------------------------------------------------
(setq *mc-conv* 25.4)
(setq *mc-tolerance* 0.1)

;;; Matching distance in metric drawing units.
;;; If drawings are offset from each other, increase these values.
(setq *mc-dim-match-limit* 25.0)
(setq *mc-text-match-limit* 25.0)


;;; -------------------------------------------------------------------
;;; mc:is-digit  --  T if character C is ASCII 0-9
;;; -------------------------------------------------------------------
(defun mc:is-digit (c)
  (and c
       (= (type c) 'STR)
       (= (strlen c) 1)
       (>= (ascii c) 48)
       (<= (ascii c) 57))
)


;;; -------------------------------------------------------------------
;;; mc:rtrim0
;;; Remove trailing zeroes and trailing decimal point.
;;; Example:
;;;   "25.4000" -> "25.4"
;;;   "25.0000" -> "25"
;;; -------------------------------------------------------------------
(defun mc:rtrim0 (s /)
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


;;; -------------------------------------------------------------------
;;; mc:fmt
;;; Format value to 4 decimals, then trim unnecessary zeroes.
;;; -------------------------------------------------------------------
(defun mc:fmt (val /)
  (mc:rtrim0 (rtos val 2 4))
)


;;; -------------------------------------------------------------------
;;; mc:distance  --  2D distance between two XY points
;;; -------------------------------------------------------------------
(defun mc:distance (p1 p2 / dx dy)
  (setq dx (- (car p1) (car p2))
        dy (- (cadr p1) (cadr p2)))
  (sqrt (+ (* dx dx) (* dy dy)))
)


;;; -------------------------------------------------------------------
;;; mc:scale-point
;;; Scale an XY point.
;;; -------------------------------------------------------------------
(defun mc:scale-point (p sc /)
  (list (* (car p) sc) (* (cadr p) sc))
)


;;; -------------------------------------------------------------------
;;; mc:find-closest-scaled
;;; Finds closest item in metric list to target position scaled by SCALE.
;;; TARGET format: (value (x y))
;;; ITEM format  : (value (x y))
;;; -------------------------------------------------------------------
(defun mc:find-closest-scaled (target lst scale / best bestd item d targetPos)
  (setq best nil
        bestd nil)

  (if (and target (cadr target))
    (setq targetPos (mc:scale-point (cadr target) scale))
  )

  (if targetPos
    (foreach item lst
      (if (and item (cadr item))
        (progn
          (setq d (mc:distance targetPos (cadr item)))
          (if (or (not bestd) (< d bestd))
            (setq best item
                  bestd d)
          )
        )
      )
    )
  )

  (if best
    (list best bestd)
    nil
  )
)


;;; -------------------------------------------------------------------
;;; mc:find-best-match
;;; Tries scaled-position match first using 25.4.
;;; Also checks unscaled position and uses whichever is closer.
;;; This protects cases where drawings are not physically scaled.
;;; Returns: (matchedItem distance usedScale)
;;; -------------------------------------------------------------------
(defun mc:find-best-match (target lst / a b)
  (setq a (mc:find-closest-scaled target lst *mc-conv*))
  (setq b (mc:find-closest-scaled target lst 1.0))

  (cond
    ((and a b)
     (if (< (cadr a) (cadr b))
       (list (car a) (cadr a) *mc-conv*)
       (list (car b) (cadr b) 1.0)
     )
    )
    (a (list (car a) (cadr a) *mc-conv*))
    (b (list (car b) (cadr b) 1.0))
    (T nil)
  )
)


;;; -------------------------------------------------------------------
;;; mc:remove-first
;;; Removes first matching item from a list.
;;; Used so one metric entity is not matched repeatedly.
;;; -------------------------------------------------------------------
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


;;; -------------------------------------------------------------------
;;; mc:safearray-point
;;; Convert variant safearray point to normal AutoLISP XY list.
;;; -------------------------------------------------------------------
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


;;; -------------------------------------------------------------------
;;; mc:linear-dim-p
;;; T for non-angular dimensions.
;;; -------------------------------------------------------------------
(defun mc:linear-dim-p (oname)
  (and (wcmatch oname "*Dimension*")
       (not (wcmatch oname "*Angular*")))
)


;;; -------------------------------------------------------------------
;;; mc:get-dim-lfac
;;; Get dimension LinearScaleFactor.
;;; If object property fails, fallback to current DIMLFAC in that document.
;;; If all fails, use 1.0.
;;; -------------------------------------------------------------------
(defun mc:get-dim-lfac (obj / lfacRes val)
  (setq lfacRes
    (vl-catch-all-apply 'vla-get-LinearScaleFactor (list obj)))

  (cond
    ((not (vl-catch-all-error-p lfacRes))
     (if (numberp lfacRes) lfacRes 1.0)
    )
    (T
     1.0
    )
  )
)


;;; -------------------------------------------------------------------
;;; mc:get-displayed-dim-value
;;; AutoCAD measurement gives raw geometric length.
;;; Displayed dimension value can be affected by LinearScaleFactor / DIMLFAC.
;;; This returns measurement x linear scale factor.
;;; -------------------------------------------------------------------
(defun mc:get-displayed-dim-value (obj / measRes lfac)
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


;;; -------------------------------------------------------------------
;;; mc:dim-geometry-point
;;; Try to get stable geometry point for a dimension.
;;; Priority:
;;;   1. ExtLine1Point + ExtLine2Point midpoint
;;;   2. TextPosition fallback
;;; -------------------------------------------------------------------
(defun mc:dim-geometry-point (obj / p1Res p2Res p1 p2 txtRes txtPos)
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
      (setq txtRes (vl-catch-all-apply 'vla-get-TextPosition (list obj)))
      (if (not (vl-catch-all-error-p txtRes))
        (setq txtPos (mc:safearray-point txtRes))
      )
      txtPos
    )
  )
)


;;; -------------------------------------------------------------------
;;; mc:get-dims
;;; Collect dimension entities from DOC model space.
;;; Returns list of:
;;;   (displayedValue (x y))
;;; -------------------------------------------------------------------
(defun mc:get-dims (doc / ms cnt i obj oname dimVal pos result)
  (setq result nil
        ms     (vla-get-ModelSpace doc)
        cnt    (vla-get-Count ms)
        i      0)

  (while (< i cnt)
    (setq obj   (vla-item ms i)
          oname (vla-get-ObjectName obj))

    (if (mc:linear-dim-p oname)
      (progn
        (setq dimVal (mc:get-displayed-dim-value obj))
        (setq pos    (mc:dim-geometry-point obj))

        (if (and dimVal pos)
          (setq result
            (cons (list dimVal pos) result))
        )
      )
    )

    (setq i (1+ i))
  )

  result
)


;;; -------------------------------------------------------------------
;;; mc:strip-mtext
;;; State-machine stripper for raw MTEXT strings.
;;; Removes formatting blocks and common escapes.
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
      ((= c "{")
       (setq depth (1+ depth))
       (if (and (< i len) (= (substr s (1+ i) 1) "\\"))
         (setq skipSemi T))
      )

      ((= c "}")
       (if (> depth 0) (setq depth (- depth 1)))
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
       (setq i (+ i 2))
      )

      (T
       (setq res (strcat res c)))
    )

    (setq i (1+ i))
  )

  res
)


;;; -------------------------------------------------------------------
;;; mc:extract-decimal-numbers
;;;
;;; IMPORTANT:
;;; This function only returns real decimal values that should be converted.
;;;
;;; It ignores:
;;;   1.
;;;   2.
;;;   3.
;;;   10.
;;;   25
;;;
;;; It accepts:
;;;   1.25
;;;   .030
;;;   0.8
;;;   9.250
;;;
;;; Returns list of numeric values only.
;;; -------------------------------------------------------------------
(defun mc:extract-decimal-numbers (str / result i len c nxt prv token started hadDot hadDigitAfterDot)
  (setq result nil
        len    (strlen str)
        i      1)

  (while (<= i len)
    (setq c (substr str i 1))

    (cond
      ;; Start with digit.
      ((mc:is-digit c)
       (setq token c
             started T
             hadDot nil
             hadDigitAfterDot nil
             i (1+ i))

       (while (and (<= i len)
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
            ;; only allow one decimal dot
            (if hadDot
              (progn
                ;; stop token on second dot
                (setq i len)
              )
              (progn
                (setq token (strcat token c))
                (setq hadDot T)
              )
            )
           )
         )

         (setq i (1+ i))
       )

       ;; Only accept decimals with digit after dot.
       ;; This skips note numbers like "1.".
       (if (and hadDot hadDigitAfterDot)
         (setq result (cons (atof token) result))
       )

       ;; step back because outer loop will increment
       (setq i (1- i))
      )

      ;; Start with . followed by digit, example .030
      ((and (= c ".")
            (< i len)
            (mc:is-digit (substr str (1+ i) 1)))
       (setq token "0."
             hadDigitAfterDot nil
             i (+ i 1))

       (while (and (<= i len)
                   (mc:is-digit (substr str i 1)))
         (setq token (strcat token (substr str i 1)))
         (setq hadDigitAfterDot T)
         (setq i (1+ i))
       )

       (if hadDigitAfterDot
         (setq result (cons (atof token) result))
       )

       (setq i (1- i))
      )
    )

    (setq i (1+ i))
  )

  (reverse result)
)


;;; -------------------------------------------------------------------
;;; mc:get-texts
;;; Collect TEXT and MTEXT entities from DOC model space.
;;; Returns list of:
;;;   (plainString (x y))
;;; -------------------------------------------------------------------
(defun mc:get-texts (doc / ms cnt i obj oname txtRes posRes pos result)
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
               (cons (list txtRes (list (car pos) (cadr pos))) result))
           )
         )
       )
      )

      ((wcmatch oname "AcDbMText")
       (setq txtRes (vl-catch-all-apply 'vla-get-TextString     (list obj)))
       (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))

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
               (cons (list (mc:strip-mtext txtRes)
                           (list (car pos) (cadr pos)))
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
;;; Sort lists by X then Y for deterministic processing.
;;; Matching itself is nearest-entity based.
;;; -------------------------------------------------------------------
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


;;; -------------------------------------------------------------------
;;; mc:balloon-height
;;; Return sensible MTEXT height.
;;; -------------------------------------------------------------------
(defun mc:balloon-height (/ dtxt dscl sz)
  (setq dtxt (getvar "DIMTXT")
        dscl (getvar "DIMSCALE")
        sz   (* (max dtxt 0.05) (max dscl 1.0) 0.85))
  (max sz 0.5)
)


;;; -------------------------------------------------------------------
;;; mc:ensure-layer
;;; Create/update layer color.
;;; -------------------------------------------------------------------
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


;;; -------------------------------------------------------------------
;;; mc:delete-layer
;;; Remove layer LNAME from current drawing.
;;; -------------------------------------------------------------------
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


;;; -------------------------------------------------------------------
;;; mc:clear-qc-layers
;;; Delete all entities on MC_PASS and MC_ERRORS, then delete layers.
;;; -------------------------------------------------------------------
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


;;; -------------------------------------------------------------------
;;; mc:place-balloon
;;; Place MTEXT balloon.
;;; PASS layer: MC_PASS    color 3 green
;;; FAIL layer: MC_ERRORS  color 7 white
;;; -------------------------------------------------------------------
(defun mc:place-balloon (px py bh isPass body / ins label layer color bw ed)
  (setq ins (list px (+ py (* bh 0.75)) 0.0))

  (if isPass
    (setq layer "MC_PASS"   color 3)
    (setq layer "MC_ERRORS" color 7)
  )

  (setq label (strcat "{\\fArial|b1|i0;" body "}"))

  (setq bw (max (* (strlen label) bh 0.50) (* bh 3.0)))

  (setq ed
    (list (cons 0   "MTEXT")
          (cons 100 "AcDbEntity")
          (cons 8   layer)
          (cons 62  color)
          (cons 100 "AcDbMText")
          (cons 10  ins)
          (cons 40  bh)
          (cons 41  bw)
          (cons 71  1)
          (cons 72  1)
          (cons 1   label)))

  (vl-catch-all-apply 'entmake (list ed))
)


;;; -------------------------------------------------------------------
;;; mc:make-label
;;; Label format without diff:
;;;   inch"  exp xmm  got ymm
;;; -------------------------------------------------------------------
(defun mc:make-label (inchVal expected actual /)
  (strcat
    (mc:fmt inchVal) "\""
    "  exp " (mc:fmt expected) "mm"
    "  got " (mc:fmt actual) "mm"
  )
)


;;; ===================================================================
;;; c:metric_check  --  main command
;;; ===================================================================
(defun c:metric_check
    (/ acadObj metricDoc metricDir inchFile openRes inchDoc
       metricDims inchDims metricTexts inchTexts
       mDimLen iDimLen mTxtLen iTxtLen
       i j iEntry mEntry matchInfo matchDist usedScale
       iVal mVal expected diff errPos
       iStr mStr iNums mNums iNLen iNum mNum
       dimMarkers txtMarkers txtBody anyFail
       remainingMetricDims remainingMetricTexts
       balloonH errIdx
       dimPass dimFail txtPass txtFail
       txtDecimalPairs)

  (vl-load-com)

  ;;----------------------------------------------------------------
  ;; 1. Active metric document
  ;;----------------------------------------------------------------
  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj))

  ;;----------------------------------------------------------------
  ;; 2. Use metric drawing folder for inch-file dialog
  ;;----------------------------------------------------------------
  (setq metricDir (getvar "DWGPREFIX"))
  (if (or (not metricDir) (= metricDir ""))
    (setq metricDir "")
  )

  ;;----------------------------------------------------------------
  ;; 3. Read metric drawing first
  ;;----------------------------------------------------------------
  (princ "\nReading metric drawing...")
  (setq metricDims  (mc:get-dims  metricDoc))
  (setq metricTexts (mc:get-texts metricDoc))

  (princ
    (strcat " "
            (itoa (length metricDims))  " dim(s), "
            (itoa (length metricTexts)) " text/mtext found."))

  ;;----------------------------------------------------------------
  ;; 4. Pick inch source drawing.
  ;;    Starts in current metric drawing path.
  ;;----------------------------------------------------------------
  (setq inchFile
    (getfiled
      "Select Inch Source Drawing"
      (strcat metricDir "*.dwg")
      "dwg"
      4))

  (if (not inchFile)
    (progn
      (princ "\nmetric_check: Cancelled.")
      (princ)
      (exit)
    )
  )

  ;;----------------------------------------------------------------
  ;; 5. Open inch drawing read-only
  ;;----------------------------------------------------------------
  (princ (strcat "\nOpening: " inchFile " ..."))

  (setq openRes
    (vl-catch-all-apply 'vla-open
      (list (vla-get-Documents acadObj) inchFile vlax-true)))

  (if (vl-catch-all-error-p openRes)
    (progn
      (princ
        (strcat "\nERROR opening file: "
                (vl-catch-all-error-message openRes)))
      (princ)
      (exit)
    )
  )

  (setq inchDoc openRes)

  ;;----------------------------------------------------------------
  ;; 6. Read inch drawing data
  ;;----------------------------------------------------------------
  (princ "\nReading inch drawing...")
  (setq inchDims  (mc:get-dims  inchDoc))
  (setq inchTexts (mc:get-texts inchDoc))

  (princ
    (strcat " "
            (itoa (length inchDims))  " dim(s), "
            (itoa (length inchTexts)) " text/mtext found."))

  ;;----------------------------------------------------------------
  ;; 7. Close inch drawing and restore metric doc
  ;;----------------------------------------------------------------
  (vl-catch-all-apply 'vla-close (list inchDoc vlax-false))
  (vla-Activate metricDoc)

  ;;----------------------------------------------------------------
  ;; 8. Sort for deterministic processing
  ;;----------------------------------------------------------------
  (setq metricDims  (mc:sort-by-pos metricDims))
  (setq inchDims    (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts   (mc:sort-by-pos inchTexts))

  ;;----------------------------------------------------------------
  ;; 9. Initialise
  ;;----------------------------------------------------------------
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
      (strcat "\nWARNING: dimension count mismatch: "
              (itoa iDimLen) " inch vs "
              (itoa mDimLen) " metric."))
  )

  (if (/= mTxtLen iTxtLen)
    (princ
      (strcat "\nWARNING: text count mismatch: "
              (itoa iTxtLen) " inch vs "
              (itoa mTxtLen) " metric."))
  )

  ;;----------------------------------------------------------------
  ;; 10. DIMENSION CHECK
  ;;     Match metric dimension to inch dimension using:
  ;;       metric position ~= inch position x 25.4
  ;;     Dimension value uses measurement x LinearScaleFactor.
  ;;----------------------------------------------------------------
  (setq remainingMetricDims metricDims)

  (princ
    (strcat "\nChecking " (itoa iDimLen) " inch dimension(s)..."))

  (setq i 0)
  (repeat iDimLen
    (setq iEntry (nth i inchDims))
    (setq matchInfo (mc:find-best-match iEntry remainingMetricDims))

    (if matchInfo
      (progn
        (setq mEntry    (car matchInfo)
              matchDist (cadr matchInfo)
              usedScale (caddr matchInfo))

        (if (> matchDist *mc-dim-match-limit*)
          (progn
            ;; Suspicious/far match.
            (setq dimFail (1+ dimFail))
            (setq errPos (cadr mEntry))
            (setq dimMarkers
              (cons
                (list nil
                      (strcat "UNMATCHED DIM  inch "
                              (mc:fmt (car iEntry)) "\"")
                      errPos)
                dimMarkers))
          )
          (progn
            (setq iVal     (car iEntry)
                  mVal     (car mEntry)
                  errPos   (cadr mEntry)
                  expected (* iVal *mc-conv*)
                  diff     (abs (- mVal expected)))

            (if (> diff *mc-tolerance*)
              (setq dimFail (1+ dimFail))
              (setq dimPass (1+ dimPass))
            )

            (setq dimMarkers
              (cons
                (list
                  (<= diff *mc-tolerance*)
                  (mc:make-label iVal expected mVal)
                  errPos)
                dimMarkers))

            ;; Remove matched metric dimension so it is not reused.
            (setq remainingMetricDims
              (mc:remove-first mEntry remainingMetricDims))
          )
        )
      )
      (progn
        (setq dimFail (1+ dimFail))
      )
    )

    (setq i (1+ i))
  )

  ;; Extra metric dimensions.
  (foreach mEntry remainingMetricDims
    (setq dimFail (1+ dimFail))
    (setq dimMarkers
      (cons
        (list nil
              (strcat "EXTRA METRIC DIM  got "
                      (mc:fmt (car mEntry)) "mm")
              (cadr mEntry))
        dimMarkers))
  )


  ;;----------------------------------------------------------------
  ;; 11. TEXT / MTEXT CHECK
  ;;     Only decimal numbers are converted.
  ;;     Sequence numbers like 1. 2. 3. are ignored.
  ;;----------------------------------------------------------------
  (setq remainingMetricTexts metricTexts)

  (princ
    (strcat "\nChecking " (itoa iTxtLen) " inch text/mtext item(s)..."))

  (setq i 0)
  (repeat iTxtLen
    (setq iEntry (nth i inchTexts))
    (setq matchInfo (mc:find-best-match iEntry remainingMetricTexts))

    (if matchInfo
      (progn
        (setq mEntry    (car matchInfo)
              matchDist (cadr matchInfo)
              usedScale (caddr matchInfo))

        (if (> matchDist *mc-text-match-limit*)
          (progn
            (setq txtFail (1+ txtFail))
            (setq txtMarkers
              (cons
                (list nil
                      "UNMATCHED TEXT"
                      (cadr mEntry))
                txtMarkers))
          )
          (progn
            (setq iStr   (car  iEntry)
                  mStr   (car  mEntry)
                  errPos (cadr mEntry))

            ;; Extract decimal-only numbers.
            ;; This avoids notes like:
            ;;   1.
            ;;   2.
            ;;   3.
            (setq iNums (mc:extract-decimal-numbers iStr))
            (setq mNums (mc:extract-decimal-numbers mStr))

            ;; Only check text entities that have decimal values.
            (if (or iNums mNums)
              (progn
                (if (= (length iNums) (length mNums))
                  (progn
                    (setq iNLen   (length iNums)
                          j       0
                          txtBody nil
                          anyFail nil)

                    (repeat iNLen
                      (setq iNum     (nth j iNums)
                            mNum     (nth j mNums)
                            expected (* iNum *mc-conv*)
                            diff     (abs (- mNum expected)))

                      (if (> diff *mc-tolerance*)
                        (setq anyFail T
                              txtFail (1+ txtFail))
                        (setq txtPass (1+ txtPass))
                      )

                      (setq txtBody
                        (if txtBody
                          (strcat txtBody
                                  "  |  "
                                  (mc:make-label iNum expected mNum))
                          (mc:make-label iNum expected mNum)))

                      (setq j (1+ j))
                    )

                    (if txtBody
                      (setq txtMarkers
                        (cons
                          (list (not anyFail) txtBody errPos)
                          txtMarkers))
                    )
                  )
                  (progn
                    ;; Number count mismatch inside text.
                    ;; This is suspicious, so mark it.
                    (setq txtFail (1+ txtFail))
                    (setq txtMarkers
                      (cons
                        (list nil
                              (strcat "TEXT NUMBER COUNT MISMATCH  inch "
                                      (itoa (length iNums))
                                      " metric "
                                      (itoa (length mNums)))
                              errPos)
                        txtMarkers))
                  )
                )
              )
            )

            ;; Remove matched metric text so it is not reused.
            (setq remainingMetricTexts
              (mc:remove-first mEntry remainingMetricTexts))
          )
        )
      )
    )

    (setq i (1+ i))
  )

  ;; Extra metric text is not always an error because many notes have no decimals.
  ;; But if it contains decimal values, flag it.
  (foreach mEntry remainingMetricTexts
    (setq txtDecimalPairs (mc:extract-decimal-numbers (car mEntry)))
    (if txtDecimalPairs
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


  ;;----------------------------------------------------------------
  ;; 12. QC layers
  ;;----------------------------------------------------------------
  (princ "\nUpdating QC layers...")
  (mc:clear-qc-layers)
  (mc:ensure-layer metricDoc "MC_PASS"   3)
  (mc:ensure-layer metricDoc "MC_ERRORS" 7)


  ;;----------------------------------------------------------------
  ;; 13. Place balloons
  ;;----------------------------------------------------------------
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
        (princ (strcat "\n  [" (itoa errIdx) "] DIM FAIL: " (cadr m)))
        (setq errIdx (1+ errIdx))
      )
    )
  )

  ;; Text / MText balloons
  (foreach m (reverse txtMarkers)
    (mc:place-balloon
      (car  (caddr m))
      (cadr (caddr m))
      balloonH
      (car m)
      (cadr m))

    (if (not (car m))
      (progn
        (princ (strcat "\n  [" (itoa errIdx) "] TXT FAIL: " (cadr m)))
        (setq errIdx (1+ errIdx))
      )
    )
  )

  (if (zerop (+ dimFail txtFail))
    (princ "\nAll checks PASSED.")
  )

  ;; Regenerate so balloons appear.
  (vla-Regen metricDoc 2)


  ;;----------------------------------------------------------------
  ;; 14. Summary
  ;;----------------------------------------------------------------
  (princ
    (strcat "\n"
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
            "--------------------------------------------"))

  (princ)
)


;;; ===================================================================
;;; c:metric_clear  --  erase all markers and delete the layers
;;; ===================================================================
(defun c:metric_clear (/)
  (vl-load-com)
  (mc:clear-qc-layers)
  (vla-Regen
    (vla-get-ActiveDocument (vlax-get-acad-object))
    2)
  (princ "\nMC_PASS and MC_ERRORS balloons and layers removed.")
  (princ)
)


(princ "\nMETRIC_CHECK.LSP v7 loaded.")
(princ "\n  METRIC_CHECK -- run check, green=pass  white=fail")
(princ "\n  METRIC_CLEAR -- erase all QC balloons and layers")
(princ)