;;; =====================================================================
;;; METRIC_CHECK.LSP  v6
;;; Commands: METRIC_CHECK   -- run the check, draw pass/fail balloons
;;;           METRIC_CLEAR   -- erase all QC balloons + layers
;;;
;;; With the metric drawing open, type METRIC_CHECK.
;;; Pick the inch source drawing from the file dialog.
;;;
;;; PASS entities get a green balloon  (layer MC_PASS)
;;; FAIL entities get a white balloon showing got/expected/actual
;;;
;;; Conversion rule:  metric = inch x 25.4   tolerance +/- 0.1 mm
;;; Angular dims are excluded.
;;; Integers in text (note numbers, counts) are skipped.
;;;
;;; v6 changes:
;;;   - Dimension matching no longer depends only on sorted text position.
;;;   - Dimension reference point uses extension-line midpoint where possible.
;;;   - Nearest-position matching is used for dimensions and text.
;;;   - Fail balloons are white, color 7.
;;;   - Values are shown with 6 decimals for exact checking.
;;; =====================================================================

(vl-load-com)


;;; -------------------------------------------------------------------
;;; mc:is-digit  --  T if character C is ASCII 0-9
;;; -------------------------------------------------------------------
(defun mc:is-digit (c)
  (and (>= (ascii c) 48) (<= (ascii c) 57))
)


;;; -------------------------------------------------------------------
;;; mc:fmt  --  format real VAL to PREC decimal places
;;; -------------------------------------------------------------------
(defun mc:fmt (val prec /)
  (rtos val 2 prec)
)


;;; -------------------------------------------------------------------
;;; mc:fmt6  --  exact display format for QC labels
;;; -------------------------------------------------------------------
(defun mc:fmt6 (val /)
  (rtos val 2 6)
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
;;; mc:find-closest
;;; Finds closest item in LST to TARGET by comparing cadr positions.
;;; TARGET format: (value (x y))
;;; ITEM format  : (value (x y))
;;; -------------------------------------------------------------------
(defun mc:find-closest (target lst / best bestd item d)
  (setq best nil
        bestd nil)
  (foreach item lst
    (if (and (cadr target) (cadr item))
      (progn
        (setq d (mc:distance (cadr target) (cadr item)))
        (if (or (not bestd) (< d bestd))
          (setq best item
                bestd d)
        )
      )
    )
  )
  best
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
    (if (and (not removed) (equal x item 1e-9))
      (setq removed T)
      (setq result (cons x result))
    )
  )
  (reverse result)
)


;;; -------------------------------------------------------------------
;;; mc:strip-mtext
;;; State-machine stripper for raw MTEXT strings.
;;; Removes {\format;...} blocks, \P \~ escapes, %%c %%d %%p symbols.
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
;;; mc:extract-numbers
;;; Parse STR, return list of  (numericValue isDecimal)  pairs.
;;; isDecimal = T when the token contained "."
;;; Pure integers return isDecimal = nil.
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
      ((mc:is-digit c)
       (setq numStr (strcat numStr c)
             inNum  T)
      )
      ((= c ".")
       (cond
         (hadDot
          (if (and inNum (> (strlen numStr) 0))
            (setq result (cons (list (atof numStr) T) result)))
          (setq numStr "" inNum nil hadDot nil)
         )
         ((or inNum
              (and (<= (1+ i) len)
                   (mc:is-digit (substr str (1+ i) 1))))
          (setq numStr (strcat numStr c)
                hadDot T
                inNum  T)
         )
         (T
          (if (and inNum (> (strlen numStr) 0))
            (setq result (cons (list (atof numStr) nil) result)))
          (setq numStr "" inNum nil hadDot nil)
         )
       )
      )
      (T
       (if (and inNum (> (strlen numStr) 0))
         (setq result (cons (list (atof numStr) hadDot) result)))
       (setq numStr "" inNum nil hadDot nil)
      )
    )
    (setq i (1+ i))
  )
  (if (and inNum (> (strlen numStr) 0))
    (setq result (cons (list (atof numStr) hadDot) result)))
  (reverse result)
)


;;; -------------------------------------------------------------------
;;; mc:linear-dim-p  --  T for linear/radial dims, nil for angular
;;; -------------------------------------------------------------------
(defun mc:linear-dim-p (oname)
  (and (wcmatch oname "*Dimension*")
       (not (wcmatch oname "*Angular*")))
)


;;; -------------------------------------------------------------------
;;; mc:safearray-point
;;; Convert variant safearray point to normal AutoLISP list.
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
;;; mc:dim-geometry-point
;;; Try to get stable geometry point for a dimension.
;;; Priority:
;;;   1. ExtLine1Point + ExtLine2Point midpoint
;;;   2. TextPosition fallback
;;; -------------------------------------------------------------------
(defun mc:dim-geometry-point (obj / p1Res p2Res p1 p2 txtRes txtPos)
  (setq p1Res (vl-catch-all-apply 'vla-get-ExtLine1Point (list obj)))
  (setq p2Res (vl-catch-all-apply 'vla-get-ExtLine2Point (list obj)))

  (if (and (not (vl-catch-all-error-p p1Res))
           (not (vl-catch-all-error-p p2Res)))
    (progn
      (setq p1 (mc:safearray-point p1Res))
      (setq p2 (mc:safearray-point p2Res))
      (if (and p1 p2)
        (list (/ (+ (car p1) (car p2)) 2.0)
              (/ (+ (cadr p1) (cadr p2)) 2.0))
        nil
      )
    )
    nil
  )

  ;; If extension points failed, fallback to text position.
  (if (not (and p1 p2))
    (progn
      (setq txtRes (vl-catch-all-apply 'vla-get-TextPosition (list obj)))
      (if (not (vl-catch-all-error-p txtRes))
        (setq txtPos (mc:safearray-point txtRes))
      )
      txtPos
    )
    (list (/ (+ (car p1) (car p2)) 2.0)
          (/ (+ (cadr p1) (cadr p2)) 2.0))
  )
)


;;; -------------------------------------------------------------------
;;; mc:get-dims
;;; Collect dimension entities from DOC model space.
;;; Returns list of  (measurement (x y))
;;; Uses stable dimension geometry point where possible.
;;; -------------------------------------------------------------------
(defun mc:get-dims (doc / ms cnt i obj oname measRes pos result)
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
        (setq pos (mc:dim-geometry-point obj))
        (if (and (not (vl-catch-all-error-p measRes))
                 pos)
          (setq result
            (cons (list measRes pos) result))
        )
      )
    )
    (setq i (1+ i))
  )
  result
)


;;; -------------------------------------------------------------------
;;; mc:get-texts
;;; Collect TEXT and MTEXT entities from DOC model space.
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
               (cons (list txtRes (list (car pos) (cadr pos)))
                     result))
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
;;; Sort  (anything (x y))  lists by X then Y.
;;; Kept for reporting fallback, but matching now uses nearest entity.
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
;;; Return a sensible MTEXT height for QC balloons.
;;; Based on DIMTXT x DIMSCALE so it scales with the drawing.
;;; -------------------------------------------------------------------
(defun mc:balloon-height (/ dtxt dscl sz)
  (setq dtxt (getvar "DIMTXT")
        dscl (getvar "DIMSCALE")
        sz   (* (max dtxt 0.05) (max dscl 1.0) 0.85))
  (max sz 0.5)
)


;;; -------------------------------------------------------------------
;;; mc:ensure-layer
;;; Create layer LNAME in DOC with COLOR if it does not exist.
;;; If it already exists, just confirm the color.
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
;;; Remove layer LNAME from the current drawing.
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
;;; Delete all entities on MC_PASS and MC_ERRORS, then delete both layers.
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
;;; Place an MTEXT balloon just above position (PX PY).
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

  (setq bw (max (* (strlen label) bh 0.55) (* bh 3.0)))

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
;;; mc:make-dim-label
;;; Body format:
;;;   inch"  expected_mm  actual_mm  diff
;;; -------------------------------------------------------------------
(defun mc:make-dim-label (inchVal expected actual diff /)
  (strcat
    (mc:fmt6 inchVal) "\""
    "  exp " (mc:fmt6 expected) "mm"
    "  got " (mc:fmt6 actual) "mm"
    "  diff " (mc:fmt6 diff) "mm"
  )
)


;;; -------------------------------------------------------------------
;;; mc:make-text-label-line
;;; Body format for text decimal pair.
;;; -------------------------------------------------------------------
(defun mc:make-text-label-line (inchVal expected actual diff /)
  (strcat
    (mc:fmt6 inchVal) "\""
    "  exp " (mc:fmt6 expected) "mm"
    "  got " (mc:fmt6 actual) "mm"
    "  diff " (mc:fmt6 diff) "mm"
  )
)


;;; ===================================================================
;;; c:metric_check  --  main command
;;; ===================================================================
(defun c:metric_check
    (/ acadObj metricDoc metricMs inchFile openRes inchDoc
       metricDims inchDims mDimLen iDimLen dimN
       metricTexts inchTexts mTxtLen iTxtLen txtN
       i j iVal mVal expected diff tolerance
       iEntry mEntry iStr mStr iNums mNums iNLen
       iNumPair mNumPair iNum mNum isDecimal
       dimMarkers txtMarkers txtBody anyFail
       errPos balloonH errIdx
       dimPass dimFail txtPass txtFail
       unmatchedDim unmatchedTxt
       remainingMetricDims remainingMetricTexts
       matchDist dimMatchLimit txtMatchLimit)

  (vl-load-com)

  ;;----------------------------------------------------------------
  ;; 1. Store metric document reference
  ;;----------------------------------------------------------------
  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj))

  ;;----------------------------------------------------------------
  ;; 2. Read metric drawing data FIRST
  ;;----------------------------------------------------------------
  (princ "\nReading metric drawing...")
  (setq metricDims  (mc:get-dims  metricDoc))
  (setq metricTexts (mc:get-texts metricDoc))
  (princ
    (strcat " "
            (itoa (length metricDims))  " dim(s), "
            (itoa (length metricTexts)) " text/mtext found."))

  ;;----------------------------------------------------------------
  ;; 3. File picker for inch source drawing
  ;;----------------------------------------------------------------
  (setq inchFile (getfiled "Select Inch Source Drawing" "" "dwg" 4))
  (if (not inchFile)
    (progn
      (princ "\nmetric_check: Cancelled.")
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
      (princ
        (strcat "\nERROR opening file: "
                (vl-catch-all-error-message openRes)))
      (princ)
      (exit)
    )
  )
  (setq inchDoc openRes)

  ;;----------------------------------------------------------------
  ;; 5. Read inch drawing data
  ;;----------------------------------------------------------------
  (princ "\nReading inch drawing...")
  (setq inchDims  (mc:get-dims  inchDoc))
  (setq inchTexts (mc:get-texts inchDoc))
  (princ
    (strcat " "
            (itoa (length inchDims))  " dim(s), "
            (itoa (length inchTexts)) " text/mtext found."))

  ;;----------------------------------------------------------------
  ;; 6. Close inch drawing and restore metric document
  ;;----------------------------------------------------------------
  (vl-catch-all-apply 'vla-close (list inchDoc vlax-false))
  (vla-Activate metricDoc)
  (setq metricMs (vla-get-ModelSpace metricDoc))

  ;;----------------------------------------------------------------
  ;; 7. Sort lists only for deterministic processing.
  ;; Matching itself is nearest-entity based.
  ;;----------------------------------------------------------------
  (setq metricDims  (mc:sort-by-pos metricDims))
  (setq inchDims    (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts   (mc:sort-by-pos inchTexts))

  ;;----------------------------------------------------------------
  ;; 8. Initialise
  ;;----------------------------------------------------------------
  (setq tolerance 0.1
        dimMarkers nil
        txtMarkers nil
        dimPass 0
        dimFail 0
        txtPass 0
        txtFail 0
        unmatchedDim 0
        unmatchedTxt 0)

  ;; Match-distance limits:
  ;; These prevent a far away dimension/text being incorrectly paired.
  ;; If your drawings are very large and shifted, increase these values.
  (setq dimMatchLimit 10.0)
  (setq txtMatchLimit 10.0)

  ;;----------------------------------------------------------------
  ;; 9. DIMENSION CHECK - nearest geometry matching
  ;;----------------------------------------------------------------
  (setq mDimLen (length metricDims)
        iDimLen (length inchDims)
        dimN    iDimLen
        i       0)

  (if (/= mDimLen iDimLen)
    (princ
      (strcat "\nWARNING: dimension count mismatch ("
              (itoa iDimLen) " inch vs "
              (itoa mDimLen) " metric). "
              "Nearest matching will be used; unmatched/far items will be flagged."))
  )

  (setq remainingMetricDims metricDims)

  (if (not (zerop dimN))
    (progn
      (princ (strcat "\nChecking " (itoa dimN) " inch dimension(s) by nearest geometry..."))
      (repeat dimN
        (setq iEntry (nth i inchDims))
        (setq mEntry (mc:find-closest iEntry remainingMetricDims))

        (if mEntry
          (progn
            (setq matchDist (mc:distance (cadr iEntry) (cadr mEntry)))

            (if (> matchDist dimMatchLimit)
              (progn
                ;; Too far, likely not same dimension.
                ;; Mark near the closest metric dimension as fail.
                (setq dimFail (1+ dimFail))
                (setq errPos (cadr mEntry))
                (setq dimMarkers
                  (cons
                    (list nil
                          (strcat "UNMATCHED/FAR DIM  dist "
                                  (mc:fmt6 matchDist)
                                  "  inch "
                                  (mc:fmt6 (car iEntry)) "\"")
                          errPos)
                    dimMarkers))
              )
              (progn
                (setq iVal    (car iEntry)
                      mVal    (car mEntry)
                      errPos  (cadr mEntry)
                      expected (* iVal 25.4)
                      diff    (abs (- mVal expected)))

                (if (> diff tolerance)
                  (setq dimFail (1+ dimFail))
                  (setq dimPass (1+ dimPass))
                )

                (setq dimMarkers
                  (cons
                    (list
                      (<= diff tolerance)
                      (mc:make-dim-label iVal expected mVal diff)
                      errPos)
                    dimMarkers))

                ;; Remove matched metric dimension so it cannot be reused.
                (setq remainingMetricDims
                  (mc:remove-first mEntry remainingMetricDims))
              )
            )
          )
          (progn
            (setq unmatchedDim (1+ unmatchedDim))
            (setq dimFail (1+ dimFail))
          )
        )

        (setq i (1+ i))
      )
    )
  )

  ;; Any metric dimensions left are extra/unmatched.
  (foreach mEntry remainingMetricDims
    (setq dimFail (1+ dimFail))
    (setq dimMarkers
      (cons
        (list nil
              (strcat "EXTRA METRIC DIM  got "
                      (mc:fmt6 (car mEntry))
                      "mm")
              (cadr mEntry))
        dimMarkers))
  )

  ;;----------------------------------------------------------------
  ;; 10. TEXT / MTEXT CHECK - nearest text matching
  ;;----------------------------------------------------------------
  (setq mTxtLen (length metricTexts)
        iTxtLen (length inchTexts)
        txtN    iTxtLen
        i       0)

  (if (/= mTxtLen iTxtLen)
    (princ
      (strcat "\nWARNING: text count mismatch ("
              (itoa iTxtLen) " inch vs "
              (itoa mTxtLen) " metric). "
              "Nearest text matching will be used."))
  )

  (setq remainingMetricTexts metricTexts)

  (if (not (zerop txtN))
    (progn
      (princ (strcat "\nChecking " (itoa txtN) " inch text/mtext item(s)..."))
      (repeat txtN
        (setq iEntry (nth i inchTexts))
        (setq mEntry (mc:find-closest iEntry remainingMetricTexts))

        (if mEntry
          (progn
            (setq matchDist (mc:distance (cadr iEntry) (cadr mEntry)))

            (if (> matchDist txtMatchLimit)
              (progn
                (setq txtFail (1+ txtFail))
                (setq txtMarkers
                  (cons
                    (list nil
                          (strcat "UNMATCHED/FAR TEXT  dist "
                                  (mc:fmt6 matchDist))
                          (cadr mEntry))
                    txtMarkers))
              )
              (progn
                (setq iStr   (car  iEntry)
                      mStr   (car  mEntry)
                      errPos (cadr mEntry)
                      iNums  (mc:extract-numbers iStr)
                      mNums  (mc:extract-numbers mStr))

                ;; Walk through all number pairs for this text entity.
                ;; Integers from the inch text are skipped.
                (if (and iNums mNums (= (length iNums) (length mNums)))
                  (progn
                    (setq iNLen   (length iNums)
                          j       0
                          txtBody nil
                          anyFail nil)

                    (repeat iNLen
                      (setq iNumPair  (nth j iNums)
                            mNumPair  (nth j mNums)
                            iNum      (car  iNumPair)
                            isDecimal (cadr iNumPair)
                            mNum      (car  mNumPair)
                            expected  (* iNum 25.4)
                            diff      (abs (- mNum expected)))

                      (if isDecimal
                        (progn
                          (if (> diff tolerance)
                            (setq anyFail T
                                  txtFail (1+ txtFail))
                            (setq txtPass (1+ txtPass))
                          )

                          (setq txtBody
                            (if txtBody
                              (strcat txtBody
                                      "  |  "
                                      (mc:make-text-label-line iNum expected mNum diff))
                              (mc:make-text-label-line iNum expected mNum diff)))
                        )
                      )

                      (setq j (1+ j))
                    )

                    ;; One marker per text entity if decimal values were checked.
                    (if txtBody
                      (setq txtMarkers
                        (cons (list (not anyFail) txtBody errPos)
                              txtMarkers))
                    )
                  )
                  (progn
                    ;; Number count mismatch inside text.
                    ;; Mark this as fail to show wrong/suspicious item.
                    (setq txtFail (1+ txtFail))
                    (setq txtMarkers
                      (cons
                        (list nil
                              (strcat "TEXT NUMBER COUNT MISMATCH  inchNums "
                                      (itoa (length iNums))
                                      "  metricNums "
                                      (itoa (length mNums)))
                              errPos)
                        txtMarkers))
                  )
                )

                ;; Remove matched metric text so it cannot be reused.
                (setq remainingMetricTexts
                  (mc:remove-first mEntry remainingMetricTexts))
              )
            )
          )
          (progn
            (setq unmatchedTxt (1+ unmatchedTxt))
            (setq txtFail (1+ txtFail))
          )
        )

        (setq i (1+ i))
      )
    )
  )

  ;; Remaining metric texts are extra/unmatched.
  (foreach mEntry remainingMetricTexts
    (setq txtFail (1+ txtFail))
    (setq txtMarkers
      (cons
        (list nil
              "EXTRA METRIC TEXT"
              (cadr mEntry))
        txtMarkers))
  )

  ;;----------------------------------------------------------------
  ;; 11. QC layers
  ;;----------------------------------------------------------------
  (princ "\nUpdating QC layers...")
  (mc:clear-qc-layers)
  (mc:ensure-layer metricDoc "MC_PASS"   3)
  (mc:ensure-layer metricDoc "MC_ERRORS" 7)

  ;;----------------------------------------------------------------
  ;; 12. Place balloons
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
  ;; 13. Summary
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
            "  Values shown  : inch, expected mm, got mm, diff mm\n"
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


(princ "\nMETRIC_CHECK.LSP v6 loaded.")
(princ "\n  METRIC_CHECK -- run check, green=pass  white=fail+expected/got/diff")
(princ "\n  METRIC_CLEAR -- erase all QC balloons and layers")
(princ)