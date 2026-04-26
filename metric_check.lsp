;;; =====================================================================
;;; METRIC_CHECK.LSP  v5
;;; Commands: METRIC_CHECK   -- run the check, draw pass/fail balloons
;;;           METRIC_CLEAR   -- erase all QC balloons + layers
;;;
;;; With the metric drawing open, type METRIC_CHECK.
;;; Pick the inch source drawing from the file dialog.
;;;
;;; PASS entities get a green  checkmark balloon  (layer MC_PASS)
;;; FAIL entities get a red    X balloon showing   got X.XXmm  exp Y.YYmm
;;;
;;; Conversion rule:  metric = inch x 25.4   tolerance +/- 0.1 mm
;;; Angular dims are excluded.
;;; Integers in text (note numbers, counts) are skipped.
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
      ;;-- %%x special symbols: skip 3 chars total
      ;;   Loop adds 1 more at end, so +2 here lands +3.
      ((and (= c "%")
            (<= (1+ i) len)
            (= (substr s (1+ i) 1) "%"))
       (setq i (+ i 2))
      )
      (T (setq res (strcat res c)))
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
;;; mc:get-dims
;;; Collect dimension entities from DOC model space.
;;; Returns list of  (measurement (x y))
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
;;; Uses vl-catch-all-apply so a missing layer is handled cleanly.
;;; -------------------------------------------------------------------
(defun mc:ensure-layer (doc lname color / layers layerRes addRes)
  (setq layers   (vla-get-Layers doc)
        layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (vl-catch-all-error-p layerRes)
    ;;-- Layer does not exist: create it
    (progn
      (setq addRes (vl-catch-all-apply 'vla-add (list layers lname)))
      (if (not (vl-catch-all-error-p addRes))
        (vla-put-Color addRes color))
    )
    ;;-- Layer exists: just update color
    (vla-put-Color layerRes color)
  )
)


;;; -------------------------------------------------------------------
;;; mc:delete-layer
;;; Remove layer LNAME from the current drawing.
;;; Tries vla-delete first; falls back to the _-PURGE command.
;;; Safely switches off LNAME as current layer before deleting.
;;; -------------------------------------------------------------------
(defun mc:delete-layer (lname / doc layers layerRes delRes)
  (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object))
        layers   (vla-get-Layers doc)
        layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (not (vl-catch-all-error-p layerRes))
    (progn
      ;;-- Never delete the current layer
      (if (= (strcase (getvar "CLAYER")) (strcase lname))
        (setvar "CLAYER" "0"))
      ;;-- Try VLA delete
      (setq delRes (vl-catch-all-apply 'vla-delete (list layerRes)))
      ;;-- If VLA delete failed, try the PURGE command as fallback
      (if (vl-catch-all-error-p delRes)
        (vl-catch-all-apply 'command
          (list "._-PURGE" "_La" lname "_No"))
      )
    )
  )
)


;;; -------------------------------------------------------------------
;;; mc:clear-qc-layers
;;; Delete all entities on MC_PASS and MC_ERRORS, then delete both
;;; layers.  Safe to call when the layers do not yet exist.
;;; -------------------------------------------------------------------
(defun mc:clear-qc-layers (/ ss i lname)
  (foreach lname (list "MC_PASS" "MC_ERRORS")
    ;;-- Purge entities on this layer
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
    ;;-- Delete the layer itself
    (mc:delete-layer lname)
  )
)


;;; -------------------------------------------------------------------
;;; mc:place-balloon
;;; Place an MTEXT balloon just above position (PX PY).
;;; BH      = balloon text height
;;; ISPASS  = T for green checkmark,  nil for red fail marker
;;; BODY    = detail string shown after the X on fail balloons
;;;           (ignored for pass)
;;;
;;; Uses entmake (same technique as dim_qc_v16) for reliability.
;;; PASS layer: MC_PASS  color 3 (green)
;;; FAIL layer: MC_ERRORS  color 1 (red)
;;; -------------------------------------------------------------------
(defun mc:place-balloon (px py bh isPass body / ins label layer color bw ed)
  ;;-- Offset the balloon slightly above the entity position
  (setq ins (list px (+ py (* bh 0.75)) 0.0))

  ;;-- Colour only changes, body always shows the three values
  (if isPass
    (setq layer "MC_PASS"   color 3)
    (setq layer "MC_ERRORS" color 1)
  )
  (setq label (strcat "{\\fArial|b1|i0;" body "}"))

  ;;-- Width: proportional to content, minimum 3x height
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
       errPos errLine balloonH errIdx
       dimPass dimFail txtPass txtFail)

  (vl-load-com)

  ;;----------------------------------------------------------------
  ;; 1. Store metric document reference
  ;;----------------------------------------------------------------
  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj))

  ;;----------------------------------------------------------------
  ;; 2. Read metric drawing data FIRST (active doc is still metric)
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
  ;;   vla-close can throw on read-only files in some AutoCAD builds,
  ;;   so catch the error and carry on regardless.
  ;;----------------------------------------------------------------
  (vl-catch-all-apply 'vla-close (list inchDoc vlax-false))
  (vla-Activate metricDoc)

  ;;-- Get metric model space after re-activation
  (setq metricMs (vla-get-ModelSpace metricDoc))

  ;;----------------------------------------------------------------
  ;; 7. Sort all lists by XY position
  ;;----------------------------------------------------------------
  (setq metricDims  (mc:sort-by-pos metricDims))
  (setq inchDims    (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts   (mc:sort-by-pos inchTexts))

  ;;----------------------------------------------------------------
  ;; 8. Initialise error accumulators
  ;;   Each error entry: (description (x y))
  ;;----------------------------------------------------------------
  (setq tolerance 0.1
        dimMarkers nil  txtMarkers nil
        dimPass    0    dimFail    0
        txtPass    0    txtFail    0)

  ;;----------------------------------------------------------------
  ;; 9. DIMENSION CHECK
  ;;----------------------------------------------------------------
  (setq mDimLen (length metricDims)
        iDimLen (length inchDims)
        dimN    (min mDimLen iDimLen)
        i       0)

  (if (not (zerop dimN))
    (progn
      (princ (strcat "\nChecking " (itoa dimN) " dimension(s)..."))
      (repeat dimN
        (setq iVal    (car  (nth i inchDims))
              errPos  (cadr (nth i metricDims))
              mVal    (car  (nth i metricDims))
              expected (* iVal 25.4)
              diff    (abs (- mVal expected)))
        (if (> diff tolerance)
          (setq dimFail (1+ dimFail))
          (setq dimPass (1+ dimPass))
        )
        ;;-- Always add a balloon marker (green=pass, red=fail)
        ;;   Body: {inch}"  {expected}mm  {actual}mm
        (setq dimMarkers
          (cons
            (list
              (<= diff tolerance)
              (strcat (mc:fmt iVal 4) "\""
                      "  " (mc:fmt expected 3) "mm"
                      "  " (mc:fmt mVal 3) "mm")
              errPos)
            dimMarkers))
        (setq i (1+ i))
      )
    )
  )

  ;;----------------------------------------------------------------
  ;; 10. TEXT / MTEXT CHECK
  ;;----------------------------------------------------------------
  (setq mTxtLen (length metricTexts)
        iTxtLen (length inchTexts)
        txtN    (min mTxtLen iTxtLen)
        i       0)

  (if (/= mTxtLen iTxtLen)
    (princ
      (strcat "\nWARNING: text count mismatch ("
              (itoa iTxtLen) " inch vs "
              (itoa mTxtLen) " metric) -- text check skipped."
              "\n  Tip: make sure both drawings have the same text entities."))
  )

  (if (and (not (zerop txtN)) (= mTxtLen iTxtLen))
    (progn
      (princ (strcat "\nChecking " (itoa txtN) " text/mtext pair(s)..."))
      (repeat txtN
        (setq iEntry (nth i inchTexts)
              mEntry (nth i metricTexts)
              iStr   (car  iEntry)
              mStr   (car  mEntry)
              errPos (cadr mEntry)
              iNums  (mc:extract-numbers iStr)
              mNums  (mc:extract-numbers mStr))

        ;;-- Walk through all number pairs for this text entity
        ;;   Collect decimal pairs into one balloon body string.
        ;;   Integers (note numbers, counts) are silently skipped.
        (if (and iNums mNums (= (length iNums) (length mNums)))
          (progn
            (setq iNLen    (length iNums)
                  j        0
                  txtBody  nil
                  anyFail  nil)
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
                  ;;-- Track pass/fail counts
                  (if (> diff tolerance)
                    (setq anyFail T txtFail (1+ txtFail))
                    (setq txtPass (1+ txtPass))
                  )
                  ;;-- Append this pair to the balloon body
                  ;;   Format: {inch}"  {expected}mm  {actual}mm
                  (setq txtBody
                    (if txtBody
                      (strcat txtBody "  |  "
                              (mc:fmt iNum 4) "\""
                              "  " (mc:fmt expected 3) "mm"
                              "  " (mc:fmt mNum 3) "mm")
                      (strcat (mc:fmt iNum 4) "\""
                              "  " (mc:fmt expected 3) "mm"
                              "  " (mc:fmt mNum 3) "mm")))
                )
              )
              (setq j (1+ j))
            )
            ;;-- One marker per text entity (only if it had decimal numbers)
            (if txtBody
              (setq txtMarkers
                (cons (list (not anyFail) txtBody errPos)
                      txtMarkers))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )

  ;;----------------------------------------------------------------
  ;; 11. QC layers -- clear old first, then create fresh
  ;;     Order matters: clear BEFORE create avoids the
  ;;     create-then-immediately-delete bug.
  ;;----------------------------------------------------------------
  (princ "\nUpdating QC layers...")
  (mc:clear-qc-layers)
  (mc:ensure-layer metricDoc "MC_PASS"   3)
  (mc:ensure-layer metricDoc "MC_ERRORS" 1)

  ;;----------------------------------------------------------------
  ;; 12. Place one balloon per entity -- green=correct, red=wrong
  ;;     Body always shows:  {inch}"  {expected}mm  {actual}mm
  ;;----------------------------------------------------------------
  (setq balloonH (mc:balloon-height)
        errIdx   1)

  ;;-- Dimension balloons
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

  ;;-- Text / MText balloons
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

  ;;-- Regenerate so balloons appear  (integer 2 = acAllViewports)
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
            (if (> (+ dimFail txtFail) 0)
              "  Green=correct  Red=wrong  (layers MC_PASS / MC_ERRORS)\n"
              "  All conversions correct -- all balloons are green.\n")
            "--------------------------------------------"))
  (princ)
)


;;; ===================================================================
;;; c:metric_clear  --  erase all markers and delete the layer
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


(princ "\nMETRIC_CHECK.LSP v5 loaded.")
(princ "\n  METRIC_CHECK -- run check, green tick=pass  red X=fail+expected")
(princ "\n  METRIC_CLEAR -- erase all QC balloons and layers")
(princ)
