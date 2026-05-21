;;; =====================================================================
;;; METRIC_CHECK.LSP  v12-DISPLAY-ATTR
;;; Commands: METRIC_CHECK   -- run the check, draw pass/fail balloons
;;;           METRIC_CLEAR   -- erase all QC balloons + layers
;;;
;;; Based on your working v10-STABLE structure.
;;;
;;; With the metric drawing open, type METRIC_CHECK.
;;; Pick the inch source drawing from the file dialog.
;;;
;;; PASS entities get a green tick balloon  (layer MC_PASS)
;;; FAIL entities get a white balloon       (layer MC_ERRORS)
;;; MISSING dimension match gets white ??   (layer MC_ERRORS)
;;;
;;; Conversion rule:
;;;   expected metric value = inch drawing DISPLAYED value x 25.4
;;;   then compare expected against metric drawing DISPLAYED value.
;;;
;;; Important behavior:
;;;   - Dimension override text is used first when present.
;;;     Example: R.03, (R.03), 1.25, 25 are read as displayed values.
;;;   - If dimension override is empty or contains <>, vla-get-Measurement is used.
;;;   - TEXT, MTEXT, direct ATTRIBUTES, ATTRIBUTE DEFINITIONS, and attributes
;;;     inside BLOCK REFERENCES are checked for decimal conversions.
;;;   - Note sequence numbers like 1. 2. 3. are ignored.
;;;   - Extra metric dims/text are ignored, no false extra balloons.
;;;   - Text number count mismatch is not an error; comparable pairs only are checked.
;;; =====================================================================

(vl-load-com)

;;; -------------------------------------------------------------------
;;; Global settings
;;; -------------------------------------------------------------------
(setq *mc-conv* 25.4)
(setq *mc-tolerance* 0.1)
(setq *mc-dim-match-limit* 25.0)
(setq *mc-text-match-limit* 25.0)

;;; -------------------------------------------------------------------
;;; Basic helpers
;;; -------------------------------------------------------------------
(defun mc:is-digit (c)
  (and c
       (= (type c) 'STR)
       (= (strlen c) 1)
       (>= (ascii c) 48)
       (<= (ascii c) 57))
)

(defun mc:min-count (a b /)
  (if (< a b) a b)
)

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

(defun mc:fmt (val /)
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

;;; -------------------------------------------------------------------
;;; Matching helpers
;;; -------------------------------------------------------------------
(defun mc:find-closest-scaled (target lst scale / best bestd item d targetPos)
  (setq best nil
        bestd nil
        targetPos nil)

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

(defun mc:find-best-match (target lst / a b)
  ;; Try both possible coordinate relationships:
  ;;   metric position ~= inch position x 25.4
  ;;   metric position ~= inch position
  ;; Use whichever is closer.
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
;;; MTEXT stripping
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
             (setq res (strcat res " "))
           )
           (setq i (1+ i))
         )
       )
      )
      ((and (= c "%")
            (<= (1+ i) len)
            (= (substr s (1+ i) 1) "%"))
       ;; Skip symbols like %%c %%d %%p. Loop increment removes total 3 chars.
       (setq i (+ i 2))
      )
      (T
       (setq res (strcat res c))
      )
    )
    (setq i (1+ i))
  )
  res
)

;;; -------------------------------------------------------------------
;;; Number extraction
;;; -------------------------------------------------------------------
(defun mc:extract-number-pairs (str / result i len c token hadDot hadDigitAfterDot stopToken)
  ;; Returns list of (value isDecimal).
  ;; Accepts: 25, 1.25, .030, R.03, R0.8, (R.03)
  ;; Skips bare note-like tokens such as 1.
  (setq result nil
        len    (strlen str)
        i      1)

  (while (<= i len)
    (setq c (substr str i 1))
    (cond
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
            (if hadDot (setq hadDigitAfterDot T))
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
           (setq i (1+ i))
         )
       )
       (if (not (and hadDot (not hadDigitAfterDot)))
         (setq result (cons (list (atof token) hadDot) result))
       )
       (setq i (1- i))
      )

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
       (setq result (cons (list (atof token) T) result))
       (setq i (1- i))
      )
    )
    (setq i (1+ i))
  )
  (reverse result)
)

(defun mc:extract-decimal-numbers (str / result pairs p)
  ;; Used for normal TEXT / MTEXT / ATTRIBUTES.
  ;; Only true decimals are converted, so note numbers 1. 2. 3. are ignored.
  (setq result nil)
  (setq pairs (mc:extract-number-pairs str))
  (foreach p pairs
    (if (cadr p)
      (setq result (cons (car p) result))
    )
  )
  (reverse result)
)

(defun mc:first-preferred-number (str / pairs p decimalVal intVal)
  ;; Used for DIMENSION OVERRIDE text.
  ;; Prefer the first decimal if one exists, so "2X R.03" returns .03 not 2.
  ;; If no decimal exists, use the first integer, so override "25" returns 25.
  (setq pairs (mc:extract-number-pairs str)
        decimalVal nil
        intVal nil)
  (foreach p pairs
    (cond
      ((and (cadr p) (not decimalVal))
       (setq decimalVal (car p))
      )
      ((and (not (cadr p)) (not intVal))
       (setq intVal (car p))
      )
    )
  )
  (if decimalVal decimalVal intVal)
)

;;; -------------------------------------------------------------------
;;; Dimension collection - displayed value first
;;; -------------------------------------------------------------------
(defun mc:linear-dim-p (oname)
  (and (wcmatch oname "*Dimension*")
       (not (wcmatch oname "*Angular*")))
)

(defun mc:get-dim-override-value (obj / txtRes txt stripped val)
  ;; If a dimension is overwritten, use what is SHOWN.
  ;; Empty override or override containing <> means use measurement fallback.
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
          (setq val (mc:first-preferred-number stripped))
          val
        )
      )
    )
  )
)

(defun mc:get-dim-value (obj / overrideVal measRes)
  ;; Correct v12 behavior:
  ;;   1. Use displayed override value when present.
  ;;   2. Otherwise use actual vla-get-Measurement.
  ;; This does NOT multiply by DIMLFAC or LinearScaleFactor.
  (setq overrideVal (mc:get-dim-override-value obj))
  (if overrideVal
    overrideVal
    (progn
      (setq measRes
        (vl-catch-all-apply 'vla-get-Measurement (list obj)))
      (if (vl-catch-all-error-p measRes)
        nil
        measRes
      )
    )
  )
)

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

(defun mc:get-dims (doc / ms cnt i obj oname dimVal pos result)
  ;; Returns list: (displayedOrMeasuredValue (x y))
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
;;; Text, MText and Attribute collection
;;; -------------------------------------------------------------------
(defun mc:get-object-insertion-xy (obj / posRes pos)
  (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p posRes)
    nil
    (progn
      (setq pos
        (vl-catch-all-apply 'vlax-safearray->list
          (list (vlax-variant-value posRes))))
      (if (and (not (vl-catch-all-error-p pos))
               (listp pos)
               (>= (length pos) 2))
        (list (car pos) (cadr pos))
        nil
      )
    )
  )
)

(defun mc:add-textlike-object (obj result / oname txtRes pos txt)
  ;; Supports TEXT, MTEXT, ATTRIBUTES, ATTRIBUTE DEFINITIONS.
  (setq oname (vla-get-ObjectName obj))
  (setq txtRes (vl-catch-all-apply 'vla-get-TextString (list obj)))
  (setq pos (mc:get-object-insertion-xy obj))

  (if (and (not (vl-catch-all-error-p txtRes))
           pos)
    (progn
      (setq txt txtRes)
      (if (wcmatch oname "AcDbMText")
        (setq txt (mc:strip-mtext txt))
      )
      (cons (list txt pos) result)
    )
    result
  )
)

(defun mc:add-block-attributes (blkRef result / hasRes attRes attList att)
  ;; Attributes inside block references are often NOT visible as ModelSpace items.
  ;; This explicitly reads them from AcDbBlockReference.GetAttributes.
  (setq hasRes (vl-catch-all-apply 'vla-get-HasAttributes (list blkRef)))
  (if (and (not (vl-catch-all-error-p hasRes)) hasRes)
    (progn
      (setq attRes (vl-catch-all-apply 'vla-GetAttributes (list blkRef)))
      (if (not (vl-catch-all-error-p attRes))
        (progn
          (setq attList
            (vl-catch-all-apply 'vlax-safearray->list
              (list (vlax-variant-value attRes))))
          (if (and (not (vl-catch-all-error-p attList))
                   (listp attList))
            (foreach att attList
              (setq result (mc:add-textlike-object att result))
            )
          )
        )
      )
    )
  )
  result
)

(defun mc:get-texts (doc / ms cnt i obj oname result)
  ;; Returns list: (plainString (x y))
  ;; Includes TEXT, MTEXT, direct attributes, attribute definitions,
  ;; and attributes contained inside block references.
  (setq result nil
        ms     (vla-get-ModelSpace doc)
        cnt    (vla-get-Count ms)
        i      0)

  (while (< i cnt)
    (setq obj   (vla-item ms i)
          oname (vla-get-ObjectName obj))
    (cond
      ((or (wcmatch oname "AcDbText")
           (wcmatch oname "AcDbMText")
           (wcmatch oname "AcDbAttribute")
           (wcmatch oname "AcDbAttributeDefinition"))
       (setq result (mc:add-textlike-object obj result))
      )
      ((wcmatch oname "AcDbBlockReference")
       (setq result (mc:add-block-attributes obj result))
      )
    )
    (setq i (1+ i))
  )
  result
)

;;; -------------------------------------------------------------------
;;; Sorting and balloons
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
        (vla-put-Color addRes color)
      )
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
        (setvar "CLAYER" "0")
      )
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
  (strcat
    (mc:fmt inchVal) "\""
    "  exp " (mc:fmt expected) "mm"
    "  got " (mc:fmt actual) "mm"
  )
)

(defun mc:make-pass-label (inchVal expected actual /)
  (strcat "\\U+2713  " (mc:make-label inchVal expected actual))
)

(defun mc:make-missing-label (inchVal expected /)
  (strcat
    "??  "
    (mc:fmt inchVal) "\""
    "  exp " (mc:fmt expected) "mm"
  )
)

(defun mc:get-current-dwg-folder (/ p)
  (setq p (getvar "DWGPREFIX"))
  (if (or (not p) (= p ""))
    (setq p "")
  )
  p
)

;;; ===================================================================
;;; c:metric_check  --  main command
;;; ===================================================================
(defun c:metric_check
    (/ acadObj metricDoc metricDir inchFile openRes inchDoc
       metricDims inchDims metricTexts inchTexts
       mDimLen iDimLen mTxtLen iTxtLen
       i j iEntry mEntry matchInfo matchDist usedScale
       iVal mVal expected diff errPos passFlag missingPos
       iStr mStr iNums mNums iNLen iNum mNum
       dimMarkers txtMarkers txtBody anyFail
       remainingMetricDims remainingMetricTexts
       balloonH errIdx
       dimPass dimFail txtPass txtFail
       ignoredMetricDims ignoredMetricTexts missingDimCount)

  (vl-load-com)

  ;; 1. Active metric document
  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj))

  ;; 2. File picker starts in current metric drawing folder
  (setq metricDir (mc:get-current-dwg-folder))

  ;; 3. Read metric drawing first
  (princ "\nReading metric drawing...")
  (setq metricDims  (mc:get-dims  metricDoc))
  (setq metricTexts (mc:get-texts metricDoc))
  (princ
    (strcat
      " "
      (itoa (length metricDims))  " dim(s), "
      (itoa (length metricTexts)) " text/mtext/attribute found."
    )
  )

  ;; 4. Pick inch source drawing
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

  ;; 5. Open inch drawing read-only
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

  ;; 6. Read inch drawing data
  (princ "\nReading inch drawing...")
  (setq inchDims  (mc:get-dims  inchDoc))
  (setq inchTexts (mc:get-texts inchDoc))
  (princ
    (strcat
      " "
      (itoa (length inchDims))  " dim(s), "
      (itoa (length inchTexts)) " text/mtext/attribute found."
    )
  )

  ;; 7. Close inch drawing and restore metric document
  (vl-catch-all-apply 'vla-close (list inchDoc vlax-false))
  (vla-Activate metricDoc)

  ;; 8. Sort for deterministic processing
  (setq metricDims  (mc:sort-by-pos metricDims))
  (setq inchDims    (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts   (mc:sort-by-pos inchTexts))

  ;; 9. Initialise
  (setq dimMarkers nil
        txtMarkers nil
        dimPass    0
        dimFail    0
        txtPass    0
        txtFail    0
        ignoredMetricDims 0
        ignoredMetricTexts 0
        missingDimCount 0)

  (setq mDimLen (length metricDims)
        iDimLen (length inchDims)
        mTxtLen (length metricTexts)
        iTxtLen (length inchTexts))

  (if (/= mDimLen iDimLen)
    (princ
      (strcat
        "\nInfo: dimension count differs: "
        (itoa iDimLen) " inch vs "
        (itoa mDimLen) " metric. Extra metric dimensions will be ignored."
      )
    )
  )

  (if (/= mTxtLen iTxtLen)
    (princ
      (strcat
        "\nInfo: text/attribute count differs: "
        (itoa iTxtLen) " inch vs "
        (itoa mTxtLen) " metric. Extra metric text/attributes will be ignored."
      )
    )
  )

  ;; 10. Dimension check
  (setq remainingMetricDims metricDims)
  (princ
    (strcat "\nChecking " (itoa iDimLen) " inch dimension(s)...")
  )
  (setq i 0)

  (repeat iDimLen
    (setq iEntry (nth i inchDims))
    (setq iVal   (car iEntry))
    (setq expected (* iVal *mc-conv*))
    (setq matchInfo (mc:find-best-match iEntry remainingMetricDims))

    (if matchInfo
      (progn
        (setq mEntry    (car matchInfo)
              matchDist (cadr matchInfo)
              usedScale (caddr matchInfo))

        ;; No far-match false alert. Compare closest available pair.
        (setq mVal     (car mEntry)
              errPos   (cadr mEntry)
              diff     (abs (- mVal expected))
              passFlag (<= diff *mc-tolerance*))

        (if passFlag
          (setq dimPass (1+ dimPass))
          (setq dimFail (1+ dimFail))
        )

        (setq dimMarkers
          (cons
            (list
              passFlag
              (if passFlag
                (mc:make-pass-label iVal expected mVal)
                (mc:make-label iVal expected mVal)
              )
              errPos
            )
            dimMarkers
          )
        )

        (setq remainingMetricDims
          (mc:remove-first mEntry remainingMetricDims)
        )
      )
      (progn
        ;; No metric dimension left / no metric match. Show ?? marker.
        (setq missingDimCount (1+ missingDimCount))
        (setq dimFail (1+ dimFail))
        (setq missingPos (mc:scale-point (cadr iEntry) *mc-conv*))
        (setq dimMarkers
          (cons
            (list nil
                  (mc:make-missing-label iVal expected)
                  missingPos)
            dimMarkers
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ;; Extra metric dimensions are intentionally ignored.
  (setq ignoredMetricDims (length remainingMetricDims))
  (if (> ignoredMetricDims 0)
    (princ
      (strcat
        "\nInfo: leftover metric dimension(s) ignored: "
        (itoa ignoredMetricDims)
      )
    )
  )

  ;; 11. Text / MText / Attribute check
  (setq remainingMetricTexts metricTexts)
  (princ
    (strcat "\nChecking " (itoa iTxtLen) " inch text/mtext/attribute item(s)...")
  )
  (setq i 0)

  (repeat iTxtLen
    (setq iEntry (nth i inchTexts))
    (setq matchInfo (mc:find-best-match iEntry remainingMetricTexts))

    (if matchInfo
      (progn
        (setq mEntry    (car matchInfo)
              matchDist (cadr matchInfo)
              usedScale (caddr matchInfo))

        (setq iStr   (car  iEntry)
              mStr   (car  mEntry)
              errPos (cadr mEntry))

        (setq iNums (mc:extract-decimal-numbers iStr))
        (setq mNums (mc:extract-decimal-numbers mStr))

        ;; Only compare when both sides have decimal values.
        ;; If counts differ, compare only available pairs.
        (if (and iNums mNums)
          (progn
            (setq iNLen   (mc:min-count (length iNums) (length mNums))
                  j       0
                  txtBody nil
                  anyFail nil)

            (if (> iNLen 0)
              (progn
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
                      (strcat
                        txtBody
                        "  |  "
                        (mc:make-label iNum expected mNum)
                      )
                      (mc:make-label iNum expected mNum)
                    )
                  )
                  (setq j (1+ j))
                )

                (if txtBody
                  (setq txtMarkers
                    (cons
                      (list
                        (not anyFail)
                        (if (not anyFail)
                          (strcat "\\U+2713  " txtBody)
                          txtBody
                        )
                        errPos)
                      txtMarkers
                    )
                  )
                )
              )
            )
          )
        )

        (setq remainingMetricTexts
          (mc:remove-first mEntry remainingMetricTexts)
        )
      )
      (progn
        ;; No metric text left. Skip silently.
      )
    )
    (setq i (1+ i))
  )

  ;; Extra metric text/mtext/attributes are intentionally ignored.
  (setq ignoredMetricTexts (length remainingMetricTexts))
  (if (> ignoredMetricTexts 0)
    (princ
      (strcat
        "\nInfo: leftover metric text/mtext/attribute item(s) ignored: "
        (itoa ignoredMetricTexts)
      )
    )
  )

  ;; 12. QC layers
  (princ "\nUpdating QC layers...")
  (mc:clear-qc-layers)
  (mc:ensure-layer metricDoc "MC_PASS"   3)
  (mc:ensure-layer metricDoc "MC_ERRORS" 7)

  ;; 13. Place balloons
  (setq balloonH (mc:balloon-height)
        errIdx   1)

  (foreach m (reverse dimMarkers)
    (mc:place-balloon
      (car  (caddr m))
      (cadr (caddr m))
      balloonH
      (car m)
      (cadr m)
    )
    (if (not (car m))
      (progn
        (princ
          (strcat
            "\n  ["
            (itoa errIdx)
            "] DIM FAIL/MISSING: "
            (cadr m)
          )
        )
        (setq errIdx (1+ errIdx))
      )
    )
  )

  (foreach m (reverse txtMarkers)
    (mc:place-balloon
      (car  (caddr m))
      (cadr (caddr m))
      balloonH
      (car m)
      (cadr m)
    )
    (if (not (car m))
      (progn
        (princ
          (strcat
            "\n  ["
            (itoa errIdx)
            "] TXT/ATTR FAIL: "
            (cadr m)
          )
        )
        (setq errIdx (1+ errIdx))
      )
    )
  )

  (if (zerop (+ dimFail txtFail))
    (princ "\nAll checked conversions PASSED.")
  )

  (vla-Regen metricDoc 2)

  ;; 14. Summary
  (princ
    (strcat
      "\n"
      "--------------------------------------------\n"
      "METRIC CHECK DONE v12-DISPLAY-ATTR\n"
      "  Dimensions : "
      (itoa dimPass) " pass  " (itoa dimFail) " fail/missing\n"
      "  Text/MText/Attr : "
      (itoa txtPass) " pass  " (itoa txtFail) " fail\n"
      "  Missing dimension matches : " (itoa missingDimCount) "\n"
      "  Total conversion errors   : " (itoa (+ dimFail txtFail)) "\n"
      "  Ignored leftover metric dims : " (itoa ignoredMetricDims) "\n"
      "  Ignored leftover metric text/attr : " (itoa ignoredMetricTexts) "\n"
      "  Dimension source : shown override first, measurement fallback\n"
      "  Text source      : TEXT / MTEXT / block ATTRIBUTES included\n"
      "  Pass balloons : Green tick layer MC_PASS\n"
      "  Fail balloons : White layer MC_ERRORS\n"
      "  Missing dims  : White ?? layer MC_ERRORS\n"
      "  Label format  : inch / expected mm / got mm\n"
      "--------------------------------------------"
    )
  )
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
    2
  )
  (princ "\nMC_PASS and MC_ERRORS balloons and layers removed.")
  (princ)
)

(princ "\nMETRIC_CHECK.LSP v12-DISPLAY-ATTR loaded.")
(princ "\n  METRIC_CHECK -- run check, green tick=pass  white=fail/??")
(princ "\n  METRIC_CLEAR -- erase all QC balloons and layers")
(princ)
