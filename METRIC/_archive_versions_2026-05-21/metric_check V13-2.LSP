;;; =====================================================================
;;; METRIC_CHECK.LSP  v13.2-EXACT-TICK
;;; Commands: METRIC_CHECK / METRIC_CLEAR
;;;
;;; Main fixes:
;;;  - Green PASS balloons show ONLY tick. No green text.
;;;  - Dimension matching is VALUE-FIRST, then position as tie-breaker.
;;;  - Leftover/unmatched metric dimensions get white ?? markers.
;;;  - Text/attribute matching is VALUE-FIRST and uses global best numeric pair.
;;;    This stops wrong numbers like 2026, 125, .625 from stealing matches when
;;;    the correct values 2.50, 12.1635, .020 are present.
;;;  - Large impossible text/attribute mismatches are treated as unrelated and
;;;    are skipped, not shown as random failures.
;;;  - Dimension override/displayed text is used first; measurement is fallback.
;;;  - TEXT / MTEXT / ATTRIBUTES / ATTRIBUTE DEFINITIONS / block attributes included.
;;; =====================================================================

(vl-load-com)

(setq *mc-conv* 25.4)
(setq *mc-tolerance* 0.1)
(setq *mc-text-max-sane-diff* 2.0) ; mm. Prevents unrelated attributes from producing random huge errors.
(setq *mc-position-weight-pass* 0.001)
(setq *mc-position-weight-fail*  0.000001)

(defun mc:is-digit (c)
  (and c (= (type c) 'STR) (= (strlen c) 1) (>= (ascii c) 48) (<= (ascii c) 57))
)

(defun mc:min-count (a b /) (if (< a b) a b))

(defun mc:xy-p (p /)
  (and (listp p) (>= (length p) 2) (numberp (car p)) (numberp (cadr p)))
)

(defun mc:rtrim0 (s /)
  (while (and (> (strlen s) 1) (= (substr s (strlen s) 1) "0") (vl-string-search "." s))
    (setq s (substr s 1 (1- (strlen s))))
  )
  (if (and (> (strlen s) 1) (= (substr s (strlen s) 1) "."))
    (setq s (substr s 1 (1- (strlen s))))
  )
  s
)

(defun mc:fmt (val /)
  (if (numberp val) (mc:rtrim0 (rtos val 2 4)) "??")
)

(defun mc:distance (p1 p2 / dx dy)
  (if (and (mc:xy-p p1) (mc:xy-p p2))
    (progn
      (setq dx (- (car p1) (car p2)) dy (- (cadr p1) (cadr p2)))
      (sqrt (+ (* dx dx) (* dy dy)))
    )
    1.0e99
  )
)

(defun mc:scale-point (p sc /)
  (if (and (mc:xy-p p) (numberp sc))
    (list (* (car p) sc) (* (cadr p) sc))
    nil
  )
)

(defun mc:safearray-point (variantValue / res)
  (setq res (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value variantValue))))
  (if (and (not (vl-catch-all-error-p res)) (mc:xy-p res))
    (list (car res) (cadr res))
    nil
  )
)

(defun mc:pos-distance-best (inchPos metricPos / dScaled dRaw scaled)
  (if (and (mc:xy-p inchPos) (mc:xy-p metricPos))
    (progn
      (setq scaled (mc:scale-point inchPos *mc-conv*))
      (setq dScaled (mc:distance scaled metricPos))
      (setq dRaw (mc:distance inchPos metricPos))
      (if (< dScaled dRaw) dScaled dRaw)
    )
    1.0e99
  )
)

(defun mc:remove-first (item lst / result removed x)
  (setq result nil removed nil)
  (foreach x lst
    (if (and (not removed) (equal x item 1e-8))
      (setq removed T)
      (setq result (cons x result))
    )
  )
  (reverse result)
)

(defun mc:member-int (n lst / found x)
  (setq found nil)
  (foreach x lst (if (= x n) (setq found T)))
  found
)

(defun mc:strip-mtext (s / res i len c nc depth skipSemi)
  (setq res "" len (strlen s) i 1 depth 0 skipSemi nil)
  (while (<= i len)
    (setq c (substr s i 1))
    (cond
      ((= c "{")
       (setq depth (1+ depth))
       (if (and (< i len) (= (substr s (1+ i) 1) "\\")) (setq skipSemi T))
      )
      ((= c "}")
       (if (> depth 0) (setq depth (- depth 1)))
       (setq skipSemi nil)
      )
      ((and skipSemi (not (= c ";"))) nil)
      ((and skipSemi (= c ";")) (setq skipSemi nil))
      ((= c "\\")
       (if (<= (1+ i) len)
         (progn
           (setq nc (substr s (1+ i) 1))
           (if (wcmatch nc "PpNn~") (setq res (strcat res " ")))
           (setq i (1+ i))
         )
       )
      )
      ((and (= c "%") (<= (1+ i) len) (= (substr s (1+ i) 1) "%"))
       ;; skip %%c / %%d / %%p etc.
       (setq i (+ i 2))
      )
      (T (setq res (strcat res c)))
    )
    (setq i (1+ i))
  )
  res
)

(defun mc:extract-number-pairs (str / result i len c token hadDot hadDigitAfterDot stopToken)
  ;; Returns list of (value isDecimal). Skips bare note tokens like 1.
  (setq result nil len (strlen str) i 1)
  (while (<= i len)
    (setq c (substr str i 1))
    (cond
      ((mc:is-digit c)
       (setq token c hadDot nil hadDigitAfterDot nil stopToken nil i (1+ i))
       (while (and (<= i len) (not stopToken) (or (mc:is-digit (substr str i 1)) (= (substr str i 1) ".")))
         (setq c (substr str i 1))
         (cond
           ((mc:is-digit c)
            (setq token (strcat token c))
            (if hadDot (setq hadDigitAfterDot T))
           )
           ((= c ".")
            (if hadDot
              (setq stopToken T)
              (progn (setq token (strcat token c)) (setq hadDot T))
            )
           )
         )
         (if (not stopToken) (setq i (1+ i)))
       )
       (if (not (and hadDot (not hadDigitAfterDot)))
         (setq result (cons (list (atof token) hadDot) result))
       )
       (setq i (1- i))
      )
      ((and (= c ".") (< i len) (mc:is-digit (substr str (1+ i) 1)))
       (setq token "0." i (+ i 1))
       (while (and (<= i len) (mc:is-digit (substr str i 1)))
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

(defun mc:first-preferred-number (str / pairs p decimalVal intVal)
  ;; For dimension overrides: prefer first decimal. If none, use first integer.
  (setq pairs (mc:extract-number-pairs str) decimalVal nil intVal nil)
  (foreach p pairs
    (if (and (listp p) (numberp (car p)))
      (cond
        ((and (cadr p) (not decimalVal)) (setq decimalVal (car p)))
        ((and (not (cadr p)) (not intVal)) (setq intVal (car p)))
      )
    )
  )
  (cond ((numberp decimalVal) decimalVal) ((numberp intVal) intVal) (T nil))
)

(defun mc:only-dimlike-integer-p (s / u cleaned pairs allDigits)
  ;; Accept plain integer dimension attributes like 17, R17, (17), %%c17.
  ;; Reject multi-number notes/counts like 4 X 90, 2X R.03, NOTE 17.
  (setq u (strcase (mc:strip-mtext s)))
  (setq pairs (mc:extract-number-pairs u))
  (if (and (= (length pairs) 1) (numberp (caar pairs)) (not (cadar pairs)))
    (progn
      (setq cleaned u)
      (foreach rem (list "(" ")" "[" "]" "{" "}" "\"" "'" "R" "DIA" "DIAM" "DIAMETER" "Ø" "%%C" "⌀" " " "\t" "\r" "\n")
        (setq cleaned (vl-string-subst "" rem cleaned))
      )
      (setq allDigits T)
      (foreach ch (mapcar 'chr (vl-string->list cleaned))
        (if (not (mc:is-digit ch)) (setq allDigits nil))
      )
      (and (> (strlen cleaned) 0) allDigits)
    )
    nil
  )
)

(defun mc:extract-conversion-numbers (str / result pairs p)
  ;; For TEXT/MTEXT/ATTRIBUTES:
  ;; - decimals always included
  ;; - integers included only if the full text looks like a dimension value
  (setq result nil)
  (setq pairs (mc:extract-number-pairs (mc:strip-mtext str)))
  (foreach p pairs
    (if (and (listp p) (numberp (car p)))
      (cond
        ((cadr p) (setq result (cons (car p) result)))
        ((mc:only-dimlike-integer-p str) (setq result (cons (car p) result)))
      )
    )
  )
  (reverse result)
)

(defun mc:linear-dim-p (oname)
  (and (wcmatch oname "*Dimension*") (not (wcmatch oname "*Angular*")))
)

(defun mc:get-dim-override-value (obj / txtRes txt stripped val)
  (setq txtRes (vl-catch-all-apply 'vla-get-TextOverride (list obj)))
  (if (vl-catch-all-error-p txtRes)
    nil
    (progn
      (setq txt txtRes)
      (if (or (not txt) (= txt "") (vl-string-search "<>" txt))
        nil
        (progn
          (setq stripped (mc:strip-mtext txt))
          (setq val (mc:first-preferred-number stripped))
          (if (numberp val) val nil)
        )
      )
    )
  )
)

(defun mc:get-dim-value (obj / overrideVal measRes)
  ;; displayed override first, measurement fallback
  (setq overrideVal (mc:get-dim-override-value obj))
  (if (numberp overrideVal)
    overrideVal
    (progn
      (setq measRes (vl-catch-all-apply 'vla-get-Measurement (list obj)))
      (if (and (not (vl-catch-all-error-p measRes)) (numberp measRes)) measRes nil)
    )
  )
)

(defun mc:dim-geometry-point (obj / p1Res p2Res p1 p2 txtRes txtPos)
  (setq p1 nil p2 nil txtPos nil)
  (setq p1Res (vl-catch-all-apply 'vla-get-ExtLine1Point (list obj)))
  (setq p2Res (vl-catch-all-apply 'vla-get-ExtLine2Point (list obj)))
  (if (and (not (vl-catch-all-error-p p1Res)) (not (vl-catch-all-error-p p2Res)))
    (progn (setq p1 (mc:safearray-point p1Res)) (setq p2 (mc:safearray-point p2Res)))
  )
  (if (and p1 p2)
    (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0))
    (progn
      (setq txtRes (vl-catch-all-apply 'vla-get-TextPosition (list obj)))
      (if (not (vl-catch-all-error-p txtRes)) (setq txtPos (mc:safearray-point txtRes)))
      txtPos
    )
  )
)

(defun mc:get-dims (doc / ms cnt i obj oname dimVal pos result)
  (setq result nil ms (vla-get-ModelSpace doc) cnt (vla-get-Count ms) i 0)
  (while (< i cnt)
    (setq obj (vla-item ms i) oname (vla-get-ObjectName obj))
    (if (mc:linear-dim-p oname)
      (progn
        (setq dimVal (mc:get-dim-value obj))
        (setq pos (mc:dim-geometry-point obj))
        (if (and (numberp dimVal) (mc:xy-p pos))
          (setq result (cons (list dimVal pos) result))
        )
      )
    )
    (setq i (1+ i))
  )
  result
)

(defun mc:get-object-insertion-xy (obj / posRes pos)
  (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p posRes)
    nil
    (progn
      (setq pos (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value posRes))))
      (if (and (not (vl-catch-all-error-p pos)) (mc:xy-p pos)) (list (car pos) (cadr pos)) nil)
    )
  )
)

(defun mc:add-textlike-object (obj result / oname txtRes pos txt)
  (setq oname (vla-get-ObjectName obj))
  (setq txtRes (vl-catch-all-apply 'vla-get-TextString (list obj)))
  (setq pos (mc:get-object-insertion-xy obj))
  (if (and (not (vl-catch-all-error-p txtRes)) (= (type txtRes) 'STR) (mc:xy-p pos))
    (progn
      (setq txt txtRes)
      (if (wcmatch oname "AcDbMText") (setq txt (mc:strip-mtext txt)))
      (cons (list txt pos) result)
    )
    result
  )
)

(defun mc:add-block-attributes (blkRef result / hasRes attRes attList att)
  (setq hasRes (vl-catch-all-apply 'vla-get-HasAttributes (list blkRef)))
  (if (and (not (vl-catch-all-error-p hasRes)) hasRes)
    (progn
      (setq attRes (vl-catch-all-apply 'vla-GetAttributes (list blkRef)))
      (if (not (vl-catch-all-error-p attRes))
        (progn
          (setq attList (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value attRes))))
          (if (and (not (vl-catch-all-error-p attList)) (listp attList))
            (foreach att attList (setq result (mc:add-textlike-object att result)))
          )
        )
      )
    )
  )
  result
)

(defun mc:get-texts (doc / ms cnt i obj oname result)
  (setq result nil ms (vla-get-ModelSpace doc) cnt (vla-get-Count ms) i 0)
  (while (< i cnt)
    (setq obj (vla-item ms i) oname (vla-get-ObjectName obj))
    (cond
      ((or (wcmatch oname "AcDbText") (wcmatch oname "AcDbMText")
           (wcmatch oname "AcDbAttribute") (wcmatch oname "AcDbAttributeDefinition"))
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

(defun mc:find-best-dim-match (iEntry metricList / expected best bestScore bestVDiff bestPDist mEntry mVal vDiff pDist score)
  ;; VALUE-FIRST dimension matching. Prevents nearby dimensions being interchanged.
  (setq best nil bestScore nil bestVDiff nil bestPDist nil)
  (if (and iEntry (numberp (car iEntry)))
    (progn
      (setq expected (* (car iEntry) *mc-conv*))
      (foreach mEntry metricList
        (setq mVal (car mEntry))
        (if (numberp mVal)
          (progn
            (setq vDiff (abs (- mVal expected)))
            (setq pDist (mc:pos-distance-best (cadr iEntry) (cadr mEntry)))
            (if (<= vDiff *mc-tolerance*)
              (setq score (+ (* vDiff 1000.0) (* pDist *mc-position-weight-pass*)))
              (setq score (+ 1000000.0 (* vDiff 1000.0) (* pDist *mc-position-weight-fail*)))
            )
            (if (or (not bestScore) (< score bestScore))
              (setq best mEntry bestScore score bestVDiff vDiff bestPDist pDist)
            )
          )
        )
      )
    )
  )
  (if best (list best bestVDiff bestPDist) nil)
)

(defun mc:global-best-number-pairs (inchNums metricNums / candidates iIdx mIdx iVal mVal expected diff sorted selected usedI usedM cand)
  ;; Build all possible inch-metric numeric pairs and choose the globally best ones.
  ;; This fixes text strings containing extra values: .625 will not beat .020 when metric is .51.
  (setq candidates nil iIdx 0)
  (foreach iVal inchNums
    (setq mIdx 0)
    (foreach mVal metricNums
      (if (and (numberp iVal) (numberp mVal))
        (progn
          (setq expected (* iVal *mc-conv*))
          (setq diff (abs (- mVal expected)))
          ;; candidate: (diff iIdx mIdx iVal mVal expected)
          (setq candidates (cons (list diff iIdx mIdx iVal mVal expected) candidates))
        )
      )
      (setq mIdx (1+ mIdx))
    )
    (setq iIdx (1+ iIdx))
  )

  (setq sorted (vl-sort candidates '(lambda (a b) (< (car a) (car b)))))
  (setq selected nil usedI nil usedM nil)
  (foreach cand sorted
    (if (and (not (mc:member-int (cadr cand) usedI))
             (not (mc:member-int (caddr cand) usedM)))
      (progn
        ;; selected pair format: (inch metric expected diff)
        (setq selected (cons (list (cadddr cand) (nth 4 cand) (nth 5 cand) (car cand)) selected))
        (setq usedI (cons (cadr cand) usedI))
        (setq usedM (cons (caddr cand) usedM))
      )
    )
  )
  (reverse selected)
)

(defun mc:text-values-total-diff (inchNums metricNums / pairs total p)
  (setq pairs (mc:global-best-number-pairs inchNums metricNums))
  (if pairs
    (progn
      (setq total 0.0)
      (foreach p pairs (setq total (+ total (cadddr p))))
      total
    )
    nil
  )
)

(defun mc:find-best-text-match (iEntry metricList / iNums mNums best bestScore bestVDiff bestPDist mEntry vDiff pDist score)
  ;; Text/attribute matching is value-first. No position-only stealing for numeric strings.
  (setq iNums (mc:extract-conversion-numbers (car iEntry))
        best nil bestScore nil bestVDiff nil bestPDist nil)

  (if iNums
    (foreach mEntry metricList
      (setq mNums (mc:extract-conversion-numbers (car mEntry)))
      (setq vDiff (mc:text-values-total-diff iNums mNums))
      (if vDiff
        (progn
          (setq pDist (mc:pos-distance-best (cadr iEntry) (cadr mEntry)))
          (if (<= vDiff *mc-tolerance*)
            (setq score (+ (* vDiff 1000.0) (* pDist *mc-position-weight-pass*)))
            (setq score (+ 1000000.0 (* vDiff 1000.0) (* pDist *mc-position-weight-fail*)))
          )
          (if (or (not bestScore) (< score bestScore))
            (setq best mEntry bestScore score bestVDiff vDiff bestPDist pDist)
          )
        )
      )
    )
  )

  ;; IMPORTANT: if numeric text has no sane value match, return nil. Do not fall back to position.
  (if (and best bestVDiff (<= bestVDiff *mc-text-max-sane-diff*))
    (list best bestVDiff bestPDist)
    nil
  )
)

(defun mc:sort-by-pos (lst /)
  (vl-sort lst
    (function
      (lambda (a b)
        (cond
          ((not (mc:xy-p (cadr a))) nil)
          ((not (mc:xy-p (cadr b))) T)
          ((< (caadr a) (caadr b)) T)
          ((and (equal (caadr a) (caadr b) 0.01) (< (cadadr a) (cadadr b))) T)
          (T nil)
        )
      )
    )
  )
)

(defun mc:balloon-height (/ dtxt dscl sz)
  (setq dtxt (getvar "DIMTXT") dscl (getvar "DIMSCALE") sz (* (max dtxt 0.05) (max dscl 1.0) 0.85))
  (max sz 0.5)
)

(defun mc:ensure-layer (doc lname color / layers layerRes addRes)
  (setq layers (vla-get-Layers doc) layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (vl-catch-all-error-p layerRes)
    (progn
      (setq addRes (vl-catch-all-apply 'vla-add (list layers lname)))
      (if (not (vl-catch-all-error-p addRes)) (vla-put-Color addRes color))
    )
    (vla-put-Color layerRes color)
  )
)

(defun mc:delete-layer (lname / doc layers layerRes delRes)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)) layers (vla-get-Layers doc) layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (not (vl-catch-all-error-p layerRes))
    (progn
      (if (= (strcase (getvar "CLAYER")) (strcase lname)) (setvar "CLAYER" "0"))
      (setq delRes (vl-catch-all-apply 'vla-delete (list layerRes)))
      (if (vl-catch-all-error-p delRes) (vl-catch-all-apply 'command (list "._-PURGE" "_La" lname "_No")))
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
  (if isPass (setq layer "MC_PASS" color 3) (setq layer "MC_ERRORS" color 7))
  (setq label (strcat "{\\fArial|b1|i0;" body "}"))
  (setq bw (max (* (strlen label) bh 0.50) (* bh 3.0)))
  (setq ed (list (cons 0 "MTEXT") (cons 100 "AcDbEntity") (cons 8 layer) (cons 62 color)
                 (cons 100 "AcDbMText") (cons 10 ins) (cons 40 bh) (cons 41 bw)
                 (cons 71 1) (cons 72 1) (cons 1 label)))
  (vl-catch-all-apply 'entmake (list ed))
)

(defun mc:make-label (inchVal expected actual /)
  (strcat (mc:fmt inchVal) "\"" "  exp " (mc:fmt expected) "mm" "  got " (mc:fmt actual) "mm")
)

(defun mc:make-pass-label (/) "\\U+2713")

(defun mc:make-missing-label (inchVal expected /)
  (strcat "??  " (mc:fmt inchVal) "\"" "  exp " (mc:fmt expected) "mm")
)

(defun mc:make-unmatched-metric-label (metricVal /)
  (strcat "??  got " (mc:fmt metricVal) "mm")
)

(defun mc:get-current-dwg-folder (/ p)
  (setq p (getvar "DWGPREFIX"))
  (if (or (not p) (= p "")) (setq p ""))
  p
)

;;; ===================================================================
;;; c:metric_check  --  main command
;;; ===================================================================
(defun c:metric_check
    (/ acadObj metricDoc metricDir inchFile openRes inchDoc
       metricDims inchDims metricTexts inchTexts mDimLen iDimLen mTxtLen iTxtLen
       i iEntry mEntry matchInfo matchDist iVal mVal expected diff errPos passFlag missingPos
       iStr mStr iNums mNums pairs pair iNum mNum dimMarkers txtMarkers txtBody anyFail
       remainingMetricDims remainingMetricTexts balloonH errIdx dimPass dimFail txtPass txtFail
       ignoredMetricTexts missingDimCount unmatchedMetricDimCount)

  (vl-load-com)
  (setq acadObj (vlax-get-acad-object) metricDoc (vla-get-ActiveDocument acadObj))
  (setq metricDir (mc:get-current-dwg-folder))

  (princ "\nReading metric drawing...")
  (setq metricDims (mc:get-dims metricDoc))
  (setq metricTexts (mc:get-texts metricDoc))
  (princ (strcat " " (itoa (length metricDims)) " dim(s), " (itoa (length metricTexts)) " text/mtext/attribute found."))

  (setq inchFile (getfiled "Select Inch Source Drawing" metricDir "dwg" 4))
  (if (not inchFile) (progn (princ "\nmetric_check: Cancelled.") (princ) (exit)))

  (princ (strcat "\nOpening: " inchFile " ..."))
  (setq openRes (vl-catch-all-apply 'vla-open (list (vla-get-Documents acadObj) inchFile vlax-true)))
  (if (vl-catch-all-error-p openRes)
    (progn (princ (strcat "\nERROR opening file: " (vl-catch-all-error-message openRes))) (princ) (exit)))
  (setq inchDoc openRes)

  (princ "\nReading inch drawing...")
  (setq inchDims (mc:get-dims inchDoc))
  (setq inchTexts (mc:get-texts inchDoc))
  (princ (strcat " " (itoa (length inchDims)) " dim(s), " (itoa (length inchTexts)) " text/mtext/attribute found."))

  (vl-catch-all-apply 'vla-close (list inchDoc vlax-false))
  (vla-Activate metricDoc)

  (setq metricDims (mc:sort-by-pos metricDims))
  (setq inchDims (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts (mc:sort-by-pos inchTexts))

  (setq dimMarkers nil txtMarkers nil dimPass 0 dimFail 0 txtPass 0 txtFail 0 ignoredMetricTexts 0 missingDimCount 0 unmatchedMetricDimCount 0)
  (setq mDimLen (length metricDims) iDimLen (length inchDims) mTxtLen (length metricTexts) iTxtLen (length inchTexts))

  (if (/= mDimLen iDimLen)
    (princ (strcat "\nInfo: dimension count differs: " (itoa iDimLen) " inch vs " (itoa mDimLen) " metric. Unmatched metric dims will get ??.")))
  (if (/= mTxtLen iTxtLen)
    (princ (strcat "\nInfo: text/attribute count differs: " (itoa iTxtLen) " inch vs " (itoa mTxtLen) " metric. Leftover metric text/attributes ignored.")))

  ;; Dimension check
  (setq remainingMetricDims metricDims)
  (princ (strcat "\nChecking " (itoa iDimLen) " inch dimension(s) with value-first matching..."))
  (setq i 0)
  (repeat iDimLen
    (setq iEntry (nth i inchDims))
    (setq iVal (car iEntry))
    (if (numberp iVal) (setq expected (* iVal *mc-conv*)) (setq expected nil))
    (setq matchInfo (mc:find-best-dim-match iEntry remainingMetricDims))
    (if matchInfo
      (progn
        (setq mEntry (car matchInfo) diff (cadr matchInfo) matchDist (caddr matchInfo))
        (setq mVal (car mEntry) errPos (cadr mEntry))
        (if (and (numberp mVal) (numberp expected)) (setq passFlag (<= diff *mc-tolerance*)) (setq passFlag nil diff 1.0e99))
        (if passFlag (setq dimPass (1+ dimPass)) (setq dimFail (1+ dimFail)))
        (setq dimMarkers (cons (list passFlag (if passFlag (mc:make-pass-label) (mc:make-label iVal expected mVal)) errPos) dimMarkers))
        (setq remainingMetricDims (mc:remove-first mEntry remainingMetricDims))
      )
      (progn
        (setq missingDimCount (1+ missingDimCount))
        (setq dimFail (1+ dimFail))
        (setq missingPos (mc:scale-point (cadr iEntry) *mc-conv*))
        (if (not (mc:xy-p missingPos)) (setq missingPos (cadr iEntry)))
        (if (mc:xy-p missingPos)
          (setq dimMarkers (cons (list nil (mc:make-missing-label iVal expected) missingPos) dimMarkers)))
      )
    )
    (setq i (1+ i))
  )

  ;; Leftover metric dimensions now get ??
  (foreach mEntry remainingMetricDims
    (setq unmatchedMetricDimCount (1+ unmatchedMetricDimCount))
    (setq dimFail (1+ dimFail))
    (setq dimMarkers (cons (list nil (mc:make-unmatched-metric-label (car mEntry)) (cadr mEntry)) dimMarkers))
  )

  ;; Text/attribute check
  (setq remainingMetricTexts metricTexts)
  (princ (strcat "\nChecking " (itoa iTxtLen) " inch text/mtext/attribute item(s)..."))
  (setq i 0)
  (repeat iTxtLen
    (setq iEntry (nth i inchTexts))
    (setq matchInfo (mc:find-best-text-match iEntry remainingMetricTexts))
    (if matchInfo
      (progn
        (setq mEntry (car matchInfo))
        (setq iStr (car iEntry) mStr (car mEntry) errPos (cadr mEntry))
        (setq iNums (mc:extract-conversion-numbers iStr))
        (setq mNums (mc:extract-conversion-numbers mStr))
        (setq pairs (mc:global-best-number-pairs iNums mNums))
        (if pairs
          (progn
            (setq txtBody nil anyFail nil)
            (foreach pair pairs
              (setq iNum (car pair) mNum (cadr pair) expected (caddr pair) diff (cadddr pair))
              (if (> diff *mc-tolerance*) (setq anyFail T txtFail (1+ txtFail)) (setq txtPass (1+ txtPass)))
              (setq txtBody (if txtBody (strcat txtBody "  |  " (mc:make-label iNum expected mNum)) (mc:make-label iNum expected mNum)))
            )
            (if txtBody
              (setq txtMarkers (cons (list (not anyFail) (if (not anyFail) (mc:make-pass-label) txtBody) errPos) txtMarkers)))
          )
        )
        (setq remainingMetricTexts (mc:remove-first mEntry remainingMetricTexts))
      )
    )
    (setq i (1+ i))
  )

  (setq ignoredMetricTexts (length remainingMetricTexts))
  (if (> ignoredMetricTexts 0)
    (princ (strcat "\nInfo: leftover metric text/mtext/attribute item(s) ignored: " (itoa ignoredMetricTexts))))

  (princ "\nUpdating QC layers...")
  (mc:clear-qc-layers)
  (mc:ensure-layer metricDoc "MC_PASS" 3)
  (mc:ensure-layer metricDoc "MC_ERRORS" 7)

  (setq balloonH (mc:balloon-height) errIdx 1)
  (foreach m (reverse dimMarkers)
    (mc:place-balloon (car (caddr m)) (cadr (caddr m)) balloonH (car m) (cadr m))
    (if (not (car m)) (progn (princ (strcat "\n  [" (itoa errIdx) "] DIM FAIL/MISSING: " (cadr m))) (setq errIdx (1+ errIdx))))
  )
  (foreach m (reverse txtMarkers)
    (mc:place-balloon (car (caddr m)) (cadr (caddr m)) balloonH (car m) (cadr m))
    (if (not (car m)) (progn (princ (strcat "\n  [" (itoa errIdx) "] TXT/ATTR FAIL: " (cadr m))) (setq errIdx (1+ errIdx))))
  )
  (if (zerop (+ dimFail txtFail)) (princ "\nAll checked conversions PASSED."))
  (vla-Regen metricDoc 2)

  (princ
    (strcat
      "\n--------------------------------------------\n"
      "METRIC CHECK DONE v13.2-EXACT-TICK\n"
      "  Dimensions : " (itoa dimPass) " pass  " (itoa dimFail) " fail/missing\n"
      "  Text/MText/Attr : " (itoa txtPass) " pass  " (itoa txtFail) " fail\n"
      "  Missing inch dimension matches : " (itoa missingDimCount) "\n"
      "  Unmatched metric dimensions   : " (itoa unmatchedMetricDimCount) "\n"
      "  Ignored leftover metric text/attr : " (itoa ignoredMetricTexts) "\n"
      "  Matching mode : VALUE-FIRST, global best numeric pairing\n"
      "  Correct/pass balloons : Green tick only\n"
      "  Fail balloons : White layer MC_ERRORS\n"
      "  Missing/unmatched dims : White ?? layer MC_ERRORS\n"
      "--------------------------------------------"))
  (princ)
)

(defun c:metric_clear (/)
  (vl-load-com)
  (mc:clear-qc-layers)
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 2)
  (princ "\nMC_PASS and MC_ERRORS balloons and layers removed.")
  (princ)
)

(princ "\nMETRIC_CHECK.LSP v13.2-EXACT-TICK loaded.")
(princ "\n  METRIC_CHECK -- value-first matching, green tick-only=pass, white=fail/??")
(princ "\n  METRIC_CLEAR -- erase all QC balloons and layers")
(princ)
