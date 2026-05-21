;;; =====================================================================
;;; METRIC_CHECK.LSP  v14.4-FINAL-PRO
;;; Commands: METRIC_CHECK / METRIC_CLEAR
;;;
;;; Professional build based on v14.2 + category patch, with these fixes:
;;;
;;;   1. Inch source DWG is read through ObjectDBX when available, so it does
;;;      NOT open as a visible drawing tab/window. If ObjectDBX is unavailable,
;;;      the code falls back to read-only vla-open and closes it automatically.
;;;
;;;   2. Ignored block names/effective names:
;;;        C, REVSYMB, REVC
;;;      Attributes inside these blocks are never read or checked.
;;;
;;;   3. Text/attribute matching is category-safe:
;;;        DIA/RAD/DIM/TOL are not cross-paired.
;;;      Example fixed:
;;;        .771 diameter will not match .03 FCF/tolerance.
;;;
;;;   4. Text/attribute result states are strict:
;;;        FOUND + correct = green tick only
;;;        FOUND + wrong   = white exp/got error
;;;        NOT FOUND       = white ??
;;;      A wrong found attribute will not become a separate inch ?? plus metric ??.
;;;
;;;   5. Random title/sheet/revision numbers are filtered out by:
;;;        - ignored block names
;;;        - dimension-like text rules
;;;        - text category rules
;;;
;;;   6. Dimensions still use displayed override first, measurement fallback.
;;;
;;; Conversion:
;;;   expected metric = inch displayed value * 25.4
;;; Tolerance:
;;;   +/- 0.1 mm
;;; =====================================================================

(vl-load-com)

;;; -------------------------------------------------------------------
;;; Settings
;;; -------------------------------------------------------------------
(setq *mc-conv* 25.4)
(setq *mc-tolerance* 0.1)
(setq *mc-text-max-sane-diff* 2.0)
(setq *mc-text-position-limit* 75.0)
(setq *mc-max-inch-text-value* 1000.0)
(setq *mc-max-metric-text-value* 25000.0)
(setq *mc-position-weight-pass* 0.001)
(setq *mc-position-weight-fail* 0.000001)
(setq *mc-ignored-block-names* (list "C" "REVSYMB" "REVC"))

;;; -------------------------------------------------------------------
;;; Basic helpers
;;; -------------------------------------------------------------------
(defun mc:is-digit (c)
  (and c (= (type c) 'STR) (= (strlen c) 1) (>= (ascii c) 48) (<= (ascii c) 57))
)

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

(defun mc:fmt (v /)
  (if (numberp v) (mc:rtrim0 (rtos v 2 4)) "??")
)

(defun mc:distance (p1 p2 / dx dy)
  (if (and (mc:xy-p p1) (mc:xy-p p2))
    (progn
      (setq dx (- (car p1) (car p2))
            dy (- (cadr p1) (cadr p2)))
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

(defun mc:pos-distance-best (inchPos metricPos / scaled dScaled dRaw)
  ;; Some DWGs are scaled by 25.4; some are not. Use whichever position relation is closer.
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

(defun mc:str-upper (s /)
  (if (= (type s) 'STR) (strcase s) "")
)

(defun mc:contains-any (s patterns / hit p u)
  (setq hit nil u (mc:str-upper s))
  (foreach p patterns
    (if (vl-string-search p u) (setq hit T))
  )
  hit
)

;;; -------------------------------------------------------------------
;;; ObjectDBX: open inch DWG without visible window
;;; -------------------------------------------------------------------
(defun mc:create-objectdbx-doc (acadObj / versions prog dbx)
  ;; Try modern ObjectDBX ProgIDs first, then generic.
  (setq versions (list "ObjectDBX.AxDbDocument.25" "ObjectDBX.AxDbDocument.24" "ObjectDBX.AxDbDocument.23"
                       "ObjectDBX.AxDbDocument.22" "ObjectDBX.AxDbDocument.21" "ObjectDBX.AxDbDocument.20"
                       "ObjectDBX.AxDbDocument.19" "ObjectDBX.AxDbDocument.18" "ObjectDBX.AxDbDocument"))
  (setq dbx nil)
  (foreach prog versions
    (if (not dbx)
      (progn
        (setq dbx (vl-catch-all-apply 'vla-GetInterfaceObject (list acadObj prog)))
        (if (vl-catch-all-error-p dbx) (setq dbx nil))
      )
    )
  )
  dbx
)

(defun mc:open-inch-source-doc (acadObj inchFile / dbx openRes visibleRes)
  ;; Returns (doc isDbx). isDbx=T means no visible drawing window was opened.
  (setq dbx (mc:create-objectdbx-doc acadObj))
  (if dbx
    (progn
      (setq openRes (vl-catch-all-apply 'vla-open (list dbx inchFile)))
      (if (vl-catch-all-error-p openRes)
        (progn
          (vl-catch-all-apply 'vlax-release-object (list dbx))
          ;; fallback: visible read-only open
          (setq visibleRes (vl-catch-all-apply 'vla-open (list (vla-get-Documents acadObj) inchFile vlax-true)))
          (if (vl-catch-all-error-p visibleRes) nil (list visibleRes nil))
        )
        (list dbx T)
      )
    )
    (progn
      (setq visibleRes (vl-catch-all-apply 'vla-open (list (vla-get-Documents acadObj) inchFile vlax-true)))
      (if (vl-catch-all-error-p visibleRes) nil (list visibleRes nil))
    )
  )
)

(defun mc:close-inch-source-doc (doc isDbx /)
  (if doc
    (if isDbx
      (vl-catch-all-apply 'vlax-release-object (list doc))
      (vl-catch-all-apply 'vla-close (list doc vlax-false))
    )
  )
)

;;; -------------------------------------------------------------------
;;; Block filters
;;; -------------------------------------------------------------------
(defun mc:block-name (blkRef / eRes nRes)
  (setq eRes (vl-catch-all-apply 'vla-get-EffectiveName (list blkRef)))
  (if (and (not (vl-catch-all-error-p eRes)) (= (type eRes) 'STR))
    eRes
    (progn
      (setq nRes (vl-catch-all-apply 'vla-get-Name (list blkRef)))
      (if (and (not (vl-catch-all-error-p nRes)) (= (type nRes) 'STR)) nRes "")
    )
  )
)

(defun mc:ignored-block-p (blkRef / bname hit x)
  (setq bname (strcase (mc:block-name blkRef)) hit nil)
  (foreach x *mc-ignored-block-names*
    (if (= bname (strcase x)) (setq hit T))
  )
  hit
)

;;; -------------------------------------------------------------------
;;; MTEXT cleanup and numeric extraction
;;; -------------------------------------------------------------------
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
       ;; Skip %%c / %%d / %%p codes.
       (setq i (+ i 2))
      )
      (T (setq res (strcat res c)))
    )
    (setq i (1+ i))
  )
  res
)

(defun mc:extract-number-pairs (str / result i len c token hadDot hadDigitAfterDot stopToken)
  ;; Returns (value isDecimal tokenString). Bare note tokens like 1. are ignored.
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
         (setq result (cons (list (atof token) hadDot token) result))
       )
       (setq i (1- i))
      )
      ((and (= c ".") (< i len) (mc:is-digit (substr str (1+ i) 1)))
       (setq token "0." i (+ i 1))
       (while (and (<= i len) (mc:is-digit (substr str i 1)))
         (setq token (strcat token (substr str i 1)))
         (setq i (1+ i))
       )
       (setq result (cons (list (atof token) T token) result))
       (setq i (1- i))
      )
    )
    (setq i (1+ i))
  )
  (reverse result)
)

(defun mc:first-preferred-number (str / pairs p decimalVal intVal)
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

(defun mc:bad-titleblock-word-p (s /)
  (mc:contains-any s
    (list "SHEET" "REV" "DATE" "DRAWING" "DWG" "TITLE" "CAGE" "SCALE" "ZONE"
          "APPRO" "CHECK" "DRAWN" "ORDER" "PART" "SERIAL" "S/N" "ITEM"
          "QTY" "QUANTITY" "PROJECT" "CUSTOMER" "CONTRACT" "SIZE" "CODE"
          "NO." "NUMBER" "MODEL" "MATERIAL" "FINISH"))
)

(defun mc:dim-cue-p (s / u)
  (setq u (mc:str-upper (vl-string-trim " \t\r\n" s)))
  (or (mc:contains-any u (list "Ø" "%%C" " DIA" "DIAM" " R" "R." " R." "RAD" " THRU" "±" "+/-"))
      (= (substr u 1 1) "R"))
)

(defun mc:single-number-string-p (s / pairs stripped)
  (setq stripped (vl-string-trim " \t\r\n()[]{}\"'" (mc:strip-mtext s)))
  (setq pairs (mc:extract-number-pairs stripped))
  (and (= (length pairs) 1) (= (strlen stripped) (strlen (caddr (car pairs)))))
)

(defun mc:only-dimlike-integer-p (s / u cleaned pairs allDigits)
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

(defun mc:number-sane-for-text-p (v isMetric /)
  (and (numberp v) (> v 0.0) (if isMetric (< v *mc-max-metric-text-value*) (< v *mc-max-inch-text-value*)))
)

(defun mc:extract-conversion-numbers-core (str isMetric / result pairs p val isDec token raw)
  ;; Strict dimension-like extraction. This keeps title/sheet/revision numbers out.
  (setq result nil raw (mc:strip-mtext str) pairs (mc:extract-number-pairs raw))
  (foreach p pairs
    (setq val (car p) isDec (cadr p) token (caddr p))
    (if (and (mc:number-sane-for-text-p val isMetric)
             (not (and (not (mc:dim-cue-p raw)) (mc:bad-titleblock-word-p raw))))
      (cond
        ((and isDec (= (substr token 1 2) "0.")) (setq result (cons val result)))
        ((and isDec (or (mc:dim-cue-p raw) (mc:single-number-string-p raw))) (setq result (cons val result)))
        ((and (not isDec) (mc:only-dimlike-integer-p raw)) (setq result (cons val result)))
      )
    )
  )
  (reverse result)
)
(defun mc:extract-conversion-numbers (str /) (mc:extract-conversion-numbers-core str nil))
(defun mc:extract-conversion-numbers-metric (str /) (mc:extract-conversion-numbers-core str T))

;;; -------------------------------------------------------------------
;;; Text category classification - prevents DIA/TOL cross-matching
;;; -------------------------------------------------------------------
(defun mc:max-num (nums / m n)
  (setq m nil)
  (foreach n nums (if (numberp n) (if (or (not m) (> n m)) (setq m n))))
  m
)
(defun mc:raw-upper (s /) (strcase (mc:strip-mtext s)))
(defun mc:has-dia-cue-p (s / u)
  (setq u (mc:raw-upper s))
  (or (vl-string-search "Ø" u) (vl-string-search "%%C" u) (vl-string-search "DIA" u) (vl-string-search "DIAM" u))
)
(defun mc:has-radius-cue-p (s / u)
  (setq u (vl-string-trim " \t\r\n" (mc:raw-upper s)))
  (or (= (substr u 1 1) "R") (vl-string-search " R" u) (vl-string-search "RAD" u))
)
(defun mc:has-fcf-cue-p (s / u)
  (setq u (mc:raw-upper s))
  (or (vl-string-search " M" u) (vl-string-search " L" u) (vl-string-search " S" u)
      (vl-string-search " A" u) (vl-string-search " B" u) (vl-string-search " C" u)
      (vl-string-search "|" u))
)
(defun mc:text-kind (s isMetric / nums mx)
  (setq nums (if isMetric (mc:extract-conversion-numbers-metric s) (mc:extract-conversion-numbers s)))
  (setq mx (mc:max-num nums))
  (cond
    ((and mx (< mx 0.1)) 'TOL)
    ((and mx (mc:has-fcf-cue-p s) (< mx 1.0)) 'TOL)
    ((mc:has-dia-cue-p s) 'DIA)
    ((mc:has-radius-cue-p s) 'RAD)
    ((and mx (>= mx 0.1)) 'DIM)
    (T 'OTHER)
  )
)
(defun mc:same-text-kind-p (inchEntry metricEntry / ki km)
  (setq ki (mc:text-kind (car inchEntry) nil))
  (setq km (mc:text-kind (car metricEntry) T))
  (= ki km)
)

;;; -------------------------------------------------------------------
;;; Dimension collection
;;; -------------------------------------------------------------------
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

;;; -------------------------------------------------------------------
;;; Text / attribute collection
;;; -------------------------------------------------------------------
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
  (if (mc:ignored-block-p blkRef)
    result
    (progn
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
  )
)
(defun mc:get-texts (doc / ms cnt i obj oname result)
  (setq result nil ms (vla-get-ModelSpace doc) cnt (vla-get-Count ms) i 0)
  (while (< i cnt)
    (setq obj (vla-item ms i) oname (vla-get-ObjectName obj))
    (cond
      ((or (wcmatch oname "AcDbText") (wcmatch oname "AcDbMText") (wcmatch oname "AcDbAttribute") (wcmatch oname "AcDbAttributeDefinition"))
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
;;; Matching helpers
;;; -------------------------------------------------------------------
(defun mc:find-best-dim-match (iEntry metricList / expected best bestScore bestVDiff bestPDist mEntry mVal vDiff pDist score)
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
  (setq candidates nil iIdx 0)
  (foreach iVal inchNums
    (setq mIdx 0)
    (foreach mVal metricNums
      (if (and (numberp iVal) (numberp mVal))
        (progn
          (setq expected (* iVal *mc-conv*))
          (setq diff (abs (- mVal expected)))
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
    (if (and (not (mc:member-int (cadr cand) usedI)) (not (mc:member-int (caddr cand) usedM)))
      (progn
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
    (progn (setq total 0.0) (foreach p pairs (setq total (+ total (cadddr p)))) total)
    nil
  )
)
(defun mc:find-best-text-match (iEntry metricList / iNums mNums best bestScore bestVDiff bestPDist mEntry vDiff pDist score)
  ;; Clean value match only, category-safe.
  (setq iNums (mc:extract-conversion-numbers (car iEntry)) best nil bestScore nil bestVDiff nil bestPDist nil)
  (if iNums
    (foreach mEntry metricList
      (if (mc:same-text-kind-p iEntry mEntry)
        (progn
          (setq mNums (mc:extract-conversion-numbers-metric (car mEntry)))
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
    )
  )
  (if (and best bestVDiff (<= bestVDiff *mc-text-max-sane-diff*)) (list best bestVDiff bestPDist) nil)
)
(defun mc:find-nearest-text-match (iEntry metricList / best bestD d mEntry)
  ;; Category-safe positional fallback. This is what makes wrong paired attributes fail instead of ??,
  ;; without allowing DIA to match TOL.
  (setq best nil bestD nil)
  (foreach mEntry metricList
    (if (and (mc:same-text-kind-p iEntry mEntry) (mc:extract-conversion-numbers-metric (car mEntry)))
      (progn
        (setq d (mc:pos-distance-best (cadr iEntry) (cadr mEntry)))
        (if (and (< d *mc-text-position-limit*) (or (not bestD) (< d bestD)))
          (setq best mEntry bestD d)
        )
      )
    )
  )
  (if best (list best bestD) nil)
)
(defun mc:sort-by-pos (lst /)
  (vl-sort lst '(lambda (a b)
    (cond
      ((not (mc:xy-p (cadr a))) nil)
      ((not (mc:xy-p (cadr b))) T)
      ((< (caadr a) (caadr b)) T)
      ((and (equal (caadr a) (caadr b) 0.01) (< (cadadr a) (cadadr b))) T)
      (T nil)))))

;;; -------------------------------------------------------------------
;;; Layers / balloons
;;; -------------------------------------------------------------------
(defun mc:balloon-height (/ dtxt dscl sz)
  (setq dtxt (getvar "DIMTXT") dscl (getvar "DIMSCALE") sz (* (max dtxt 0.05) (max dscl 1.0) 0.85))
  (max sz 0.5)
)
(defun mc:ensure-layer (doc lname color / layers layerRes addRes)
  (setq layers (vla-get-Layers doc) layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (vl-catch-all-error-p layerRes)
    (progn (setq addRes (vl-catch-all-apply 'vla-add (list layers lname))) (if (not (vl-catch-all-error-p addRes)) (vla-put-Color addRes color)))
    (vla-put-Color layerRes color))
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
    (if ss (progn (setq i 0) (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))))
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
(defun mc:make-label (inchVal expected actual /) (strcat (mc:fmt inchVal) "\"" "  exp " (mc:fmt expected) "mm" "  got " (mc:fmt actual) "mm"))
(defun mc:make-pass-label (/) "\\U+2713")
(defun mc:make-missing-label (inchVal expected /) (strcat "??  " (mc:fmt inchVal) "\"" "  exp " (mc:fmt expected) "mm"))
(defun mc:make-unmatched-metric-label (metricVal /) (strcat "??  got " (mc:fmt metricVal) "mm"))
(defun mc:get-current-dwg-folder (/ p) (setq p (getvar "DWGPREFIX")) (if (or (not p) (= p "")) (setq p "")) p)

;;; ===================================================================
;;; c:metric_check
;;; ===================================================================
(defun c:metric_check
    (/ acadObj metricDoc metricDir inchFile inchOpen inchDoc inchIsDbx
       metricDims inchDims metricTexts inchTexts iDimLen mDimLen iTxtLen mTxtLen
       i iEntry mEntry matchInfo posInfo iVal mVal expected diff errPos passFlag missingPos
       iStr mStr iNums mNums pairs pair iNum mNum dimMarkers txtMarkers txtBody anyFail
       remainingMetricDims remainingMetricTexts balloonH errIdx dimPass dimFail txtPass txtFail
       missingDimCount unmatchedMetricDimCount textMissingCount ignoredMetricTexts)

  (vl-load-com)
  (setq acadObj (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj)
        metricDir (mc:get-current-dwg-folder))

  (princ "\nReading metric drawing...")
  (setq metricDims (mc:get-dims metricDoc))
  (setq metricTexts (mc:get-texts metricDoc))
  (princ (strcat " " (itoa (length metricDims)) " dim(s), " (itoa (length metricTexts)) " text/mtext/attribute found."))

  (setq inchFile (getfiled "Select Inch Source Drawing" metricDir "dwg" 4))
  (if (not inchFile) (progn (princ "\nmetric_check: Cancelled.") (princ) (exit)))

  (princ "\nOpening inch source invisibly using ObjectDBX when available...")
  (setq inchOpen (mc:open-inch-source-doc acadObj inchFile))
  (if (not inchOpen)
    (progn (princ "\nERROR: Could not open inch source drawing.") (princ) (exit))
  )
  (setq inchDoc (car inchOpen))
  (setq inchIsDbx (cadr inchOpen))
  (if inchIsDbx
    (princ " ObjectDBX OK, no visible inch drawing window.")
    (princ " ObjectDBX unavailable, used read-only visible fallback and will close it."))

  (princ "\nReading inch drawing...")
  (setq inchDims (mc:get-dims inchDoc))
  (setq inchTexts (mc:get-texts inchDoc))
  (princ (strcat " " (itoa (length inchDims)) " dim(s), " (itoa (length inchTexts)) " text/mtext/attribute found."))

  (mc:close-inch-source-doc inchDoc inchIsDbx)
  (vla-Activate metricDoc)

  (setq metricDims (mc:sort-by-pos metricDims))
  (setq inchDims (mc:sort-by-pos inchDims))
  (setq metricTexts (mc:sort-by-pos metricTexts))
  (setq inchTexts (mc:sort-by-pos inchTexts))

  (setq dimMarkers nil txtMarkers nil dimPass 0 dimFail 0 txtPass 0 txtFail 0
        missingDimCount 0 unmatchedMetricDimCount 0 textMissingCount 0 ignoredMetricTexts 0)
  (setq mDimLen (length metricDims) iDimLen (length inchDims) mTxtLen (length metricTexts) iTxtLen (length inchTexts))

  (if (/= mDimLen iDimLen)
    (princ (strcat "\nInfo: dimension count differs: " (itoa iDimLen) " inch vs " (itoa mDimLen) " metric.")))
  (if (/= mTxtLen iTxtLen)
    (princ (strcat "\nInfo: text/attribute count differs: " (itoa iTxtLen) " inch vs " (itoa mTxtLen) " metric. Ignored blocks: C, REVSYMB, REVC.")))

  ;; --------------------------- dimension check -----------------------
  (setq remainingMetricDims metricDims i 0)
  (repeat iDimLen
    (setq iEntry (nth i inchDims)
          iVal (car iEntry)
          expected (if (numberp iVal) (* iVal *mc-conv*) nil)
          matchInfo (mc:find-best-dim-match iEntry remainingMetricDims))
    (if matchInfo
      (progn
        (setq mEntry (car matchInfo) diff (cadr matchInfo) mVal (car mEntry) errPos (cadr mEntry))
        (setq passFlag (and (numberp mVal) (numberp expected) (<= diff *mc-tolerance*)))
        (if passFlag (setq dimPass (1+ dimPass)) (setq dimFail (1+ dimFail)))
        (setq dimMarkers (cons (list passFlag (if passFlag (mc:make-pass-label) (mc:make-label iVal expected mVal)) errPos) dimMarkers))
        (setq remainingMetricDims (mc:remove-first mEntry remainingMetricDims))
      )
      (progn
        (setq missingDimCount (1+ missingDimCount) dimFail (1+ dimFail))
        (setq missingPos (mc:scale-point (cadr iEntry) *mc-conv*))
        (if (not (mc:xy-p missingPos)) (setq missingPos (cadr iEntry)))
        (if (mc:xy-p missingPos)
          (setq dimMarkers (cons (list nil (mc:make-missing-label iVal expected) missingPos) dimMarkers))
        )
      )
    )
    (setq i (1+ i))
  )

  (foreach mEntry remainingMetricDims
    (setq unmatchedMetricDimCount (1+ unmatchedMetricDimCount) dimFail (1+ dimFail))
    (setq dimMarkers (cons (list nil (mc:make-unmatched-metric-label (car mEntry)) (cadr mEntry)) dimMarkers))
  )

  ;; ------------------------ text/attribute check ---------------------
  ;; Clean value match first. If not found, category-safe positional fallback.
  ;; Positional fallback produces WRONG VALUE label, not ??.
  (setq remainingMetricTexts metricTexts i 0)
  (repeat iTxtLen
    (setq iEntry (nth i inchTexts)
          iStr (car iEntry)
          iNums (mc:extract-conversion-numbers iStr))
    (if iNums
      (progn
        (setq matchInfo (mc:find-best-text-match iEntry remainingMetricTexts))
        (if (not matchInfo)
          (setq posInfo (mc:find-nearest-text-match iEntry remainingMetricTexts))
          (setq posInfo nil)
        )
        (if matchInfo
          (setq mEntry (car matchInfo))
          (if posInfo (setq mEntry (car posInfo)) (setq mEntry nil))
        )
        (if mEntry
          (progn
            (setq mStr (car mEntry)
                  errPos (cadr mEntry)
                  mNums (mc:extract-conversion-numbers-metric mStr)
                  pairs (mc:global-best-number-pairs iNums mNums))
            (if pairs
              (progn
                (setq txtBody nil anyFail nil)
                (foreach pair pairs
                  (setq iNum (car pair) mNum (cadr pair) expected (caddr pair) diff (cadddr pair))
                  (if (> diff *mc-tolerance*)
                    (setq anyFail T txtFail (1+ txtFail))
                    (setq txtPass (1+ txtPass))
                  )
                  (setq txtBody (if txtBody (strcat txtBody "  |  " (mc:make-label iNum expected mNum)) (mc:make-label iNum expected mNum)))
                )
                (setq txtMarkers (cons (list (not anyFail) (if (not anyFail) (mc:make-pass-label) txtBody) errPos) txtMarkers))
              )
              (progn
                (setq textMissingCount (1+ textMissingCount) txtFail (1+ txtFail))
                (setq txtMarkers (cons (list nil (mc:make-missing-label (car iNums) (* (car iNums) *mc-conv*)) (cadr iEntry)) txtMarkers))
              )
            )
            (setq remainingMetricTexts (mc:remove-first mEntry remainingMetricTexts))
          )
          (progn
            (setq textMissingCount (1+ textMissingCount) txtFail (1+ txtFail))
            (setq txtMarkers (cons (list nil (mc:make-missing-label (car iNums) (* (car iNums) *mc-conv*)) (cadr iEntry)) txtMarkers))
          )
        )
      )
    )
    (setq i (1+ i))
  )

  ;; Leftover metric text/attrs: only report if still dimensional and category-valid.
  (foreach mEntry remainingMetricTexts
    (setq mNums (mc:extract-conversion-numbers-metric (car mEntry)))
    (if mNums
      (progn
        (setq txtFail (1+ txtFail))
        (setq txtMarkers (cons (list nil (mc:make-unmatched-metric-label (car mNums)) (cadr mEntry)) txtMarkers))
      )
      (setq ignoredMetricTexts (1+ ignoredMetricTexts))
    )
  )

  ;; ----------------------------- output ------------------------------
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
    (if (not (car m)) (progn (princ (strcat "\n  [" (itoa errIdx) "] TXT/ATTR FAIL/MISSING: " (cadr m))) (setq errIdx (1+ errIdx))))
  )

  (if (zerop (+ dimFail txtFail)) (princ "\nAll checked conversions PASSED."))
  (vla-Regen metricDoc 2)

  (princ
    (strcat
      "\n--------------------------------------------\n"
      "METRIC CHECK DONE v14.4-FINAL-PRO\n"
      "  Dimensions : " (itoa dimPass) " pass  " (itoa dimFail) " fail/missing\n"
      "  Text/MText/Attr : " (itoa txtPass) " pass  " (itoa txtFail) " fail/missing\n"
      "  Missing inch dimension matches : " (itoa missingDimCount) "\n"
      "  Unmatched metric dimensions   : " (itoa unmatchedMetricDimCount) "\n"
      "  Suspicious/missing text/attr  : " (itoa textMissingCount) "\n"
      "  Ignored non-dimensional metric text/attr : " (itoa ignoredMetricTexts) "\n"
      "  Ignored block names : C, REVSYMB, REVC\n"
      "  Inch source open mode : " (if inchIsDbx "ObjectDBX invisible" "visible fallback read-only") "\n"
      "  Key fixes : no DIA/TOL cross-match; wrong paired attributes fail, not ??\n"
      "--------------------------------------------"
    )
  )
  (princ)
)

(defun c:metric_clear (/)
  (vl-load-com)
  (mc:clear-qc-layers)
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 2)
  (princ "\nMC_PASS and MC_ERRORS balloons and layers removed.")
  (princ)
)

(princ "\nMETRIC_CHECK.LSP v14.4-FINAL-PRO loaded.")
(princ "\n  METRIC_CHECK -- ObjectDBX no-window source read, category-safe matching")
(princ "\n  METRIC_CLEAR -- erase all QC balloons and layers")
(princ)
