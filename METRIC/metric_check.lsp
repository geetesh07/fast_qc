;;; =====================================================================
;;; METRIC_CHECK.LSP  v18.7
;;; Commands: METRIC_CHECK / METRIC_CLEAR / MC_SELFTEST / MC_DIAG
;;;
;;; v18.7 over v18.6:
;;;   Position-rescue pass for unmatched dims: after value-based matching,
;;;   remaining inch and metric dims are paired by visual proximity (500mm
;;;   threshold in metric coordinate space).  NTS dims that share the same
;;;   drawing location now show ONE labelled FAIL balloon ("703\" exp 17856mm
;;;   got 7030mm") instead of two separate "??" balloons.  Summary reports
;;;   rescue count separately from truly missing/unmatched dims.
;;;
;;; v18.6 over v18.5:
;;;   Bilateral tolerance fix: text starting with "+" (e.g. "+0.38" from a
;;;   surface-finish or hole callout attribute) now classified as TOL regardless
;;;   of magnitude.  Previously "+0.38" (≥0.1) was DIM while "+0.015" (<0.1)
;;;   was TOL — kind mismatch prevented pairing of matching bilateral tolerances.
;;;
;;; v18.5 over v18.4:
;;;   GD&T feature-control-frame fix: Ø/%%C before a tolerance value with
;;;   GD&T modifiers (M)/(L)/(S)/(F)/(P) or pipe | now correctly classified
;;;   as TOL instead of DIA.  Previously ".03 (M) A B C" → TOL and
;;;   "Ø 0.76 (S) A C" → DIA caused a kind-mismatch so the pair was never
;;;   evaluated.  isGdtFrame flag now checked BEFORE the %%C→DIA rule.
;;;
;;; v18.4 over v18.3:
;;;   vlax-get-property replaces vla-get-DimLFac (type-lib version safety).
;;;   Blocks collection scan replaces depth-limited traversal.
;;;   Dedup + sanity gate added.  NTS upper-limit filter removed.
;;; =====================================================================

(vl-load-com)

;;; -------------------------------------------------------------------
;;; Settings
;;; -------------------------------------------------------------------
(setq *mc-conv*                  25.4)
(setq *mc-tolerance*             0.5)   ; mm — relaxed from 0.1 to accept metric rounding (e.g. 2.0"=50.8mm→51mm)
(setq *mc-text-max-sane-diff*    2.0)
(setq *mc-text-position-limit*   75.0)
(setq *mc-max-inch-text-value*   1000.0)
(setq *mc-max-metric-text-value* 25000.0)
(setq *mc-position-weight-pass*  0.001)
(setq *mc-position-weight-fail*  0.000001)
;;; No hard upper limit on dim values — drawings may contain intentionally
;;; large reference or NTS (not-to-scale) dimensions.
(setq *mc-ignored-block-names*
  (list "C" "D" "KF" "REVSYMB" "REVC" "REVTRI" "REVCIRCLE"
        "TITLEBLOCK" "BORDER" "TB" "TITLE" "REVISION" "REVCLOUD" "FRAME"))
(setq *mc-active-inch-doc* nil)
(setq *mc-active-inch-dbx*  nil)

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
    (setq s (substr s 1 (1- (strlen s)))))
  (if (and (> (strlen s) 1) (= (substr s (strlen s) 1) "."))
    (setq s (substr s 1 (1- (strlen s)))))
  s
)
(defun mc:fmt (v /)
  (if (numberp v) (mc:rtrim0 (rtos v 2 4)) "??")
)
(defun mc:distance (p1 p2 / dx dy)
  (if (and (mc:xy-p p1) (mc:xy-p p2))
    (progn
      (setq dx (- (car p1) (car p2)) dy (- (cadr p1) (cadr p2)))
      (sqrt (+ (* dx dx) (* dy dy))))
    1.0e99)
)
(defun mc:scale-point (p sc /)
  (if (and (mc:xy-p p) (numberp sc))
    (list (* (car p) sc) (* (cadr p) sc))
    nil)
)
(defun mc:safearray-point (variantValue / res)
  (setq res (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value variantValue))))
  (if (and (not (vl-catch-all-error-p res)) (mc:xy-p res))
    (list (car res) (cadr res))
    nil)
)
(defun mc:pos-distance-best (inchPos metricPos / scaled dScaled dRaw)
  (if (and (mc:xy-p inchPos) (mc:xy-p metricPos))
    (progn
      (setq scaled  (mc:scale-point inchPos *mc-conv*)
            dScaled (mc:distance scaled metricPos)
            dRaw    (mc:distance inchPos metricPos))
      (if (< dScaled dRaw) dScaled dRaw))
    1.0e99)
)
(defun mc:remove-first (item lst / result removed x)
  (setq result nil removed nil)
  (foreach x lst
    (if (and (not removed) (equal x item 1e-8))
      (setq removed T)
      (setq result (cons x result))))
  (reverse result)
)
(defun mc:member-int (n lst / found x)
  (setq found nil)
  (foreach x lst (if (= x n) (setq found T)))
  found
)
;;; Remove duplicate dim entries by value.
;;; Two entries are "same" if their values are within dupTol of each other.
;;; This eliminates double-counting when the same dim lives in multiple block
;;; definitions (e.g., both the left-view block and right-view block have an
;;; identical 9.3735" dim — we only want to compare it once).
(defun mc:dedup-dims (lst dupTol / result seen e v found s)
  (setq result nil seen nil)
  (foreach e lst
    (setq v (car e) found nil)
    (if (numberp v)
      (progn
        (foreach s seen (if (equal v s dupTol) (setq found T)))
        (if (not found)
          (setq result (cons e result) seen (cons v seen)))))  )
  (reverse result)
)
(defun mc:str-upper (s /)
  (if (= (type s) 'STR) (strcase s) "")
)
(defun mc:normalize-control-codes (s / u)
  (setq u (mc:str-upper s))
  (while (vl-string-search "\\U+00D8" u) (setq u (vl-string-subst "%%C" "\\U+00D8" u)))
  (while (vl-string-search "\\U+2300" u) (setq u (vl-string-subst "%%C" "\\U+2300" u)))
  (while (vl-string-search "\\U+00B1" u) (setq u (vl-string-subst "+/-" "\\U+00B1" u)))
  (while (vl-string-search "Ø"       u) (setq u (vl-string-subst "%%C" "Ø"        u)))
  (while (vl-string-search "⌀"       u) (setq u (vl-string-subst "%%C" "⌀"        u)))
  (while (vl-string-search "±"       u) (setq u (vl-string-subst "+/-" "±"        u)))
  u
)
(defun mc:contains-any (s patterns / hit p u)
  (setq hit nil u (mc:normalize-control-codes s))
  (foreach p patterns (if (vl-string-search p u) (setq hit T)))
  hit
)

;;; -------------------------------------------------------------------
;;; ObjectDBX
;;; -------------------------------------------------------------------
(defun mc:create-objectdbx-doc (acadObj / versions prog dbx)
  (setq versions (list "ObjectDBX.AxDbDocument.25" "ObjectDBX.AxDbDocument.24"
                       "ObjectDBX.AxDbDocument.23" "ObjectDBX.AxDbDocument.22"
                       "ObjectDBX.AxDbDocument.21" "ObjectDBX.AxDbDocument.20"
                       "ObjectDBX.AxDbDocument.19" "ObjectDBX.AxDbDocument.18"
                       "ObjectDBX.AxDbDocument")
        dbx nil)
  (foreach prog versions
    (if (not dbx)
      (progn
        (setq dbx (vl-catch-all-apply 'vla-GetInterfaceObject (list acadObj prog)))
        (if (vl-catch-all-error-p dbx) (setq dbx nil)))))
  dbx
)
(defun mc:open-inch-source-doc (acadObj inchFile / dbx openRes visibleRes)
  (setq dbx (mc:create-objectdbx-doc acadObj))
  (if dbx
    (progn
      (setq openRes (vl-catch-all-apply 'vla-open (list dbx inchFile)))
      (if (vl-catch-all-error-p openRes)
        (progn
          (vl-catch-all-apply 'vlax-release-object (list dbx))
          (setq visibleRes (vl-catch-all-apply 'vla-open
                             (list (vla-get-Documents acadObj) inchFile vlax-true)))
          (if (vl-catch-all-error-p visibleRes) nil (list visibleRes nil)))
        (list dbx T)))
    (progn
      (setq visibleRes (vl-catch-all-apply 'vla-open
                         (list (vla-get-Documents acadObj) inchFile vlax-true)))
      (if (vl-catch-all-error-p visibleRes) nil (list visibleRes nil))))
)
(defun mc:close-inch-source-doc (doc isDbx /)
  (if doc
    (if isDbx
      (vl-catch-all-apply 'vlax-release-object (list doc))
      (vl-catch-all-apply 'vla-close (list doc vlax-false))))
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
      (if (and (not (vl-catch-all-error-p nRes)) (= (type nRes) 'STR)) nRes "")))
)
(defun mc:ignored-block-p (blkRef / bname)
  (setq bname (strcase (mc:block-name blkRef)))
  (wcmatch bname
    "C,D,KF,REVSYMB,REVC,REVTRI,REVCIRCLE,TITLEBLOCK,BORDER,TB,TITLE,REVISION,REVCLOUD,FRAME")
)

;;; -------------------------------------------------------------------
;;; MTEXT strip — all escape sequences
;;; -------------------------------------------------------------------
(defun mc:strip-mtext (s / res i len c nc depth skipSemi sc)
  (setq res "" len (strlen s) i 1 depth 0 skipSemi nil)
  (while (<= i len)
    (setq c (substr s i 1))
    (cond
      ((= c "{")
       (setq depth (1+ depth))
       (if (and (< i len) (= (substr s (1+ i) 1) "\\")) (setq skipSemi T)))
      ((= c "}")
       (if (> depth 0) (setq depth (- depth 1)))
       (setq skipSemi nil))
      ((and skipSemi (not (= c ";"))) nil)
      ((and skipSemi (= c ";")) (setq skipSemi nil))
      ((= c "\\")
       (if (<= (1+ i) len)
         (progn
           (setq nc (strcase (substr s (1+ i) 1)))
           (cond
             ((wcmatch nc "P,N,~")
              (setq res (strcat res " ") i (1+ i)))
             ((= nc "S")
              (setq sc (+ i 2))
              (while (and (<= sc len) (/= (substr s sc 1) ";")) (setq sc (1+ sc)))
              (setq res (strcat res " ") i sc))
             ((wcmatch nc "F,H,A,C,W,Q,T")
              (setq sc (+ i 2))
              (while (and (<= sc len) (/= (substr s sc 1) ";")) (setq sc (1+ sc)))
              (setq i sc))
             ((and (= nc "U") (<= (+ i 2) len) (= (substr s (+ i 2) 1) "+"))
              (setq res (strcat res " ") i (+ i 6)))
             ((wcmatch nc "L,O,K,X")
              (setq i (1+ i)))
             ((= nc "\\")
              (setq res (strcat res "\\") i (1+ i)))
             (T nil)))))
      ((and (= c "%") (<= (1+ i) len) (= (substr s (1+ i) 1) "%"))
       (setq i (+ i 2)))
      (T (setq res (strcat res c))))
    (setq i (1+ i)))
  res
)

;;; -------------------------------------------------------------------
;;; Numeric extraction
;;; -------------------------------------------------------------------
(defun mc:extract-number-pairs (str / result i len c token hadDot hadDigitAfterDot stopToken sign nextc)
  (setq result nil len (strlen str) i 1)
  (while (<= i len)
    (setq c (substr str i 1) sign "")
    (if (and (or (= c "+") (= c "-")) (< i len)
             (or (mc:is-digit (substr str (1+ i) 1)) (= (substr str (1+ i) 1) ".")))
      (progn
        (setq nextc (substr str (1+ i) 1))
        (if (or (= i 1) (not (mc:is-digit (substr str (1- i) 1))))
          (progn (setq sign c i (1+ i) c nextc)))))
    (cond
      ((mc:is-digit c)
       (setq token (strcat sign c) hadDot nil hadDigitAfterDot nil stopToken nil i (1+ i))
       (while (and (<= i len) (not stopToken)
                   (or (mc:is-digit (substr str i 1)) (= (substr str i 1) ".")))
         (setq c (substr str i 1))
         (cond
           ((mc:is-digit c)
            (setq token (strcat token c))
            (if hadDot (setq hadDigitAfterDot T)))
           ((= c ".")
            (if hadDot
              (setq stopToken T)
              (progn (setq token (strcat token c) hadDot T)))))
         (if (not stopToken) (setq i (1+ i))))
       (if (not (and hadDot (not hadDigitAfterDot)))
         (setq result (cons (list (atof token) hadDot token) result)))
       (setq i (1- i)))
      ((and (= c ".") (< i len) (mc:is-digit (substr str (1+ i) 1)))
       (setq token (strcat sign "0.") i (+ i 1))
       (while (and (<= i len) (mc:is-digit (substr str i 1)))
         (setq token (strcat token (substr str i 1)) i (1+ i)))
       (setq result (cons (list (atof token) T token) result))
       (setq i (1- i))))
    (setq i (1+ i)))
  (reverse result)
)
(defun mc:first-preferred-number (str / pairs p decimalVal intVal)
  (setq pairs (mc:extract-number-pairs str) decimalVal nil intVal nil)
  (foreach p pairs
    (if (and (listp p) (numberp (car p)))
      (cond
        ((and (cadr p) (not decimalVal)) (setq decimalVal (car p)))
        ((and (not (cadr p)) (not intVal)) (setq intVal (car p))))))
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
  (setq u (mc:normalize-control-codes (vl-string-trim " \t\r\n" s)))
  (or (mc:contains-any u (list "%%C" " DIA" "DIAM" " R" "R." "RAD" " THRU" "+/-"))
      (= (substr u 1 1) "R"))
)
(defun mc:single-number-string-p (s / pairs stripped ok ch)
  (setq stripped (vl-string-trim " \t\r\n()[]{}\"'" (mc:strip-mtext s)))
  (setq pairs (mc:extract-number-pairs stripped) ok T)
  (foreach ch (mapcar 'chr (vl-string->list stripped))
    (if (not (or (mc:is-digit ch) (= ch ".") (= ch "+") (= ch "-") (= ch ",")))
      (setq ok nil)))
  (and (= (length pairs) 1) ok)
)
(defun mc:only-dimlike-integer-p (s / u cleaned pairs allDigits rem ch)
  (setq u (strcase (mc:strip-mtext s)) pairs (mc:extract-number-pairs u))
  (if (and (= (length pairs) 1) (numberp (caar pairs)) (not (cadar pairs)))
    (progn
      (setq cleaned u)
      (foreach rem (list "(" ")" "[" "]" "{" "}" "\"" "'" "+" "-"
                         "R" "DIA" "DIAM" "DIAMETER" "Ø" "%%C" "⌀" " " "\t" "\r" "\n")
        (setq cleaned (vl-string-subst "" rem cleaned)))
      (setq allDigits T)
      (foreach ch (mapcar 'chr (vl-string->list cleaned))
        (if (not (mc:is-digit ch)) (setq allDigits nil)))
      (and (> (strlen cleaned) 0) allDigits))
    nil)
)
(defun mc:number-sane-for-text-p (v isMetric / av)
  (setq av (if (numberp v) (abs v) nil))
  (and av (> av 0.0)
       (if isMetric (< av *mc-max-metric-text-value*) (< av *mc-max-inch-text-value*)))
)
(defun mc:extract-conversion-numbers-core (str isMetric / result pairs p val isDec token raw)
  (setq result nil raw (mc:strip-mtext str) pairs (mc:extract-number-pairs raw))
  (foreach p pairs
    (setq val (car p) isDec (cadr p) token (caddr p))
    (if (and (mc:number-sane-for-text-p val isMetric)
             (not (and (not (mc:dim-cue-p raw)) (mc:bad-titleblock-word-p raw))))
      (cond
        ((and isDec (= (substr token 1 2) "0.")) (setq result (cons val result)))
        ((and isDec (or (mc:dim-cue-p raw) (mc:single-number-string-p raw))) (setq result (cons val result)))
        ((and (not isDec) (mc:only-dimlike-integer-p raw)) (setq result (cons val result))))))
  (reverse result)
)
(defun mc:extract-conversion-numbers        (str /) (mc:extract-conversion-numbers-core str nil))
(defun mc:extract-conversion-numbers-metric (str /) (mc:extract-conversion-numbers-core str T))

;;; -------------------------------------------------------------------
;;; Text category — one raw-upper pass, DIA/RAD before numeric threshold
;;; -------------------------------------------------------------------
(defun mc:max-num (nums / m n)
  (setq m nil)
  (foreach n nums (if (numberp n) (if (or (not m) (> n m)) (setq m n))))
  m
)
(defun mc:raw-upper (s /) (mc:normalize-control-codes (mc:strip-mtext s)))

(defun mc:text-kind-from-nums (s nums / mx u trimU isGdtFrame)
  ;; Compute mc:raw-upper once and reuse for all cue checks.
  (setq mx (mc:max-num nums) u (mc:raw-upper s) trimU (vl-string-trim " \t\r\n" u))
  ;; ---------------------------------------------------------------
  ;; GD&T feature-control-frame detection — MUST run before %%C→DIA.
  ;; In GD&T, Ø/%%C before a tolerance value means "cylindrical tolerance
  ;; zone", NOT a diameter dimension.  Inch FCF annotations often omit the
  ;; Ø prefix entirely (e.g. ".03 (M) A B C") while the metric version adds
  ;; it ("Ø 0.76 (S) A C").  Without this check both sides must have %%C or
  ;; both must omit it, or they end up with different kinds (DIA vs TOL) and
  ;; the matcher never pairs them.
  ;;
  ;; GD&T-specific modifiers that are unambiguous FCF markers:
  ;;   (M) = Maximum Material Condition    (L) = Least Material Condition
  ;;   (S) = Statistical / RFS            (F) = Free State
  ;;   (P) = Projected tolerance zone     |   = FCF cell separator
  ;; ---------------------------------------------------------------
  (setq isGdtFrame
    (or (vl-string-search "(M)" u)
        (vl-string-search "(L)" u)
        (vl-string-search "(S)" u)
        (vl-string-search "(F)" u)
        (vl-string-search "(P)" u)
        (vl-string-search "|"   u)))
  (cond
    ;; GD&T FCF — Ø/%%C means cylindrical zone, not a diameter → TOL
    ((and isGdtFrame mx) 'TOL)
    ;; Signed bilateral tolerance: text starts with "+" (e.g. attribute "+0.38"
    ;; from a surface-finish or hole callout block, or "+.015" from a tolerance
    ;; note).  A leading "+" in drawing annotations is exclusively a plus-
    ;; tolerance, never a standalone dimension.  Must classify as TOL regardless
    ;; of numeric magnitude so it pairs with the inch counterpart (which may be
    ;; small enough to already be TOL via the < 0.1 rule).
    ((and mx (= (substr trimU 1 1) "+")) 'TOL)
    ;; Plain diameter symbol without FCF context → diameter dimension
    ((or (vl-string-search "%%C"  u)
         (vl-string-search "DIA"  u)
         (vl-string-search "DIAM" u)) 'DIA)
    ((or (= (substr trimU 1 1) "R")
         (vl-string-search " R"  trimU)
         (vl-string-search "RAD" trimU)) 'RAD)
    ((and mx (< mx 1.0)
          (or (vl-string-search " M" u) (vl-string-search " L" u)
              (vl-string-search " S" u) (vl-string-search " A" u)
              (vl-string-search " B" u) (vl-string-search " C" u)
              (vl-string-search "|" u))) 'TOL)
    ((and mx (< mx 0.1)) 'TOL)
    ((and mx (>= mx 0.1)) 'DIM)
    (T 'OTHER))
)
(defun mc:has-dia-cue-p (s / u)
  (setq u (mc:raw-upper s))
  (or (vl-string-search "%%C" u) (vl-string-search "DIA" u) (vl-string-search "DIAM" u))
)
(defun mc:has-radius-cue-p (s / u)
  (setq u (vl-string-trim " \t\r\n" (mc:raw-upper s)))
  (or (= (substr u 1 1) "R") (vl-string-search " R" u) (vl-string-search "RAD" u))
)
(defun mc:text-kind (s isMetric / nums)
  (setq nums (if isMetric (mc:extract-conversion-numbers-metric s) (mc:extract-conversion-numbers s)))
  (mc:text-kind-from-nums s nums)
)
(defun mc:same-text-kind-p (inchEntry metricEntry /)
  (= (mc:text-kind (car inchEntry) nil) (mc:text-kind (car metricEntry) T))
)

;;; -------------------------------------------------------------------
;;; Text enrichment — pre-cache nums + kind for speed
;;; entry format after enrichment: (text pos nums kind)
;;; -------------------------------------------------------------------
(defun mc:enrich-text-entry (entry isMetric / txt nums)
  (setq txt  (car entry)
        nums (if isMetric (mc:extract-conversion-numbers-metric txt)
                          (mc:extract-conversion-numbers txt)))
  (list txt (cadr entry) nums (mc:text-kind-from-nums txt nums))
)

;;; -------------------------------------------------------------------
;;; Dimension collection helpers
;;; -------------------------------------------------------------------
(defun mc:linear-dim-p (oname)
  ;; Ordinate dims return an absolute coordinate from vla-get-Measurement, not a length.
  ;; Angular dims return degrees. Both are excluded.
  (and (wcmatch oname "*Dimension*")
       (not (wcmatch oname "*Angular*"))
       (not (wcmatch oname "*Ordinate*")))
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
          (setq stripped (mc:strip-mtext txt) val (mc:first-preferred-number stripped))
          (if (numberp val) val nil)))))
)
;;; Read DIMLFAC from a dimension object via COM dispatch.
;;; vla-get-DimLFac is not generated in all AutoCAD type libraries, so use
;;; vlax-get-property (raw dispatch, version-agnostic) instead.
;;; DXF group code 41 in DIMENSION entities is NOT DIMLFAC — it stores rotation
;;; angle or leader length depending on dim type — never use it as a fallback.
(defun mc:get-dim-lfac (obj / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj "DimLFac")))
  (if (and (not (vl-catch-all-error-p r))
           (numberp r)
           ;; Sanity: real DIMLFAC is in [0.001, 1000].
           ;; Values outside this range are almost always a mis-read
           ;; (e.g., a rotation angle in radians returned as the property).
           (>= (abs r) 0.001)
           (<= (abs r) 1000.0))
    r
    1.0)
)

(defun mc:get-dim-value (obj / overrideVal measRes meas lfac)
  (setq overrideVal (mc:get-dim-override-value obj))
  (if (numberp overrideVal)
    overrideVal
    (progn
      (setq measRes (vl-catch-all-apply 'vla-get-Measurement (list obj)))
      (if (and (not (vl-catch-all-error-p measRes)) (numberp measRes))
        (progn
          (setq meas (abs measRes)
                lfac (mc:get-dim-lfac obj))
          ;; Apply DIMLFAC: drawings where geometry is in mm but dim style
          ;; shows inches use DimLFac ≠ 1.0. Without this, measurements are
          ;; in the wrong unit and every dim value is wrong.
          (if (and (> (abs lfac) 1e-10)
                   (not (equal lfac 1.0 1e-6)))
            (setq meas (* meas lfac)))
          (if (> meas 0.0) meas nil))
        nil)))
)
(defun mc:dim-geometry-point (obj / p1Res p2Res p1 p2 txtRes txtPos)
  (setq p1 nil p2 nil txtPos nil
        p1Res (vl-catch-all-apply 'vla-get-ExtLine1Point (list obj))
        p2Res (vl-catch-all-apply 'vla-get-ExtLine2Point (list obj)))
  (if (and (not (vl-catch-all-error-p p1Res)) (not (vl-catch-all-error-p p2Res)))
    (progn (setq p1 (mc:safearray-point p1Res) p2 (mc:safearray-point p2Res))))
  (if (and p1 p2)
    (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0))
    (progn
      (setq txtRes (vl-catch-all-apply 'vla-get-TextPosition (list obj)))
      (if (not (vl-catch-all-error-p txtRes)) (setq txtPos (mc:safearray-point txtRes)))
      txtPos))
)

;;; Collect dims from any iterable space object (modelspace, layout block, block def)
(defun mc:collect-dims-in-space (spaceObj result / obj oname dimVal pos)
  (vlax-for obj spaceObj
    (setq oname (vla-get-ObjectName obj))
    (if (mc:linear-dim-p oname)
      (progn
        (setq dimVal (mc:get-dim-value obj) pos (mc:dim-geometry-point obj))
        (if (and (numberp dimVal) (mc:xy-p pos))
          (setq result (cons (list dimVal pos) result))))))
  result
)

;;; Collect dims from a block reference — applies full affine transform
(defun mc:collect-dims-from-blockref (blkRef blocksColl result
                                      / bname blkDef insRes scXRes scYRes rotRes
                                        ins scX scY rot cos-r sin-r obj oname dimVal pos wx wy)
  (setq bname (mc:block-name blkRef))
  (if (and bname (> (strlen bname) 0)
           (not (= (substr bname 1 1) "*"))
           (not (mc:ignored-block-p blkRef)))
    (progn
      (setq blkDef (vl-catch-all-apply 'vla-item (list blocksColl bname)))
      (if (not (vl-catch-all-error-p blkDef))
        (progn
          (setq insRes (vl-catch-all-apply 'vla-get-InsertionPoint (list blkRef))
                scXRes (vl-catch-all-apply 'vla-get-XScaleFactor   (list blkRef))
                scYRes (vl-catch-all-apply 'vla-get-YScaleFactor   (list blkRef))
                rotRes (vl-catch-all-apply 'vla-get-Rotation       (list blkRef)))
          (if (not (vl-catch-all-error-p insRes))
            (progn
              (setq ins   (mc:safearray-point insRes)
                    scX   (if (or (vl-catch-all-error-p scXRes) (not (numberp scXRes))) 1.0 scXRes)
                    scY   (if (or (vl-catch-all-error-p scYRes) (not (numberp scYRes))) 1.0 scYRes)
                    rot   (if (or (vl-catch-all-error-p rotRes) (not (numberp rotRes))) 0.0 rotRes)
                    cos-r (cos rot)
                    sin-r (sin rot))
              (if (mc:xy-p ins)
                (vlax-for obj blkDef
                  (setq oname (vla-get-ObjectName obj))
                  (if (mc:linear-dim-p oname)
                    (progn
                      (setq dimVal (mc:get-dim-value obj) pos (mc:dim-geometry-point obj))
                      (if (and (numberp dimVal) (mc:xy-p pos))
                        (progn
                          (setq wx (+ (car ins)
                                      (* scX (- (* (car pos) cos-r) (* (cadr pos) sin-r))))
                                wy (+ (cadr ins)
                                      (* scY (+ (* (car pos) sin-r) (* (cadr pos) cos-r)))))
                          (setq result (cons (list dimVal (list wx wy)) result))))))))))))))
  result
)

;;; Main dim reader — scans EVERY block definition in the document.
;;;
;;; Why this approach:
;;;   The old code traversed modelspace + 1 level of block refs + paper-space
;;;   layouts. That misses dims that are 2+ block-nesting levels deep, which
;;;   is common in multi-view mechanical drawings (view→detail block→dim).
;;;
;;;   Scanning the Blocks collection directly covers all depths without any
;;;   recursion: every dim entity, wherever it lives, belongs to exactly one
;;;   block definition, and we visit every block definition once.
;;;
;;; What we include:
;;;   *Model_Space  — modelspace dims (direct)
;;;   *Paper_Space* — dims placed directly in layout sheets
;;;   Named user blocks — any named block that isn't in the ignore list
;;;
;;; What we exclude:
;;;   *D... anonymous blocks (dynamic block instances) — dims inside are
;;;      display-only clones; reading them gives duplicate / wrong values
;;;   Other *-prefix internal blocks — same reason
;;;   Named ignored blocks — border, title block, revision symbols, D, KF, etc.
(defun mc:get-dims (doc / blocksColl blkDef bname bnu obj oname dimVal pos result)
  (setq result nil blocksColl (vla-get-Blocks doc))
  (vlax-for blkDef blocksColl
    (setq bname (vl-catch-all-apply 'vla-get-Name (list blkDef)))
    (if (and (not (vl-catch-all-error-p bname))
             (= (type bname) 'STR)
             (> (strlen bname) 0))
      (progn
        (setq bnu (strcase bname))
        (if (or
              ;; Always include model space and all paper-space blocks
              (wcmatch bnu "*MODEL*SPACE*")
              (wcmatch bnu "*PAPER*SPACE*")
              ;; Include named user blocks — skip anonymous (*-prefix) blocks
              ;; and skip the named ones on the ignore list
              (and (not (= (substr bname 1 1) "*"))
                   (not (wcmatch bnu
                          "C,D,KF,REVSYMB,REVC,REVTRI,REVCIRCLE,TITLEBLOCK,BORDER,TB,TITLE,REVISION,REVCLOUD,FRAME"))))
          (vlax-for obj blkDef
            (setq oname (vla-get-ObjectName obj))
            (if (mc:linear-dim-p oname)
              (progn
                (setq dimVal (mc:get-dim-value obj)
                      pos    (mc:dim-geometry-point obj))
                (if (and (numberp dimVal) (mc:xy-p pos))
                  (setq result (cons (list dimVal pos) result))))))))))
  result
)

;;; -------------------------------------------------------------------
;;; Text / attribute / MLeader collection
;;; -------------------------------------------------------------------
(defun mc:get-object-insertion-xy (obj / posRes pos)
  (setq posRes (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p posRes)
    nil
    (progn
      (setq pos (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value posRes))))
      (if (and (not (vl-catch-all-error-p pos)) (mc:xy-p pos)) (list (car pos) (cadr pos)) nil)))
)
(defun mc:add-textlike-object (obj result / oname txtRes pos txt)
  (setq oname (vla-get-ObjectName obj) txtRes (vl-catch-all-apply 'vla-get-TextString (list obj))
        pos   (mc:get-object-insertion-xy obj))
  (if (and (not (vl-catch-all-error-p txtRes)) (= (type txtRes) 'STR) (mc:xy-p pos))
    (progn
      (setq txt txtRes)
      (if (wcmatch oname "AcDbMText,AcDbMLeader") (setq txt (mc:strip-mtext txt)))
      (cons (list txt pos) result))
    result)
)
(defun mc:add-mleader-text (obj result / txtRes locRes pos txt)
  (setq txtRes (vl-catch-all-apply 'vla-get-TextString   (list obj))
        locRes (vl-catch-all-apply 'vla-get-TextLocation (list obj)))
  (if (and (not (vl-catch-all-error-p txtRes)) (= (type txtRes) 'STR)
           (not (vl-catch-all-error-p locRes)))
    (progn
      (setq pos (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value locRes))))
      (if (and (not (vl-catch-all-error-p pos)) (mc:xy-p pos))
        (progn
          (setq txt (mc:strip-mtext txtRes))
          (setq result (cons (list txt (list (car pos) (cadr pos))) result))))))
  result
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
                (foreach att attList (setq result (mc:add-textlike-object att result))))))))
      result))
)

;;; Collect texts from any iterable space object
(defun mc:collect-texts-in-space (spaceObj result / obj oname)
  (vlax-for obj spaceObj
    (setq oname (vla-get-ObjectName obj))
    (cond
      ((wcmatch oname "AcDbText,AcDbMText,AcDbAttribute,AcDbAttributeDefinition")
       (setq result (mc:add-textlike-object obj result)))
      ((wcmatch oname "AcDbBlockReference")
       (setq result (mc:add-block-attributes obj result)))
      ((wcmatch oname "AcDbMLeader")
       (setq result (mc:add-mleader-text obj result)))))
  result
)

;;; Main text reader: modelspace + all paper-space layouts
(defun mc:get-texts (doc / ms layouts layout lname blk result)
  (setq result nil ms (vla-get-ModelSpace doc))
  (setq result (mc:collect-texts-in-space ms result))
  (setq layouts (vl-catch-all-apply 'vla-get-Layouts (list doc)))
  (if (not (vl-catch-all-error-p layouts))
    (vlax-for layout layouts
      (setq lname (vl-catch-all-apply 'vla-get-Name (list layout)))
      (if (and (not (vl-catch-all-error-p lname))
               (not (= (strcase lname) "MODEL")))
        (progn
          (setq blk (vl-catch-all-apply 'vla-get-Block (list layout)))
          (if (not (vl-catch-all-error-p blk))
            (setq result (mc:collect-texts-in-space blk result)))))))
  result
)

;;; -------------------------------------------------------------------
;;; Global matching
;;; -------------------------------------------------------------------
(defun mc:unmatched-by-index (lst used / result idx x)
  (setq result nil idx 0)
  (foreach x lst
    (if (not (mc:member-int idx used)) (setq result (cons x result)))
    (setq idx (1+ idx)))
  (reverse result)
)
(defun mc:global-best-number-pairs (inchNums metricNums / candidates iIdx mIdx iVal mVal expected diff sorted selected usedI usedM cand)
  (setq candidates nil iIdx 0)
  (foreach iVal inchNums
    (setq mIdx 0)
    (foreach mVal metricNums
      (if (and (numberp iVal) (numberp mVal))
        (progn
          (setq expected (* (abs iVal) *mc-conv*)
                diff     (abs (- (abs mVal) expected)))
          (setq candidates (cons (list diff iIdx mIdx iVal mVal expected) candidates))))
      (setq mIdx (1+ mIdx)))
    (setq iIdx (1+ iIdx)))
  (setq sorted (vl-sort candidates '(lambda (a b) (< (car a) (car b))))
        selected nil usedI nil usedM nil)
  (foreach cand sorted
    (if (and (not (mc:member-int (cadr cand) usedI)) (not (mc:member-int (caddr cand) usedM)))
      (progn
        (setq selected (cons (list (cadddr cand) (nth 4 cand) (nth 5 cand) (car cand)) selected))
        (setq usedI (cons (cadr cand) usedI) usedM (cons (caddr cand) usedM)))))
  (reverse selected)
)
(defun mc:text-values-total-diff (inchNums metricNums / pairs total p)
  (setq pairs (mc:global-best-number-pairs inchNums metricNums))
  (if pairs
    (progn (setq total 0.0) (foreach p pairs (setq total (+ total (cadddr p)))) total)
    nil)
)
(defun mc:global-dim-assignments (inchList metricList
                                  / candidates sorted assignments usedI usedM
                                    iIdx mIdx iEntry mEntry iVal mVal expected
                                    vDiff pDist score cand)
  (setq candidates nil iIdx 0)
  (foreach iEntry inchList
    (setq iVal (car iEntry) mIdx 0)
    (foreach mEntry metricList
      (setq mVal (car mEntry))
      (if (and (numberp iVal) (numberp mVal))
        (progn
          (setq expected (* (abs iVal) *mc-conv*)
                vDiff    (abs (- (abs mVal) expected)))
          ;; SANITY GATE: reject pairings that are clearly wrong.
          ;; A 20% or 5mm discrepancy (whichever is larger) means the two
          ;; dims are almost certainly unrelated. Forcing such a pairing
          ;; produces garbage output (e.g., "9 inch → expected 228.6mm got 9mm").
          ;; Better to leave both dims UNMATCHED so they appear as MISSING.
          (if (<= vDiff (max 5.0 (* expected 0.20)))
            (progn
              ;; Score is value-only. Position NOT used for dim matching because:
              ;; (a) drawings often have uncorrelated coordinate systems;
              ;; (b) for equal-value dims any pairing produces the same QC result.
              (if (<= vDiff *mc-tolerance*)
                (setq score (* vDiff 1000.0))
                (setq score (+ 1000000.0 (* vDiff 1000.0))))
              (setq candidates (cons (list score iIdx mIdx iEntry mEntry vDiff) candidates))))))
      (setq mIdx (1+ mIdx)))
    (setq iIdx (1+ iIdx)))
  (setq sorted (vl-sort candidates '(lambda (a b) (< (car a) (car b))))
        assignments nil usedI nil usedM nil)
  (foreach cand sorted
    (if (and (not (mc:member-int (cadr cand) usedI)) (not (mc:member-int (caddr cand) usedM)))
      (progn
        (setq assignments (cons cand assignments))
        (setq usedI (cons (cadr cand) usedI) usedM (cons (caddr cand) usedM)))))
  (list
    (vl-sort assignments '(lambda (a b) (< (cadr a) (cadr b))))
    (mc:unmatched-by-index inchList  usedI)
    (mc:unmatched-by-index metricList usedM))
)

;;; Text assignment using PRE-ENRICHED entries: (text pos nums kind)
;;; nums and kind are already computed — no extract calls in the inner loop.
(defun mc:global-text-assignments (inchRich metricRich
                                   / candidates sorted assignments usedI usedM
                                     iIdx mIdx iEntry mEntry iNums mNums iKind mKind
                                     vDiff pDist score cand)
  (setq candidates nil iIdx 0)
  (foreach iEntry inchRich
    (setq iNums (caddr iEntry) iKind (cadddr iEntry) mIdx 0)
    (foreach mEntry metricRich
      (setq mKind (cadddr mEntry))
      (if (= iKind mKind)
        (progn
          (setq mNums (caddr mEntry)
                vDiff (mc:text-values-total-diff iNums mNums)
                pDist (mc:pos-distance-best (cadr iEntry) (cadr mEntry)))
          (if (and vDiff (<= vDiff *mc-text-max-sane-diff*))
            (progn
              (if (<= vDiff *mc-tolerance*)
                (setq score (+ (* vDiff 1000.0) (* pDist *mc-position-weight-pass*)))
                (setq score (+ 1000000.0 (* vDiff 1000.0) (* pDist *mc-position-weight-fail*))))
              (setq candidates
                (cons (list score iIdx mIdx iEntry mEntry vDiff pDist 'VALUE) candidates))))
          (if (< pDist *mc-text-position-limit*)
            (setq candidates
              (cons (list (+ 2000000.0 pDist) iIdx mIdx iEntry mEntry vDiff pDist 'POSITION) candidates)))))
      (setq mIdx (1+ mIdx)))
    (setq iIdx (1+ iIdx)))
  (setq sorted (vl-sort candidates '(lambda (a b) (< (car a) (car b))))
        assignments nil usedI nil usedM nil)
  (foreach cand sorted
    (if (and (not (mc:member-int (cadr cand) usedI)) (not (mc:member-int (caddr cand) usedM)))
      (progn
        (setq assignments (cons cand assignments))
        (setq usedI (cons (cadr cand) usedI) usedM (cons (caddr cand) usedM)))))
  (list
    (vl-sort assignments '(lambda (a b) (< (cadr a) (cadr b))))
    (mc:unmatched-by-index inchRich  usedI)
    (mc:unmatched-by-index metricRich usedM))
)

(defun mc:sort-by-pos (lst /)
  (vl-sort lst '(lambda (a b)
    (cond
      ((not (mc:xy-p (cadr a))) nil)
      ((not (mc:xy-p (cadr b))) T)
      ((< (caadr a) (caadr b)) T)
      ((and (equal (caadr a) (caadr b) 0.01) (< (cadadr a) (cadadr b))) T)
      (T nil))))
)

;;; -------------------------------------------------------------------
;;; Layers / balloons
;;; -------------------------------------------------------------------
(defun mc:balloon-height (/ dtxt dscl sz)
  (setq dtxt (getvar "DIMTXT") dscl (getvar "DIMSCALE")
        sz   (* (max dtxt 0.05) (max dscl 1.0) 0.85))
  (max sz 0.5)
)
(defun mc:ensure-layer (doc lname color / layers layerRes addRes)
  (setq layers   (vla-get-Layers doc)
        layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (vl-catch-all-error-p layerRes)
    (progn
      (setq addRes (vl-catch-all-apply 'vla-add (list layers lname)))
      (if (not (vl-catch-all-error-p addRes)) (vla-put-Color addRes color)))
    (vla-put-Color layerRes color))
)
(defun mc:delete-layer (lname / doc layers layerRes delRes)
  (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object))
        layers   (vla-get-Layers doc)
        layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (not (vl-catch-all-error-p layerRes))
    (progn
      (if (= (strcase (getvar "CLAYER")) (strcase lname)) (setvar "CLAYER" "0"))
      (setq delRes (vl-catch-all-apply 'vla-delete (list layerRes)))
      (if (vl-catch-all-error-p delRes)
        (vl-catch-all-apply 'command (list "._-PURGE" "_La" lname "_No")))))
)
(defun mc:clear-qc-layers (/ ss i lname)
  (foreach lname (list "MC_PASS" "MC_ERRORS")
    (setq ss (ssget "X" (list (cons 8 lname))))
    (if ss (progn (setq i 0) (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))))
    (mc:delete-layer lname))
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
  (strcat (mc:fmt inchVal) "\"" "  exp " (mc:fmt expected) "mm" "  got " (mc:fmt actual) "mm"))
(defun mc:make-pass-label     (/) "\\U+2713")
(defun mc:make-missing-label  (inchVal expected /)
  (strcat "??  " (mc:fmt inchVal) "\"" "  exp " (mc:fmt expected) "mm"))
(defun mc:make-unmatched-metric-label (metricVal /)
  (strcat "??  got " (mc:fmt metricVal) "mm"))
(defun mc:get-current-dwg-folder (/ p)
  (setq p (getvar "DWGPREFIX"))
  (if (or (not p) (= p "")) (setq p ""))
  p
)

;;; ===================================================================
;;; c:metric_check
;;; ===================================================================
(defun c:metric_check
    (/ *error* oldError oldCmdecho acadObj metricDoc metricDir inchFile inchOpen inchDoc inchIsDbx
       metricDims inchDims metricTexts inchTexts inchRich metricRich
       iDimLen mDimLen iTxtLen mTxtLen iRichLen mRichLen
       iEntry mEntry assignment iVal mVal expected diff errPos passFlag missingPos
       iStr mStr iNums mNums pairs pair iNum mNum dimMarkers txtMarkers txtBody anyFail
       remainingMetricDims unmatchedInchDims remainingMetricTexts unmatchedInchTexts
       dimPlan textPlan dimAssignments textAssignments balloonH errIdx
       dimPass dimFail txtPass txtFail
       missingDimCount unmatchedMetricDimCount textMissingCount ignoredMetricTexts m
       usedRescueM rescuedInchSet stillMissingInch stillUnmatchedMetric
       rescuePosLimit bestRD bestRM bestRMIdx mIdx2 rd)

  (vl-load-com)
  (setq oldError *error* oldCmdecho (getvar "CMDECHO")
        *mc-active-inch-doc* nil *mc-active-inch-dbx* nil)

  (defun *error* (msg)
    (if *mc-active-inch-doc*
      (mc:close-inch-source-doc *mc-active-inch-doc* *mc-active-inch-dbx*))
    (setq *mc-active-inch-doc* nil *mc-active-inch-dbx* nil)
    (if metricDoc (vl-catch-all-apply 'vla-Activate (list metricDoc)))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (setq *error* oldError)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nMETRIC_CHECK error: " msg)))
    (princ))

  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj)
        metricDir (mc:get-current-dwg-folder))

  (princ "\nReading metric drawing (modelspace + layouts)...")
  (setq metricDims  (mc:get-dims  metricDoc)
        metricTexts (mc:get-texts metricDoc))
  (princ (strcat " " (itoa (length metricDims)) " dim(s), "
                     (itoa (length metricTexts)) " text/attr found."))

  (setq inchFile (getfiled "Select Inch Source Drawing" metricDir "dwg" 4))
  (if (not inchFile)
    (progn (princ "\nCancelled.") (setq *error* oldError) (princ) (exit)))

  (princ "\nOpening inch source via ObjectDBX...")
  (setq inchOpen (mc:open-inch-source-doc acadObj inchFile))
  (if (not inchOpen)
    (progn (princ "\nERROR: Could not open inch drawing.") (setq *error* oldError) (princ) (exit)))

  (setq inchDoc (car inchOpen) inchIsDbx (cadr inchOpen)
        *mc-active-inch-doc* inchDoc *mc-active-inch-dbx* inchIsDbx)
  (princ (if inchIsDbx " ObjectDBX OK." " visible fallback."))

  (princ "\nReading inch drawing (modelspace + layouts + nested blocks)...")
  (setq inchDims  (mc:get-dims  inchDoc)
        inchTexts (mc:get-texts inchDoc))
  (princ (strcat " " (itoa (length inchDims)) " dim(s), "
                     (itoa (length inchTexts)) " text/attr found."))

  (mc:close-inch-source-doc inchDoc inchIsDbx)
  (setq *mc-active-inch-doc* nil *mc-active-inch-dbx* nil)
  (vla-Activate metricDoc)

  ;; --- Deduplicate: same value within 0.05mm treated as one unique dim ---
  ;; Drawings with symmetric views produce duplicate dim readings from block
  ;; scanning. Dedup before matching avoids n:1 false-miss pairings.
  (setq metricDims (mc:dedup-dims metricDims 0.05)
        inchDims   (mc:dedup-dims inchDims   0.005))   ; tighter tol for inch (unit=inches)

  (setq metricDims  (mc:sort-by-pos metricDims)
        inchDims    (mc:sort-by-pos inchDims)
        metricTexts (mc:sort-by-pos metricTexts)
        inchTexts   (mc:sort-by-pos inchTexts))

  (setq dimMarkers nil txtMarkers nil
        dimPass 0 dimFail 0 txtPass 0 txtFail 0
        missingDimCount 0 unmatchedMetricDimCount 0
        textMissingCount 0 ignoredMetricTexts 0)
  (setq mDimLen (length metricDims) iDimLen (length inchDims)
        mTxtLen (length metricTexts) iTxtLen (length inchTexts))

  (princ (strcat "\nAfter dedup+filter: " (itoa iDimLen) " unique inch dims, "
                                           (itoa mDimLen) " unique metric dims."))
  (if (/= mDimLen iDimLen)
    (princ (strcat " (count mismatch — " (itoa (abs (- iDimLen mDimLen))) " extra on "
                   (if (> iDimLen mDimLen) "inch" "metric") " side)")))

  ;; Pre-cache text extraction — eliminates O(n*m) repeated strip/parse
  (princ "\nPre-caching text extraction...")
  (setq inchRich
    (vl-remove-if '(lambda (e) (null (caddr e)))
      (mapcar '(lambda (e) (mc:enrich-text-entry e nil)) inchTexts)))
  (setq metricRich
    (vl-remove-if '(lambda (e) (null (caddr e)))
      (mapcar '(lambda (e) (mc:enrich-text-entry e T)) metricTexts)))
  (setq iRichLen (length inchRich) mRichLen (length metricRich))
  (princ (strcat " " (itoa iRichLen) " inch dimensional, "
                     (itoa mRichLen) " metric dimensional."))

  ;; ----------------------- dimension check --------------------------
  (princ "\nMatching dimensions...")
  (setq dimPlan            (mc:global-dim-assignments inchDims metricDims)
        dimAssignments     (car   dimPlan)
        unmatchedInchDims  (cadr  dimPlan)
        remainingMetricDims (caddr dimPlan))

  (foreach assignment dimAssignments
    (setq iEntry   (nth 3 assignment)
          mEntry   (nth 4 assignment)
          diff     (nth 5 assignment)
          iVal     (car iEntry)
          mVal     (car mEntry)
          expected (if (numberp iVal) (* (abs iVal) *mc-conv*) nil)
          errPos   (cadr mEntry)
          passFlag (and (numberp mVal) (numberp expected) (<= diff *mc-tolerance*)))
    (if passFlag (setq dimPass (1+ dimPass)) (setq dimFail (1+ dimFail)))
    (setq dimMarkers
      (cons (list passFlag
                  (if passFlag (mc:make-pass-label) (mc:make-label iVal expected mVal))
                  errPos)
            dimMarkers)))

  ;; --- Position-rescue pass for remaining unmatched dims ---------------
  ;; After value-based matching (20%/5mm sanity gate), leftover inch and metric
  ;; dims that share the same visual location are paired by position proximity.
  ;; NTS ("not to scale") dims sit at the same spot in both drawings even when
  ;; their values don't follow the 25.4x rule.  Without rescue they generate
  ;; TWO confusing "??" balloons; after rescue they become ONE labelled FAIL
  ;; balloon: "703\"  exp 17856mm  got 7030.02mm" — you see both values clearly.
  (setq usedRescueM nil rescuedInchSet nil
        stillMissingInch nil rescuePosLimit 500.0)  ; 500mm covers same visual area
  (foreach iEntry unmatchedInchDims
    (setq bestRD 1.0e99 bestRM nil bestRMIdx nil mIdx2 0)
    (foreach mEntry remainingMetricDims
      (if (not (mc:member-int mIdx2 usedRescueM))
        (progn
          (setq rd (mc:pos-distance-best (cadr iEntry) (cadr mEntry)))
          (if (< rd bestRD) (setq bestRD rd bestRM mEntry bestRMIdx mIdx2))))
      (setq mIdx2 (1+ mIdx2)))
    (if (and bestRM (< bestRD rescuePosLimit))
      ;; Rescue pair — show as FAIL balloon at the metric dim position
      (progn
        (setq iVal     (car iEntry)
              mVal     (car bestRM)
              expected (if (numberp iVal) (* (abs iVal) *mc-conv*) nil)
              errPos   (cadr bestRM)
              dimFail  (1+ dimFail))
        (setq dimMarkers
          (cons (list nil (mc:make-label iVal expected mVal) errPos) dimMarkers))
        (setq usedRescueM    (cons bestRMIdx usedRescueM)
              rescuedInchSet (cons iEntry rescuedInchSet)))
      ;; No metric dim nearby at all — truly missing
      (setq stillMissingInch (cons iEntry stillMissingInch))))

  ;; Truly missing inch dims (no rescue match found)
  (foreach iEntry stillMissingInch
    (setq iVal     (car iEntry)
          expected (if (numberp iVal) (* (abs iVal) *mc-conv*) nil)
          missingPos (mc:scale-point (cadr iEntry) *mc-conv*)
          missingDimCount (1+ missingDimCount)
          dimFail  (1+ dimFail))
    (if (not (mc:xy-p missingPos)) (setq missingPos (cadr iEntry)))
    (if (mc:xy-p missingPos)
      (setq dimMarkers (cons (list nil (mc:make-missing-label iVal expected) missingPos) dimMarkers))))

  ;; Truly unmatched metric dims (no inch dim nearby)
  (setq stillUnmatchedMetric (mc:unmatched-by-index remainingMetricDims usedRescueM))
  (foreach mEntry stillUnmatchedMetric
    (setq unmatchedMetricDimCount (1+ unmatchedMetricDimCount) dimFail (1+ dimFail))
    (setq dimMarkers
      (cons (list nil (mc:make-unmatched-metric-label (car mEntry)) (cadr mEntry)) dimMarkers)))

  (princ (strcat " done. " (itoa dimPass) " pass, " (itoa dimFail) " fail/missing."))

  ;; -------------------- text/attribute check ------------------------
  (princ "\nMatching text/attributes...")
  (setq textPlan             (mc:global-text-assignments inchRich metricRich)
        textAssignments      (car   textPlan)
        unmatchedInchTexts   (cadr  textPlan)
        remainingMetricTexts (caddr textPlan))

  (foreach assignment textAssignments
    (setq iEntry (nth 3 assignment)
          mEntry (nth 4 assignment)
          iStr   (car iEntry)
          mStr   (car mEntry)
          errPos (cadr mEntry)
          iNums  (caddr iEntry)
          mNums  (caddr mEntry)
          pairs  (mc:global-best-number-pairs iNums mNums))
    (if pairs
      (progn
        (setq txtBody nil anyFail nil)
        (foreach pair pairs
          (setq iNum (car pair) mNum (cadr pair) expected (caddr pair) diff (cadddr pair))
          (if (> diff *mc-tolerance*)
            (setq anyFail T txtFail (1+ txtFail))
            (setq txtPass (1+ txtPass)))
          (setq txtBody
            (if txtBody (strcat txtBody "  |  " (mc:make-label iNum expected mNum))
                        (mc:make-label iNum expected mNum))))
        (setq txtMarkers
          (cons (list (not anyFail)
                      (if (not anyFail) (mc:make-pass-label) txtBody)
                      errPos)
                txtMarkers)))
      (progn
        (setq textMissingCount (1+ textMissingCount) txtFail (1+ txtFail))
        (setq txtMarkers
          (cons (list nil
                      (mc:make-missing-label (car iNums) (* (abs (car iNums)) *mc-conv*))
                      (cadr iEntry))
                txtMarkers)))))

  (foreach iEntry unmatchedInchTexts
    (setq iNums (caddr iEntry))
    (if iNums
      (progn
        (setq textMissingCount (1+ textMissingCount) txtFail (1+ txtFail))
        (setq txtMarkers
          (cons (list nil
                      (mc:make-missing-label (car iNums) (* (abs (car iNums)) *mc-conv*))
                      (cadr iEntry))
                txtMarkers)))))

  (foreach mEntry remainingMetricTexts
    (setq mNums (caddr mEntry))
    (if mNums
      (progn
        (setq txtFail (1+ txtFail))
        (setq txtMarkers
          (cons (list nil (mc:make-unmatched-metric-label (car mNums)) (cadr mEntry)) txtMarkers)))
      (setq ignoredMetricTexts (1+ ignoredMetricTexts))))

  (princ (strcat " done. " (itoa txtPass) " pass, " (itoa txtFail) " fail/missing."))

  ;; ----------------------------- output -----------------------------
  (princ "\nUpdating QC layers...")
  (mc:clear-qc-layers)
  (mc:ensure-layer metricDoc "MC_PASS"   3)
  (mc:ensure-layer metricDoc "MC_ERRORS" 7)

  (setq balloonH (mc:balloon-height) errIdx 1)
  (foreach m (reverse dimMarkers)
    (mc:place-balloon (car (caddr m)) (cadr (caddr m)) balloonH (car m) (cadr m))
    (if (not (car m))
      (progn (princ (strcat "\n  [" (itoa errIdx) "] DIM: " (cadr m))) (setq errIdx (1+ errIdx)))))
  (foreach m (reverse txtMarkers)
    (mc:place-balloon (car (caddr m)) (cadr (caddr m)) balloonH (car m) (cadr m))
    (if (not (car m))
      (progn (princ (strcat "\n  [" (itoa errIdx) "] TXT: " (cadr m))) (setq errIdx (1+ errIdx)))))

  (if (zerop (+ dimFail txtFail)) (princ "\nAll checked conversions PASSED."))
  (vla-Regen metricDoc 2)

  (princ
    (strcat
      "\n--------------------------------------------\n"
      "METRIC CHECK DONE v18.7\n"
      "  Dimensions      : " (itoa dimPass) " pass  " (itoa dimFail) " fail/missing\n"
      "  Text/Attr       : " (itoa txtPass) " pass  " (itoa txtFail) " fail/missing\n"
      "  Position-rescue pairs   : " (itoa (length rescuedInchSet))
        " (NTS/unmatched dims paired by location)\n"
      "  Truly missing inch dims : " (itoa missingDimCount) "\n"
      "  Truly unmatched metric  : " (itoa unmatchedMetricDimCount) "\n"
      "  Missing text/attr       : " (itoa textMissingCount) "\n"
      "  Ignored non-dim text    : " (itoa ignoredMetricTexts) "\n"
      "  Inch dim scan  : modelspace + nested blocks + all layouts\n"
      "  Inch text scan : modelspace + all layouts\n"
      "  Ignored blocks : C D KF REVSYMB REVC REVTRI REVCIRCLE TITLEBLOCK BORDER TB TITLE REVISION REVCLOUD FRAME\n"
      "  Inch source    : " (if inchIsDbx "ObjectDBX invisible" "visible fallback") "\n"
      "--------------------------------------------"))
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (setq *error* oldError)
  (princ)
)

;;; -------------------------------------------------------------------
;;; Self-test
;;; -------------------------------------------------------------------
(defun mc:selftest-check (name ok /)
  (princ (strcat "\n  " (if ok "PASS " "FAIL ") name))
  ok
)
(defun c:mc_selftest (/ pass fail ok nums kind pairs plan)
  (setq pass 0 fail 0)
  (princ "\nMETRIC_CHECK v18.7 self-test")

  ;; --- number extraction ---
  (setq nums (mc:extract-conversion-numbers "%%C.03 [.76]"))
  (setq ok (and (= (length nums) 2) (equal (car nums) 0.03 1e-8)))
  (if (mc:selftest-check "diameter decimal extraction" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq nums (mc:extract-conversion-numbers "SHEET 12 REV 3"))
  (setq ok (null nums))
  (if (mc:selftest-check "title-block noise ignored" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq nums (mc:extract-conversion-numbers "-.125"))
  (setq ok (and nums (equal (car nums) -0.125 1e-8)))
  (if (mc:selftest-check "signed decimal -.125" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq nums (mc:extract-conversion-numbers (mc:strip-mtext "1.000\\S+.004^-.000;")))
  (setq ok (and nums (= (length nums) 1) (equal (car nums) 1.0 1e-8)))
  (if (mc:selftest-check "stacked fraction stripped" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; --- kind classification ---
  ;; %%C without FCF context = diameter dimension
  (setq kind (mc:text-kind "%%C.03 [.76]" nil))
  (setq ok (eq kind 'DIA))
  (if (mc:selftest-check "%%C.03 [.76] classified DIA (no FCF context)" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; Bare diameter symbol without GDT modifier → DIA
  (setq ok (mc:has-dia-cue-p "Ø12"))
  (if (mc:selftest-check "Ø12 has-dia-cue" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; GD&T FCF: metric "Ø 0.76 (S) A C" → TOL  (Ø = cylindrical zone, not diameter)
  (setq kind (mc:text-kind "Ø 0.76 (S) A C" T))
  (setq ok (eq kind 'TOL))
  (if (mc:selftest-check "GDT: Ø 0.76 (S) A C classified TOL" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; GD&T FCF: inch "%%C.03 (M) A B C" → TOL
  (setq kind (mc:text-kind "%%C.03 (M) A B C" nil))
  (setq ok (eq kind 'TOL))
  (if (mc:selftest-check "GDT: %%C.03 (M) A B C classified TOL" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; GD&T FCF: inch without Ø ".030 (M) A B C" → TOL
  (setq kind (mc:text-kind ".030 (M) A B C" nil))
  (setq ok (eq kind 'TOL))
  (if (mc:selftest-check "GDT: .030 (M) A B C classified TOL" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; GD&T FCF via pipe separator
  (setq kind (mc:text-kind "| 0.76 | A | B |" T))
  (setq ok (eq kind 'TOL))
  (if (mc:selftest-check "GDT: pipe-separated FCF classified TOL" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; Bilateral tolerance: leading "+" → TOL regardless of magnitude
  (setq kind (mc:text-kind "+0.38" T))
  (setq ok (eq kind 'TOL))
  (if (mc:selftest-check "bilateral +0.38 classified TOL" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq kind (mc:text-kind "+0.015" nil))
  (setq ok (eq kind 'TOL))
  (if (mc:selftest-check "bilateral +0.015 classified TOL" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; Key regression: +0.015" should pair with +0.38mm (0.015×25.4=0.381≈0.38)
  (setq plan
    (mc:global-text-assignments
      (list (mc:enrich-text-entry (list "+0.015" (list 50.0 50.0)) nil))
      (list (mc:enrich-text-entry (list "+0.38"  (list 50.0 50.0)) T))))
  (setq ok (and (= (length (car plan)) 1)
                (numberp (nth 5 (car (car plan))))
                (<= (nth 5 (car (car plan))) *mc-tolerance*)))
  (if (mc:selftest-check "+0.015in pairs & passes vs +0.38mm" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; --- numeric matching ---
  (setq pairs (mc:global-best-number-pairs (list 0.125) (list 3.175)))
  (setq ok (and pairs (<= (cadddr (car pairs)) *mc-tolerance*)))
  (if (mc:selftest-check "0.125in -> 3.175mm within tolerance" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; Key regression: .03 (M) A B C  vs  Ø 0.76 (S) A C  must PAIR and PASS
  (setq plan
    (mc:global-text-assignments
      (list (mc:enrich-text-entry (list ".030 (M) A B C" (list 0.0 0.0)) nil))
      (list (mc:enrich-text-entry (list "Ø 0.76 (S) A C" (list 0.0 0.0)) T))))
  (setq ok (and (= (length (car plan)) 1)
                (numberp (nth 5 (car (car plan))))
                (<= (nth 5 (car (car plan))) *mc-tolerance*)))
  (if (mc:selftest-check "GDT .030in FCF pairs & passes vs Ø 0.76 metric" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; Same but metric has %%C not Ø
  (setq plan
    (mc:global-text-assignments
      (list (mc:enrich-text-entry (list ".030 (M) A B C"    (list 0.0 0.0)) nil))
      (list (mc:enrich-text-entry (list "%%C 0.76 (S) A C"  (list 0.0 0.0)) T))))
  (setq ok (and (= (length (car plan)) 1)
                (<= (nth 5 (car (car plan))) *mc-tolerance*)))
  (if (mc:selftest-check "GDT .030in FCF pairs & passes vs %%C 0.76 metric" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; Global text assignment — swapped order smoke-test
  (setq plan
    (mc:global-text-assignments
      (list (list "%%C.03" (list 0.0 0.0) (list 0.03) 'DIA)
            (list ".125"   (list 100.0 0.0) (list 0.125) 'DIM))
      (list (list "3.175"  (list 100.0 0.0) (list 3.175) 'DIM)
            (list "%%C.76" (list 0.0 0.0)   (list 0.76)  'DIA))))
  (setq ok (= (length (car plan)) 2))
  (if (mc:selftest-check "global text assignment swapped order" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (princ (strcat "\n  " (itoa pass) " passed, " (itoa fail) " failed."))
  (princ)
)

;;; -------------------------------------------------------------------
;;; METRIC_CLEAR
;;; -------------------------------------------------------------------
(defun c:metric_clear (/)
  (vl-load-com)
  (mc:clear-qc-layers)
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 2)
  (princ "\nMC_PASS and MC_ERRORS balloons and layers removed.")
  (princ)
)

;;; -------------------------------------------------------------------
;;; MC_DIAG  — diagnostic dump: what dims are actually collected
;;; Run this to see exactly what the script reads from both drawings.
;;; -------------------------------------------------------------------
(defun c:mc_diag
    (/ *error* oldError acadObj metricDoc metricDir inchFile inchOpen inchDoc inchIsDbx
       metricDims inchDims iDims mDims e v i)
  (vl-load-com)
  (setq oldError *error*)
  (defun *error* (msg)
    (if *mc-active-inch-doc*
      (mc:close-inch-source-doc *mc-active-inch-doc* *mc-active-inch-dbx*))
    (setq *mc-active-inch-doc* nil *mc-active-inch-dbx* nil)
    (setq *error* oldError)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nMC_DIAG error: " msg)))
    (princ))

  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj)
        metricDir (mc:get-current-dwg-folder))

  (princ "\n=== MC_DIAG v18.7 ===")
  (princ "\nCollecting metric dims (active doc)...")
  (setq metricDims (mc:get-dims metricDoc))
  (princ (strcat " found " (itoa (length metricDims)) " dim(s)."))

  (setq inchFile (getfiled "Select Inch Source Drawing" metricDir "dwg" 4))
  (if (not inchFile)
    (progn (princ "\nCancelled.") (setq *error* oldError) (princ) (exit)))

  (princ "\nOpening inch drawing...")
  (setq inchOpen (mc:open-inch-source-doc acadObj inchFile))
  (if (not inchOpen)
    (progn (princ "\nERROR: Could not open inch drawing.") (setq *error* oldError) (princ) (exit)))
  (setq inchDoc (car inchOpen) inchIsDbx (cadr inchOpen)
        *mc-active-inch-doc* inchDoc *mc-active-inch-dbx* inchIsDbx)
  (princ (if inchIsDbx " ObjectDBX." " visible."))

  (princ "\nCollecting inch dims...")
  (setq inchDims (mc:get-dims inchDoc))
  (princ (strcat " found " (itoa (length inchDims)) " dim(s)."))

  (mc:close-inch-source-doc inchDoc inchIsDbx)
  (setq *mc-active-inch-doc* nil *mc-active-inch-dbx* nil)

  ;; Sort by value for easy reading
  (setq iDims (vl-sort inchDims   '(lambda (a b) (< (car a) (car b))))
        mDims (vl-sort metricDims '(lambda (a b) (< (car a) (car b)))))

  (princ "\n\n--- INCH DIMS (sorted) ---")
  (setq i 0)
  (foreach e iDims
    (setq v (car e))
    (if (numberp v)
      (progn
        (setq i (1+ i))
        (if (<= i 40)
          (princ (strcat "\n  " (itoa i) ". " (rtos v 2 4) "\"  exp-metric=" (rtos (* v 25.4) 2 3) "mm"))))))
  (if (> i 40) (princ (strcat "\n  ... (" (itoa (- i 40)) " more)")))

  (princ "\n\n--- METRIC DIMS (sorted) ---")
  (setq i 0)
  (foreach e mDims
    (setq v (car e))
    (if (numberp v)
      (progn
        (setq i (1+ i))
        (if (<= i 40)
          (princ (strcat "\n  " (itoa i) ". " (rtos v 2 4) "mm"))))))
  (if (> i 40) (princ (strcat "\n  ... (" (itoa (- i 40)) " more)")))

  (princ "\n\n--- PROPOSED PAIRINGS ---")
  (foreach e iDims
    (setq v (car e))
    (if (numberp v)
      (progn
        (setq expected (* (abs v) 25.4))
        ;; Find best metric match within 20% or 5mm
        (setq bestDiff 1.0e99 bestVal nil)
        (foreach m mDims
          (setq mv (car m))
          (if (numberp mv)
            (progn
              (setq d (abs (- (abs mv) expected)))
              (if (and (< d bestDiff) (<= d (max 5.0 (* expected 0.20))))
                (setq bestDiff d bestVal mv)))))
        (if bestVal
          (progn
            (if (<= bestDiff *mc-tolerance*)
              (princ (strcat "\n  PASS  " (rtos v 2 4) "\" -> " (rtos bestVal 2 3) "mm  (exp " (rtos expected 2 3) "mm  diff=" (rtos bestDiff 2 3) ")"))
              (princ (strcat "\n  FAIL  " (rtos v 2 4) "\" -> " (rtos bestVal 2 3) "mm  (exp " (rtos expected 2 3) "mm  diff=" (rtos bestDiff 2 3) ")"))))
          (princ (strcat "\n  MISS  " (rtos v 2 4) "\"  exp " (rtos expected 2 3) "mm  (no metric dim within range)"))))))

  (princ "\n\n=== end MC_DIAG ===\n")
  (setq *error* oldError)
  (princ)
)

(princ "\nMETRIC_CHECK.LSP v18.7 loaded.  <-- confirm this version when loaded")
(princ "\n  METRIC_CHECK  -- full QC run with visual balloons")
(princ "\n  MC_DIAG       -- diagnostic: dump all collected dim values (RUN THIS FIRST)")
(princ "\n  METRIC_CLEAR  -- erase QC balloons and layers")
(princ "\n  MC_SELFTEST   -- parser / matcher self-test")
(princ)
