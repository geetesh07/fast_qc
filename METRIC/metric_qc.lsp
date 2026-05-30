;;; =====================================================================
;;; METRIC_QC.LSP  v2.3   (clean rewrite)
;;;
;;; v2.1: value-rescue pass.  An inch entity stranded by position-greedy
;;;       matching inside a cluster of near-identical callouts is no longer
;;;       a false MISSING -- if its converted value exists in an unmatched
;;;       metric entity anywhere, they are paired.  MISSING now strictly
;;;       means "this converted value appears NOWHERE in the metric drawing".
;;;
;;; PURPOSE
;;;   Verify that an inch drawing was correctly converted to a metric
;;;   drawing by a macro/script.  The two drawings share IDENTICAL
;;;   geometry and coordinates (they overlap exactly) -- only the
;;;   DISPLAYED dimension/text values differ (inch vs mm).
;;;
;;; CORE CONCEPT  (why this version works where value-matching failed)
;;;   * MATCH BY POSITION, VERIFY BY VALUE.
;;;     Because both drawings sit in the same coordinate space, an entity
;;;     is at the SAME XY in both files.  Pair inch<->metric entities by
;;;     location (nearest neighbour, distance ~ 0 for true partners), then
;;;     check  inch_displayed * 25.4  ==  metric_displayed.
;;;   * This catches the real failure modes:
;;;       - dim the macro forgot to convert  -> pairs by position, fails value
;;;       - dim the macro dropped entirely    -> inch dim unmatched = MISSING
;;;       - NTS dims (703 -> 7030 on purpose)  -> ONE balloon, both values
;;;   * No value-based pairing, no DIA/RAD/TOL kind guessing, no dedup
;;;     hacks -- position is unique and reliable.
;;;
;;; READING CORRECTNESS
;;;   * Dimensions inside blocks are read with a full 2D transform matrix
;;;     accumulated through every nesting level, so their positions are
;;;     true WORLD coordinates (old versions read raw block-local coords).
;;;   * Displayed value = text-override number, else Measurement * DimLFac.
;;;
;;; COMMANDS
;;;   METRIC_CHECK / MQC  -- run QC, place pass/fail balloons on metric dwg
;;;   MQC_DIAG            -- text-only dump of matches/missing/extra
;;;   MQC_CLEAR          -- remove QC balloons + layers
;;;   MQC_TEST           -- parser / matcher self test
;;;
;;;   LOAD ONLY THIS FILE (do not also load the old metric_check.lsp).
;;; =====================================================================

(vl-load-com)

;;; -------------------------------------------------------------------
;;; Settings
;;; -------------------------------------------------------------------
(setq *qc-conv*       25.4)   ; inch -> mm
(setq *qc-tol*        0.6)    ; mm.  Rounding to nearest whole mm gives <=0.5mm
                              ; error for any size, so 0.6 covers rounding while
                              ; still catching genuine conversion errors.
;; Position match gate = how far (as a fraction of overall drawing spread) a
;; dimension may have shifted between inch and metric and still be paired.
;; Generous enough to absorb a macro nudging dim text "a lil bit"; greedy
;; nearest-first ordering + the value tie-breaker stop it grabbing a wrong
;; neighbour.  Raise if real partners are missed; lower if neighbours cross-match.
(setq *qc-dim-gate*   0.20)   ; dims are sparse -> can be generous
(setq *qc-txt-gate*   0.12)   ; text is denser -> a bit tighter
;; When two metric entities fall inside the gate of one inch entity, multiply
;; the position score of the value-CORRECT candidate by this factor so it wins.
;; A same-spot wrong-value dim (distance ~ 0) still wins outright, so genuine
;; un-converted dims are still matched and flagged.
(setq *qc-valbonus*   0.6)
(setq *qc-max-depth*  8)      ; block nesting recursion limit
(setq *qc-ignore-blocks*
  "C,D,KF,BOM,BOM1,REVD,REVSYMB,REVC,REVTRI,REVCIRCLE,REVCLOUD,REVISION,TITLEBLOCK,BORDER,TB,TITLE,FRAME,LOGO")
(setq *qc-active-inch-doc* nil)
(setq *qc-active-inch-dbx* nil)

;;; accumulators (reset per drawing)
(setq *qc-dims*    nil)
(setq *qc-texts*   nil)
(setq *qc-blocks*  nil)
(setq *qc-doc-lfac* 1.0)   ; document DIMLFAC of the drawing currently being read

;;; -------------------------------------------------------------------
;;; Tiny helpers
;;; -------------------------------------------------------------------
(defun qc:num (x dflt)
  (if (and (not (vl-catch-all-error-p x)) (numberp x)) x dflt)
)
(defun qc:xy-p (p)
  (and (listp p) (>= (length p) 2) (numberp (car p)) (numberp (cadr p)))
)
(defun qc:dist (a b / dx dy)
  (setq dx (- (car a) (car b)) dy (- (cadr a) (cadr b)))
  (sqrt (+ (* dx dx) (* dy dy)))
)
(defun qc:member (n lst / f x)
  (setq f nil)
  (foreach x lst (if (= x n) (setq f T)))
  f
)
(defun qc:is-digit (c)
  (and c (= (type c) 'STR) (= (strlen c) 1) (>= (ascii c) 48) (<= (ascii c) 57))
)
(defun qc:rtrim0 (s)
  (while (and (> (strlen s) 1) (= (substr s (strlen s) 1) "0") (vl-string-search "." s))
    (setq s (substr s 1 (1- (strlen s)))))
  (if (and (> (strlen s) 1) (= (substr s (strlen s) 1) "."))
    (setq s (substr s 1 (1- (strlen s)))))
  s
)
(defun qc:fmt (v)
  (if (numberp v) (qc:rtrim0 (rtos v 2 4)) "?")
)
(defun qc:str-upper (s)
  (if (= (type s) 'STR) (strcase s) "")
)

;;; -------------------------------------------------------------------
;;; 2D affine transform matrix  (a b c d e f):
;;;   wx = a*x + c*y + e
;;;   wy = b*x + d*y + f
;;; -------------------------------------------------------------------
(defun qc:m-identity () (list 1.0 0.0 0.0 1.0 0.0 0.0))
(defun qc:m-apply (m p / x y)
  (setq x (car p) y (cadr p))
  (list (+ (* (nth 0 m) x) (* (nth 2 m) y) (nth 4 m))
        (+ (* (nth 1 m) x) (* (nth 3 m) y) (nth 5 m)))
)
;; compose: result applies L first, then M  (M . L)
(defun qc:m-compose (m l)
  (list
    (+ (* (nth 0 m) (nth 0 l)) (* (nth 2 m) (nth 1 l)))
    (+ (* (nth 1 m) (nth 0 l)) (* (nth 3 m) (nth 1 l)))
    (+ (* (nth 0 m) (nth 2 l)) (* (nth 2 m) (nth 3 l)))
    (+ (* (nth 1 m) (nth 2 l)) (* (nth 3 m) (nth 3 l)))
    (+ (* (nth 0 m) (nth 4 l)) (* (nth 2 m) (nth 5 l)) (nth 4 m))
    (+ (* (nth 1 m) (nth 4 l)) (* (nth 3 m) (nth 5 l)) (nth 5 m)))
)
(defun qc:m-from-insert (ins sx sy rot / c s)
  (setq c (cos rot) s (sin rot))
  (list (* sx c) (* sx s) (* (- sy) s) (* sy c) (car ins) (cadr ins))
)

;;; -------------------------------------------------------------------
;;; Safe geometry getters
;;; -------------------------------------------------------------------
(defun qc:safearray-xy (v / r)
  (setq r (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value v))))
  (if (and (not (vl-catch-all-error-p r)) (listp r) (>= (length r) 2)
           (numberp (car r)) (numberp (cadr r)))
    (list (car r) (cadr r))
    nil)
)
(defun qc:obj-insertion (obj / r)
  (setq r (vl-catch-all-apply 'vla-get-InsertionPoint (list obj)))
  (if (vl-catch-all-error-p r) nil (qc:safearray-xy r))
)
;; Entity handle -- a string that is PRESERVED when the conversion script edits
;; the drawing in place (SaveAs keeps handles).  The inch dim and its converted
;; metric dim are the SAME entity, hence the SAME handle.  This is the exact
;; 1:1 key that makes EXTRA/MISSING impossible for in-place conversions.
(defun qc:obj-handle (obj / r)
  (setq r (vl-catch-all-apply 'vla-get-Handle (list obj)))
  (if (and (not (vl-catch-all-error-p r)) (= (type r) 'STR) (> (strlen r) 0))
    (strcase r)
    nil)
)
(defun qc:blockref-matrix (br / insR ins)
  (setq insR (vl-catch-all-apply 'vla-get-InsertionPoint (list br)))
  (if (vl-catch-all-error-p insR)
    nil
    (progn
      (setq ins (qc:safearray-xy insR))
      (if (not ins)
        nil
        (qc:m-from-insert ins
          (qc:num (vl-catch-all-apply 'vla-get-XScaleFactor (list br)) 1.0)
          (qc:num (vl-catch-all-apply 'vla-get-YScaleFactor (list br)) 1.0)
          (qc:num (vl-catch-all-apply 'vla-get-Rotation     (list br)) 0.0)))))
)

;;; -------------------------------------------------------------------
;;; MTEXT strip (handles all escape sequences)
;;; -------------------------------------------------------------------
(defun qc:strip-mtext (s / res i len c nc depth skipSemi sc)
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
(defun qc:normalize-cc (s / u)
  (setq u (qc:str-upper s))
  (while (vl-string-search "\\U+00D8" u) (setq u (vl-string-subst "%%C" "\\U+00D8" u)))
  (while (vl-string-search "\\U+2300" u) (setq u (vl-string-subst "%%C" "\\U+2300" u)))
  (while (vl-string-search "\\U+00B1" u) (setq u (vl-string-subst "+/-" "\\U+00B1" u)))
  (while (vl-string-search "ÃƒËœ"       u) (setq u (vl-string-subst "%%C" "ÃƒËœ"        u)))
  (while (vl-string-search "Ã‚Â±"       u) (setq u (vl-string-subst "+/-" "Ã‚Â±"        u)))
  u
)
(defun qc:contains-any (s patterns / hit p u)
  (setq hit nil u (qc:normalize-cc s))
  (foreach p patterns (if (vl-string-search p u) (setq hit T)))
  hit
)

;;; -------------------------------------------------------------------
;;; Numeric extraction
;;; -------------------------------------------------------------------
(defun qc:extract-number-pairs (str / result i len c token hadDot hadDigitAfterDot stopToken sign nextc)
  (setq result nil len (strlen str) i 1)
  (while (<= i len)
    (setq c (substr str i 1) sign "")
    (if (and (or (= c "+") (= c "-")) (< i len)
             (or (qc:is-digit (substr str (1+ i) 1)) (= (substr str (1+ i) 1) ".")))
      (progn
        (setq nextc (substr str (1+ i) 1))
        (if (or (= i 1) (not (qc:is-digit (substr str (1- i) 1))))
          (progn (setq sign c i (1+ i) c nextc)))))
    (cond
      ((qc:is-digit c)
       (setq token (strcat sign c) hadDot nil hadDigitAfterDot nil stopToken nil i (1+ i))
       (while (and (<= i len) (not stopToken)
                   (or (qc:is-digit (substr str i 1)) (= (substr str i 1) ".")))
         (setq c (substr str i 1))
         (cond
           ((qc:is-digit c)
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
      ((and (= c ".") (< i len) (qc:is-digit (substr str (1+ i) 1)))
       (setq token (strcat sign "0.") i (+ i 1))
       (while (and (<= i len) (qc:is-digit (substr str i 1)))
         (setq token (strcat token (substr str i 1)) i (1+ i)))
       (setq result (cons (list (atof token) T token) result))
       (setq i (1- i))))
    (setq i (1+ i)))
  (reverse result)
)
(defun qc:first-number (str / pairs p decimalVal intVal)
  (setq pairs (qc:extract-number-pairs str) decimalVal nil intVal nil)
  (foreach p pairs
    (if (and (listp p) (numberp (car p)))
      (cond
        ((and (cadr p) (not decimalVal)) (setq decimalVal (car p)))
        ((and (not (cadr p)) (not intVal)) (setq intVal (car p))))))
  (cond ((numberp decimalVal) decimalVal) ((numberp intVal) intVal) (T nil))
)
(defun qc:bad-word-p (s)
  (qc:contains-any s
    (list "SHEET" "REV" "DATE" "DRAWING" "DWG" "TITLE" "CAGE" "SCALE" "ZONE"
          "APPRO" "CHECK" "DRAWN" "ORDER" "PART" "SERIAL" "S/N" "ITEM"
          "QTY" "QUANTITY" "PROJECT" "CUSTOMER" "CONTRACT" "SIZE" "CODE"
          "NO." "NUMBER" "MODEL" "MATERIAL" "FINISH" "NF" "NC" "UNC" "UNF" "NPT"))
)
(defun qc:dim-cue-p (s / u)
  (setq u (qc:normalize-cc (vl-string-trim " \t\r\n" s)))
  (or (qc:contains-any u (list "%%C" " DIA" "DIAM" "RAD" " THRU" "+/-"))
      (= (substr u 1 1) "R"))
)
(defun qc:single-number-p (s / pairs stripped ok ch)
  (setq stripped (vl-string-trim " \t\r\n()[]{}\"'" (qc:strip-mtext s)))
  (setq pairs (qc:extract-number-pairs stripped) ok T)
  (foreach ch (mapcar 'chr (vl-string->list stripped))
    (if (not (or (qc:is-digit ch) (= ch ".") (= ch "+") (= ch "-") (= ch ",")))
      (setq ok nil)))
  (and (= (length pairs) 1) ok)
)
(defun qc:dimlike-int-p (s / u cleaned pairs allDigits rem ch)
  (setq u (strcase (qc:strip-mtext s)) pairs (qc:extract-number-pairs u))
  (if (and (= (length pairs) 1) (numberp (caar pairs)) (not (cadar pairs)))
    (progn
      (setq cleaned u)
      (foreach rem (list "(" ")" "[" "]" "{" "}" "\"" "'" "+" "-"
                         "R" "DIA" "DIAM" "DIAMETER" "%%C" " " "\t" "\r" "\n")
        (setq cleaned (vl-string-subst "" rem cleaned)))
      (setq allDigits T)
      (foreach ch (mapcar 'chr (vl-string->list cleaned))
        (if (not (qc:is-digit ch)) (setq allDigits nil)))
      (and (> (strlen cleaned) 0) allDigits))
    nil)
)
;; Extract the dimensional numbers worth checking from a text string.
;; Filters out title-block noise, thread callouts, plain integers, etc.
(defun qc:dim-numbers (str / res raw pairs p val isdec tok)
  (setq res nil raw (qc:strip-mtext str) pairs (qc:extract-number-pairs raw))
  (foreach p pairs
    (setq val (car p) isdec (cadr p) tok (caddr p))
    (if (and (numberp val) (> (abs val) 0.0) (< (abs val) 1.0e6)
             (not (and (not (qc:dim-cue-p raw)) (qc:bad-word-p raw))))
      (cond
        ((and isdec (>= (strlen tok) 2) (= (substr tok 1 2) "0."))
         (setq res (cons val res)))
        ((and isdec (or (qc:dim-cue-p raw) (qc:single-number-p raw)))
         (setq res (cons val res)))
        ((and (not isdec) (qc:dimlike-int-p raw))
         (setq res (cons val res))))))
  (reverse res)
)

;;; -------------------------------------------------------------------
;;; Dimension value + position
;;; -------------------------------------------------------------------
(defun qc:linear-dim-p (oname)
  ;; Keep linear / aligned / rotated / radial / diametric (all convert by 25.4).
  ;; Exclude angular (degrees) and ordinate (coordinate value).
  (and (wcmatch oname "*Dimension*")
       (not (wcmatch oname "*Angular*"))
       (not (wcmatch oname "*Ordinate*")))
)
;; Document DIMLFAC (linear scale factor) of a drawing.  A metric drawing
;; converted from inch geometry typically carries DIMLFAC = 25.4 so its dims
;; DISPLAY mm while the underlying geometry stays in inches.
(defun qc:doc-dimlfac (doc / r)
  (setq r (vl-catch-all-apply 'vla-getvariable (list doc "DIMLFAC")))
  (if (and (not (vl-catch-all-error-p r)) (numberp r)
           (>= (abs r) 0.001) (<= (abs r) 1000.0))
    r
    1.0)
)
;; Effective DIMLFAC for one dimension.  Prefer the object's own DimLFac
;; (covers per-dim overrides, including a legitimate 1.0 for mm-geometry dims).
;; ONLY when that read fails do we fall back to the document DIMLFAC -- this is
;; the fix for dims whose object property can't be fetched via COM/ObjectDBX,
;; which were being read at the raw inch value (e.g. "exp 181.102 got 7.13").
;; Read the dimension's OWN DimLFac so we reconstruct the value it actually
;; DISPLAYS.  We deliberately do NOT fall back to the document DIMLFAC: if the
;; conversion script left a dim unconverted (still showing the inch value), we
;; WANT it to read that inch value and FAIL the check, so the user can fix it.
(defun qc:dimlfac (obj / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj "DimLFac")))
  (if (and (not (vl-catch-all-error-p r)) (numberp r)
           (>= (abs r) 0.001) (<= (abs r) 1000.0))
    r
    1.0)
)
(defun qc:dim-override (obj / tr txt s v)
  (setq tr (vl-catch-all-apply 'vla-get-TextOverride (list obj)))
  (if (vl-catch-all-error-p tr)
    nil
    (progn
      (setq txt tr)
      (if (or (not txt) (= txt "") (vl-string-search "<>" txt))
        nil
        (progn
          (setq s (qc:strip-mtext txt) v (qc:first-number s))
          (if (numberp v) v nil)))))
)
(defun qc:dim-value (obj / ov mr m lf)
  (setq ov (qc:dim-override obj))
  (if (numberp ov)
    ov
    (progn
      (setq mr (vl-catch-all-apply 'vla-get-Measurement (list obj)))
      (if (and (not (vl-catch-all-error-p mr)) (numberp mr))
        (progn
          (setq m (abs mr) lf (qc:dimlfac obj))
          (if (and (> (abs lf) 1e-9) (not (equal lf 1.0 1e-6))) (setq m (* m lf)))
          (if (> m 0.0) m nil))
        nil)))
)
(defun qc:dim-localpos (obj / a b p1 p2 tp)
  (setq a (vl-catch-all-apply 'vla-get-ExtLine1Point (list obj))
        b (vl-catch-all-apply 'vla-get-ExtLine2Point (list obj)))
  (if (and (not (vl-catch-all-error-p a)) (not (vl-catch-all-error-p b)))
    (setq p1 (qc:safearray-xy a) p2 (qc:safearray-xy b)))
  (if (and p1 p2)
    (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0))
    (progn
      (setq tp (vl-catch-all-apply 'vla-get-TextPosition (list obj)))
      (if (not (vl-catch-all-error-p tp)) (qc:safearray-xy tp) nil)))
)

;;; -------------------------------------------------------------------
;;; Block-name filters
;;; -------------------------------------------------------------------
(defun qc:raw-name (br / r)
  (setq r (vl-catch-all-apply 'vla-get-Name (list br)))
  (if (and (not (vl-catch-all-error-p r)) (= (type r) 'STR)) r "")
)
(defun qc:eff-name (br / r)
  (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list br)))
  (if (and (not (vl-catch-all-error-p r)) (= (type r) 'STR)) r (qc:raw-name br))
)
(defun qc:ignored-block-p (br / nm)
  (setq nm (strcase (qc:eff-name br)))
  (wcmatch nm *qc-ignore-blocks*)
)

;;; -------------------------------------------------------------------
;;; Recursive collection (world coordinates via transform matrix)
;;;   *qc-dims*  element : (worldpos value)
;;;   *qc-texts* element : (worldpos nums sourcetext)
;;; -------------------------------------------------------------------
;; Dim entry shape:  (worldpos value handle)
(defun qc:add-dim (obj mtx / val lp)
  (setq val (qc:dim-value obj) lp (qc:dim-localpos obj))
  (if (and (numberp val) (qc:xy-p lp))
    (setq *qc-dims* (cons (list (qc:m-apply mtx lp) val (qc:obj-handle obj)) *qc-dims*)))
)
;; Text entry shape:  (worldpos nums sourcetext handle)
(defun qc:add-text-string (txt lp mtx handle / nums)
  (if (and (= (type txt) 'STR) (qc:xy-p lp))
    (progn
      (setq nums (qc:dim-numbers txt))
      (if nums
        (setq *qc-texts* (cons (list (qc:m-apply mtx lp) nums txt handle) *qc-texts*)))))
)
(defun qc:add-text (obj mtx / oname tr txt lp)
  (setq tr (vl-catch-all-apply 'vla-get-TextString (list obj)))
  (if (and (not (vl-catch-all-error-p tr)) (= (type tr) 'STR))
    (progn
      (setq oname (vla-get-ObjectName obj)
            txt   (if (wcmatch oname "*MText*") (qc:strip-mtext tr) tr)
            lp    (qc:obj-insertion obj))
      (qc:add-text-string txt lp mtx (qc:obj-handle obj))))
)
(defun qc:add-mleader (obj mtx / tr lr txt lp)
  (setq tr (vl-catch-all-apply 'vla-get-TextString   (list obj))
        lr (vl-catch-all-apply 'vla-get-TextLocation (list obj)))
  (if (and (not (vl-catch-all-error-p tr)) (= (type tr) 'STR)
           (not (vl-catch-all-error-p lr)))
    (progn
      (setq txt (qc:strip-mtext tr) lp (qc:safearray-xy lr))
      (qc:add-text-string txt lp mtx (qc:obj-handle obj))))
)
(defun qc:add-attributes (br mtx / hr ar al att lp tr txt)
  (setq hr (vl-catch-all-apply 'vla-get-HasAttributes (list br)))
  (if (and (not (vl-catch-all-error-p hr)) hr)
    (progn
      (setq ar (vl-catch-all-apply 'vla-GetAttributes (list br)))
      (if (not (vl-catch-all-error-p ar))
        (progn
          (setq al (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value ar))))
          (if (and (not (vl-catch-all-error-p al)) (listp al))
            (foreach att al
              (setq tr (vl-catch-all-apply 'vla-get-TextString (list att)))
              (if (and (not (vl-catch-all-error-p tr)) (= (type tr) 'STR))
                (progn
                  (setq lp (qc:obj-insertion att))
                  (qc:add-text-string tr lp mtx (qc:obj-handle att)))))))))) ; attrib pos already world; mtx=identity at top
)
(defun qc:descend-blockref (br mtx depth / lmtx cmtx nm def)
  (if (not (qc:ignored-block-p br))
    (progn
      (qc:add-attributes br mtx)
      (if (< depth *qc-max-depth*)
        (progn
          (setq lmtx (qc:blockref-matrix br))
          (if lmtx
            (progn
              (setq cmtx (qc:m-compose mtx lmtx)
                    nm   (qc:raw-name br))
              (if (and nm (> (strlen nm) 0))
                (progn
                  (setq def (vl-catch-all-apply 'vla-Item (list *qc-blocks* nm)))
                  (if (not (vl-catch-all-error-p def))
                    (qc:collect-space def cmtx (1+ depth)))))))))))
)
(defun qc:collect-space (space mtx depth / obj oname)
  (vlax-for obj space
    (setq oname (vla-get-ObjectName obj))
    (cond
      ((qc:linear-dim-p oname)              (qc:add-dim obj mtx))
      ((wcmatch oname "AcDbText,AcDbMText") (qc:add-text obj mtx))
      ((= oname "AcDbMLeader")              (qc:add-mleader obj mtx))
      ((= oname "AcDbBlockReference")       (qc:descend-blockref obj mtx depth))))
)
(defun qc:collect-layouts (doc / layouts layout lname blk)
  (setq layouts (vl-catch-all-apply 'vla-get-Layouts (list doc)))
  (if (not (vl-catch-all-error-p layouts))
    (vlax-for layout layouts
      (setq lname (vl-catch-all-apply 'vla-get-Name (list layout)))
      (if (and (not (vl-catch-all-error-p lname)) (= (type lname) 'STR)
               (not (= (strcase lname) "MODEL")))
        (progn
          (setq blk (vl-catch-all-apply 'vla-get-Block (list layout)))
          (if (not (vl-catch-all-error-p blk))
            (qc:collect-space blk (qc:m-identity) 0))))))
)
(defun qc:collect (doc)
  (setq *qc-dims* nil *qc-texts* nil
        *qc-blocks*   (vla-get-Blocks doc)
        *qc-doc-lfac* (qc:doc-dimlfac doc))
  (qc:collect-space (vla-get-ModelSpace doc) (qc:m-identity) 0)
  (qc:collect-layouts doc)
  (list *qc-dims* *qc-texts*)
)

;;; -------------------------------------------------------------------
;;; Position matching   (raw coordinates -- same space in both drawings)
;;;   element shape: (pos . rest)  ; (car e) is the XY position
;;;   returns: (matched unmatched-inch unmatched-metric)
;;;   matched element: (inchElem metricElem normalized-distance)
;;; -------------------------------------------------------------------
(defun qc:centroid (lst / n sx sy p)
  (setq n 0 sx 0.0 sy 0.0)
  (foreach e lst
    (setq p (car e))
    (if (qc:xy-p p) (setq sx (+ sx (car p)) sy (+ sy (cadr p)) n (1+ n))))
  (if (> n 0) (list (/ sx n) (/ sy n)) nil)
)
(defun qc:spread (lst c / n s p dx dy)
  (setq n 0 s 0.0)
  (foreach e lst
    (setq p (car e))
    (if (qc:xy-p p)
      (setq dx (- (car p) (car c)) dy (- (cadr p) (cadr c))
            s (+ s (* dx dx) (* dy dy)) n (1+ n))))
  (if (> n 0) (sqrt (/ s n)) 0.0)
)
;; Value signature of an entry: dim -> its number; text -> its first number.
(defun qc:entry-vkey (e / v)
  (setq v (cadr e))
  (cond ((numberp v) v)
        ((and (listp v) v (numberp (car v))) (car v))
        (T nil))
)
;; Collapse COINCIDENT DUPLICATES: entries at essentially the same position
;; AND the same value are one requirement, not two (common from copy-paste,
;; or a block + overlapping text).  posTol is a tiny fraction of drawing
;; spread, so two DISTINCT holes/callouts (always meaningfully apart) are
;; never merged -- only literal overlaps.  This stops a duplicated inch
;; callout from producing a phantom MISSING when metric carries it once.
(defun qc:dedup-coincident (lst / c sp posTol kept e p vk k dup)
  (setq c (qc:centroid lst))
  (if (not c)
    lst
    (progn
      (setq sp (qc:spread lst c))
      (if (<= sp 1e-9) (setq sp 1.0))
      (setq posTol (* sp 1.0e-4) kept nil)   ; 0.01% of spread = truly coincident
      (foreach e lst
        (setq p (car e) vk (qc:entry-vkey e) dup nil)
        (foreach k kept
          (if (and (not dup) (qc:xy-p p) (qc:xy-p (car k))
                   (< (qc:dist p (car k)) posTol)
                   vk (qc:entry-vkey k)
                   (equal vk (qc:entry-vkey k) 1e-6))
            (setq dup T)))
        (if (not dup) (setq kept (cons e kept))))
      (reverse kept)))
)
;; EXACT 1:1 match by entity HANDLE -- the primary matcher.
;;   An in-place conversion keeps each entity's handle, so the inch dim and its
;;   metric counterpart share a handle.  Pairing on handle is exact: no gate, no
;;   ambiguity, and -- crucially -- it makes EXTRA / MISSING impossible whenever
;;   handles are preserved (every inch entity has its metric twin and vice versa).
;;   hidx = index of the handle within an entry (dims: 2, texts: 3).
;;   Returns (matched unmatched-inch unmatched-metric); matched = (ie me 0.0).
;;   Entries with no handle (nil) are passed through as unmatched for the
;;   position fallback, so nothing is lost if a converter rebuilt entities.
(defun qc:match-by-handle (inchL metricL hidx
                           / mtable i e h cell matched usedM inchLeft metricLeft)
  (setq mtable nil i 0)
  (foreach e metricL
    (setq h (nth hidx e))
    (if (and h (= (type h) 'STR)) (setq mtable (cons (cons h i) mtable)))
    (setq i (1+ i)))
  (setq matched nil usedM nil inchLeft nil)
  (foreach e inchL
    (setq h    (nth hidx e)
          cell (if (and h (= (type h) 'STR)) (assoc h mtable) nil))
    (if (and cell (not (qc:member (cdr cell) usedM)))
      (setq matched (cons (list e (nth (cdr cell) metricL) 0.0) matched)
            usedM   (cons (cdr cell) usedM))
      (setq inchLeft (cons e inchLeft))))
  (setq metricLeft nil i 0)
  (foreach e metricL
    (if (not (qc:member i usedM)) (setq metricLeft (cons e metricLeft)))
    (setq i (1+ i)))
  (list (reverse matched) (reverse inchLeft) (reverse metricLeft))
)
;; Value-agreement predicates used as a tie-breaker during position matching.
(defun qc:dim-valok (ie me)
  (<= (abs (- (abs (cadr me)) (* (abs (cadr ie)) *qc-conv*))) *qc-tol*)
)
(defun qc:txt-valok (ie me / inums mnums pairs ok q)
  (setq inums (cadr ie) mnums (cadr me))
  (if (and inums mnums (= (length inums) (length mnums)))
    (progn
      (setq pairs (qc:best-pairs inums mnums)
            ok    (and pairs (= (length pairs) (length inums))))
      (if ok
        (progn (foreach q pairs (if (> (cadddr q) *qc-tol*) (setq ok nil))) ok)
        nil))
    nil)
)
;; Position matcher.  valfn (or nil) is a (lambda (ie me) -> T/nil) value check;
;; when it returns T the candidate's position score is reduced by *qc-valbonus*
;; so a same-location, correctly-converted pair is preferred over a coincidental
;; neighbour -- WITHOUT raising the spatial gate (gate still tests raw distance).
(defun qc:match (inchL metricL gate valfn
                 / cm sm cand i j ie me d vok score sorted usedI usedM matched ui um c)
  (setq cm (qc:centroid metricL))
  (if (not cm)
    (list nil inchL metricL)
    (progn
      (setq sm (qc:spread metricL cm))
      (if (<= sm 1e-9) (setq sm 1.0))
      (setq cand nil i 0)
      (foreach ie inchL
        (if (qc:xy-p (car ie))
          (progn
            (setq j 0)
            (foreach me metricL
              (if (qc:xy-p (car me))
                (progn
                  (setq d (/ (qc:dist (car ie) (car me)) sm))
                  (if (< d gate)
                    (progn
                      (setq vok   (if valfn (apply valfn (list ie me)) nil)
                            score (* d (if vok *qc-valbonus* 1.0)))
                      (setq cand (cons (list score i j ie me) cand))))))
              (setq j (1+ j)))))
        (setq i (1+ i)))
      (setq sorted (vl-sort cand '(lambda (a b) (< (car a) (car b))))
            usedI nil usedM nil matched nil)
      (foreach c sorted
        (if (and (not (qc:member (cadr c) usedI)) (not (qc:member (caddr c) usedM)))
          (setq matched (cons (list (nth 3 c) (nth 4 c) (car c)) matched)
                usedI   (cons (cadr c) usedI)
                usedM   (cons (caddr c) usedM))))
      (setq ui nil i 0)
      (foreach ie inchL (if (not (qc:member i usedI)) (setq ui (cons ie ui))) (setq i (1+ i)))
      (setq um nil j 0)
      (foreach me metricL (if (not (qc:member j usedM)) (setq um (cons me um))) (setq j (1+ j)))
      (list (reverse matched) (reverse ui) (reverse um))))
)

;;; Greedy best pairing of two number lists (for multi-number text strings)
;;; returns list of (inchNum metricNum expected diff)
(defun qc:best-pairs (inums mnums / cand i j iv mv ex df sorted sel ui um c)
  (setq cand nil i 0)
  (foreach iv inums
    (setq j 0)
    (foreach mv mnums
      (if (and (numberp iv) (numberp mv))
        (progn
          (setq ex (* (abs iv) *qc-conv*) df (abs (- (abs mv) ex)))
          (setq cand (cons (list df i j iv mv ex) cand))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (setq sorted (vl-sort cand '(lambda (a b) (< (car a) (car b)))) sel nil ui nil um nil)
  (foreach c sorted
    (if (and (not (qc:member (cadr c) ui)) (not (qc:member (caddr c) um)))
      (setq sel (cons (list (nth 3 c) (nth 4 c) (nth 5 c) (car c)) sel)
            ui  (cons (cadr c) ui)
            um  (cons (caddr c) um))))
  (reverse sel)
)

;;; Value-rescue pass.
;;;   "MISSING" must mean a converted value appears NOWHERE in the metric
;;;   drawing -- not merely that greedy position-matching stranded an entity
;;;   in a cluster of near-identical callouts (e.g., a 12x hole pattern with
;;;   leader copies).  After position matching, each still-unmatched inch
;;;   entity is paired with the NEAREST still-unmatched metric entity whose
;;;   value converts correctly (valfn = T).  If no metric value matches, it
;;;   stays MISSING -- so a genuinely un-converted dimension is still caught.
;;;   Returns (rescued-pairs still-missing-inch still-extra-metric).
(defun qc:value-rescue (missI missM valfn
                        / pairs stillI stillM usedM ie me j d bestMe bestJ bestD)
  (setq pairs nil stillI nil usedM nil)
  (foreach ie missI
    (setq bestMe nil bestJ nil bestD 1.0e99 j 0)
    (foreach me missM
      (if (and (not (qc:member j usedM)) (apply valfn (list ie me)))
        (progn
          (setq d (if (and (qc:xy-p (car ie)) (qc:xy-p (car me)))
                    (qc:dist (car ie) (car me))
                    0.0))
          (if (< d bestD) (setq bestD d bestMe me bestJ j))))
      (setq j (1+ j)))
    (if bestMe
      (setq pairs (cons (list ie bestMe 0.0) pairs)
            usedM (cons bestJ usedM))
      (setq stillI (cons ie stillI))))
  (setq stillM nil j 0)
  (foreach me missM
    (if (not (qc:member j usedM)) (setq stillM (cons me stillM)))
    (setq j (1+ j)))
  (list (reverse pairs) (reverse stillI) (reverse stillM))
)

;;; -------------------------------------------------------------------
;;; Labels
;;; -------------------------------------------------------------------
(defun qc:lbl-ok () "\\U+2713")
(defun qc:lbl-cmp (iv ex mv)
  (strcat (qc:fmt iv) "\" exp " (qc:fmt ex) " got " (qc:fmt mv) "mm")
)
(defun qc:lbl-miss (iv ex)
  (strcat "MISSING  " (qc:fmt iv) "\" exp " (qc:fmt ex) "mm")
)
(defun qc:lbl-extra (mv)
  (strcat "EXTRA  " (qc:fmt mv) "mm")
)

;;; -------------------------------------------------------------------
;;; ObjectDBX open / close
;;; -------------------------------------------------------------------
(defun qc:create-dbx (acadObj / versions prog dbx)
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
(defun qc:open-inch (acadObj inchFile / dbx openRes visibleRes)
  (setq dbx (qc:create-dbx acadObj))
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
(defun qc:close-inch (doc isDbx)
  (if doc
    (if isDbx
      (vl-catch-all-apply 'vlax-release-object (list doc))
      (vl-catch-all-apply 'vla-close (list doc vlax-false))))
)

;;; -------------------------------------------------------------------
;;; Layers / balloons
;;; -------------------------------------------------------------------
(defun qc:balloon-height (/ dtxt dscl sz)
  (setq dtxt (getvar "DIMTXT") dscl (getvar "DIMSCALE")
        sz   (* (max dtxt 0.05) (max dscl 1.0) 0.85))
  (max sz 0.5)
)
(defun qc:ensure-layer (doc lname color / layers layerRes addRes)
  (setq layers   (vla-get-Layers doc)
        layerRes (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (vl-catch-all-error-p layerRes)
    (progn
      (setq addRes (vl-catch-all-apply 'vla-add (list layers lname)))
      (if (not (vl-catch-all-error-p addRes)) (vla-put-Color addRes color)))
    (vla-put-Color layerRes color))
)
(defun qc:delete-layer (lname / doc layers layerRes delRes)
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
(defun qc:clear-layers (/ ss i lname)
  (foreach lname (list "MC_PASS" "MC_ERRORS")
    (setq ss (ssget "X" (list (cons 8 lname))))
    (if ss (progn (setq i 0) (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i)))))
    (qc:delete-layer lname))
)
(defun qc:place-balloon (px py bh isPass body / ins label layer color bw ed)
  (setq ins (list px (+ py (* bh 0.75)) 0.0))
  (if isPass (setq layer "MC_PASS" color 3) (setq layer "MC_ERRORS" color 7))
  (setq label (strcat "{\\fArial|b1|i0;" body "}"))
  (setq bw (max (* (strlen label) bh 0.50) (* bh 3.0)))
  (setq ed (list (cons 0 "MTEXT") (cons 100 "AcDbEntity") (cons 8 layer) (cons 62 color)
                 (cons 100 "AcDbMText") (cons 10 ins) (cons 40 bh) (cons 41 bw)
                 (cons 71 1) (cons 72 1) (cons 1 label)))
  (vl-catch-all-apply 'entmake (list ed))
)
(defun qc:dwg-folder (/ p)
  (setq p (getvar "DWGPREFIX"))
  (if (or (not p) (= p "")) (setq p ""))
  p
)

;;; ===================================================================
;;; MAIN  --  METRIC_CHECK / MQC
;;; ===================================================================
(defun c:metric_check
    (/ *error* oldError oldCmd acadObj metricDoc metricDir inchFile inchOpen inchDoc inchIsDbx
       mc-res ic-res metricDims metricTexts inchDims inchTexts metricLfac inchLfac
       dimRes dMatched dMiss dExtra txtRes tMatched tMiss tExtra dResc tResc
       dimMarks txtMarks dimPass dimFail txtPass txtFail
       missDim extraDim missTxt extraTxt
       pr ie me iv mv exp df pass bp inums mnums pairs body bad q qi qm qe qd
       balloonH errIdx m)

  (vl-load-com)
  (setq oldError *error* oldCmd (getvar "CMDECHO")
        *qc-active-inch-doc* nil *qc-active-inch-dbx* nil)

  (defun *error* (msg)
    (if *qc-active-inch-doc* (qc:close-inch *qc-active-inch-doc* *qc-active-inch-dbx*))
    (setq *qc-active-inch-doc* nil *qc-active-inch-dbx* nil)
    (if metricDoc (vl-catch-all-apply 'vla-Activate (list metricDoc)))
    (if oldCmd (setvar "CMDECHO" oldCmd))
    (setq *error* oldError)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nMETRIC_QC error: " msg)))
    (princ))

  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj)
        metricDir (qc:dwg-folder))

  (princ "\n[METRIC_QC v2.3] Reading metric (active) drawing...")
  (setq mc-res      (qc:collect metricDoc)
        metricDims  (car  mc-res)
        metricTexts (cadr mc-res)
        metricLfac  *qc-doc-lfac*)
  (princ (strcat " " (itoa (length metricDims)) " dim(s), "
                     (itoa (length metricTexts)) " text(s)."))

  (setq inchFile (getfiled "Select Inch Source Drawing" metricDir "dwg" 4))
  (if (not inchFile)
    (progn (princ "\nCancelled.") (setq *error* oldError) (princ) (exit)))

  (princ "\nOpening inch source...")
  (setq inchOpen (qc:open-inch acadObj inchFile))
  (if (not inchOpen)
    (progn (princ "\nERROR: could not open inch drawing.") (setq *error* oldError) (princ) (exit)))
  (setq inchDoc (car inchOpen) inchIsDbx (cadr inchOpen)
        *qc-active-inch-doc* inchDoc *qc-active-inch-dbx* inchIsDbx)
  (princ (if inchIsDbx " ObjectDBX OK." " visible fallback."))

  (princ "\nReading inch drawing...")
  (setq ic-res    (qc:collect inchDoc)
        inchDims  (car  ic-res)
        inchTexts (cadr ic-res)
        inchLfac  *qc-doc-lfac*)
  (princ (strcat " " (itoa (length inchDims)) " dim(s), "
                     (itoa (length inchTexts)) " text(s)."))

  (qc:close-inch inchDoc inchIsDbx)
  (setq *qc-active-inch-doc* nil *qc-active-inch-dbx* nil)
  (vla-Activate metricDoc)

  (setq dimMarks nil txtMarks nil
        dimPass 0 dimFail 0 txtPass 0 txtFail 0
        missDim 0 extraDim 0 missTxt 0 extraTxt 0)

  ;; ----------------------- DIMENSIONS -------------------------------
  (princ "\nMatching dimensions by position...")
  ;; Phase 1 -- EXACT handle match. In-place conversion keeps entity handles,
  ;; so this pairs every dim 1:1 with its converted twin. No EXTRA/MISSING here.
  (setq dimRes   (qc:match-by-handle inchDims metricDims 2)
        dMatched (car   dimRes)
        dMiss    (cadr  dimRes)
        dExtra   (caddr dimRes))
  ;; Phase 2 -- position match on any handle leftovers (only if the converter
  ;; rebuilt entities and changed handles). Dedup coincident dups on leftovers.
  (setq dimRes   (qc:match (qc:dedup-coincident dMiss) (qc:dedup-coincident dExtra)
                           *qc-dim-gate* 'qc:dim-valok)
        dMatched (append dMatched (car dimRes))
        dMiss    (cadr  dimRes)
        dExtra   (caddr dimRes))
  ;; Phase 3 -- value-rescue: a leftover inch dim whose converted value exists
  ;; in a leftover metric dim is paired, not called MISSING.
  (setq dResc    (qc:value-rescue dMiss dExtra 'qc:dim-valok)
        dMatched (append dMatched (car dResc))
        dMiss    (cadr  dResc)
        dExtra   (caddr dResc))

  (foreach pr dMatched
    (setq ie   (car pr)   me (cadr pr)
          iv   (cadr ie)  mv (cadr me)
          exp  (* (abs iv) *qc-conv*)
          df   (abs (- (abs mv) exp))
          pass (<= df *qc-tol*)
          bp   (car me))
    (if pass (setq dimPass (1+ dimPass)) (setq dimFail (1+ dimFail)))
    (setq dimMarks
      (cons (list pass (if pass (qc:lbl-ok) (qc:lbl-cmp iv exp mv)) bp) dimMarks)))

  (foreach ie dMiss
    (setq iv (cadr ie) exp (* (abs iv) *qc-conv*) bp (car ie)
          dimFail (1+ dimFail) missDim (1+ missDim))
    (setq dimMarks (cons (list nil (qc:lbl-miss iv exp) bp) dimMarks)))

  (foreach me dExtra
    (setq mv (cadr me) bp (car me) dimFail (1+ dimFail) extraDim (1+ extraDim))
    (setq dimMarks (cons (list nil (qc:lbl-extra mv) bp) dimMarks)))

  (princ (strcat " done. " (itoa dimPass) " pass, " (itoa dimFail) " fail."))

  ;; ----------------------- TEXT / ATTRIB ----------------------------
  (princ "\nMatching text by position...")
  ;; Phase 1 -- exact handle match (handle is at index 3 for text entries).
  (setq txtRes   (qc:match-by-handle inchTexts metricTexts 3)
        tMatched (car   txtRes)
        tMiss    (cadr  txtRes)
        tExtra   (caddr txtRes))
  ;; Phase 2 -- position match on leftovers.
  (setq txtRes   (qc:match (qc:dedup-coincident tMiss) (qc:dedup-coincident tExtra)
                           *qc-txt-gate* 'qc:txt-valok)
        tMatched (append tMatched (car txtRes))
        tMiss    (cadr  txtRes)
        tExtra   (caddr txtRes))
  ;; Phase 3 -- value-rescue leftovers.
  (setq tResc    (qc:value-rescue tMiss tExtra 'qc:txt-valok)
        tMatched (append tMatched (car tResc))
        tMiss    (cadr  tResc)
        tExtra   (caddr tResc))

  (foreach pr tMatched
    (setq ie    (car pr)   me (cadr pr)
          inums (cadr ie)  mnums (cadr me) bp (car me)
          pairs (qc:best-pairs inums mnums))
    (if (and pairs (= (length inums) (length mnums)) (= (length pairs) (length inums)))
      (progn
        (setq body nil bad nil)
        (foreach q pairs
          (setq qi (car q) qm (cadr q) qe (caddr q) qd (cadddr q))
          (if (> qd *qc-tol*) (setq bad T))
          (setq body (if body (strcat body "  |  " (qc:lbl-cmp qi qe qm))
                              (qc:lbl-cmp qi qe qm))))
        (if bad (setq txtFail (1+ txtFail)) (setq txtPass (1+ txtPass)))
        (setq txtMarks (cons (list (not bad) (if bad body (qc:lbl-ok)) bp) txtMarks)))
      (progn
        ;; count mismatch (macro dropped/added a number) -> fail
        (setq txtFail (1+ txtFail))
        (setq txtMarks
          (cons (list nil
                      (qc:lbl-cmp (car inums) (* (abs (car inums)) *qc-conv*)
                                  (if mnums (car mnums) 0.0))
                      bp)
                txtMarks)))))

  (foreach ie tMiss
    (setq inums (cadr ie) bp (car ie) txtFail (1+ txtFail) missTxt (1+ missTxt))
    (setq txtMarks
      (cons (list nil (qc:lbl-miss (car inums) (* (abs (car inums)) *qc-conv*)) bp) txtMarks)))

  (foreach me tExtra
    (setq mnums (cadr me) bp (car me) txtFail (1+ txtFail) extraTxt (1+ extraTxt))
    (setq txtMarks (cons (list nil (qc:lbl-extra (car mnums)) bp) txtMarks)))

  (princ (strcat " done. " (itoa txtPass) " pass, " (itoa txtFail) " fail."))

  ;; ----------------------- OUTPUT -----------------------------------
  (princ "\nPlacing balloons...")
  (qc:clear-layers)
  (qc:ensure-layer metricDoc "MC_PASS"   3)
  (qc:ensure-layer metricDoc "MC_ERRORS" 7)
  (setq balloonH (qc:balloon-height) errIdx 1)

  (foreach m (reverse dimMarks)
    (qc:place-balloon (car (caddr m)) (cadr (caddr m)) balloonH (car m) (cadr m))
    (if (not (car m))
      (progn (princ (strcat "\n  [" (itoa errIdx) "] DIM: " (cadr m))) (setq errIdx (1+ errIdx)))))
  (foreach m (reverse txtMarks)
    (qc:place-balloon (car (caddr m)) (cadr (caddr m)) balloonH (car m) (cadr m))
    (if (not (car m))
      (progn (princ (strcat "\n  [" (itoa errIdx) "] TXT: " (cadr m))) (setq errIdx (1+ errIdx)))))

  (if (zerop (+ dimFail txtFail)) (princ "\nAll checked conversions PASSED."))
  (vla-Regen metricDoc 2)

  (princ
    (strcat
      "\n--------------------------------------------\n"
      "METRIC_QC v2.3  --  HANDLE-match (exact 1:1) -> position -> value-rescue\n"
      "  Dimensions : " (itoa dimPass) " pass   " (itoa dimFail) " fail\n"
      "  Text/Attr  : " (itoa txtPass) " pass   " (itoa txtFail) " fail\n"
      "  -- dim missing (inch has, metric lacks) : " (itoa missDim) "\n"
      "  -- dim extra   (metric has, inch lacks) : " (itoa extraDim) "\n"
      "  -- text missing                         : " (itoa missTxt) "\n"
      "  -- text extra                           : " (itoa extraTxt) "\n"
      "  Inch source : " (if inchIsDbx "ObjectDBX invisible" "visible fallback") "\n"
      "  DIMLFAC     : metric=" (qc:fmt metricLfac) "  inch=" (qc:fmt inchLfac) "\n"
      "  Ignored blocks: " *qc-ignore-blocks* "\n"
      "--------------------------------------------"))
  (if oldCmd (setvar "CMDECHO" oldCmd))
  (setq *error* oldError)
  (princ)
)
(defun c:mqc () (c:metric_check))

;;; ===================================================================
;;; MQC_DIAG  --  text-only dump (no balloons)
;;; ===================================================================
(defun c:mqc_diag
    (/ *error* oldError acadObj metricDoc metricDir inchFile inchOpen inchDoc inchIsDbx
       mc-res ic-res metricDims inchDims metricTexts inchTexts metricLfac inchLfac
       dimRes dMatched dMiss dExtra dResc txtRes tMatched tMiss tExtra tResc
       pr ie me iv mv exp df)
  (vl-load-com)
  (setq oldError *error*)
  (defun *error* (msg)
    (if *qc-active-inch-doc* (qc:close-inch *qc-active-inch-doc* *qc-active-inch-dbx*))
    (setq *qc-active-inch-doc* nil *qc-active-inch-dbx* nil)
    (setq *error* oldError)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nMQC_DIAG error: " msg)))
    (princ))

  (setq acadObj   (vlax-get-acad-object)
        metricDoc (vla-get-ActiveDocument acadObj)
        metricDir (qc:dwg-folder))
  (princ "\n=== MQC_DIAG v2.3 ===")
  (princ "\nReading metric...")
  (setq mc-res (qc:collect metricDoc) metricDims (car mc-res) metricTexts (cadr mc-res)
        metricLfac *qc-doc-lfac*)
  (princ (strcat " " (itoa (length metricDims)) " dim(s), " (itoa (length metricTexts))
                 " text(s).  DIMLFAC=" (qc:fmt metricLfac)))

  (setq inchFile (getfiled "Select Inch Source Drawing" metricDir "dwg" 4))
  (if (not inchFile)
    (progn (princ "\nCancelled.") (setq *error* oldError) (princ) (exit)))
  (setq inchOpen (qc:open-inch acadObj inchFile))
  (if (not inchOpen)
    (progn (princ "\nERROR: could not open.") (setq *error* oldError) (princ) (exit)))
  (setq inchDoc (car inchOpen) inchIsDbx (cadr inchOpen)
        *qc-active-inch-doc* inchDoc *qc-active-inch-dbx* inchIsDbx)
  (princ "\nReading inch...")
  (setq ic-res (qc:collect inchDoc) inchDims (car ic-res) inchTexts (cadr ic-res)
        inchLfac *qc-doc-lfac*)
  (princ (strcat " " (itoa (length inchDims)) " dim(s), " (itoa (length inchTexts))
                 " text(s).  DIMLFAC=" (qc:fmt inchLfac)))
  (qc:close-inch inchDoc inchIsDbx)
  (setq *qc-active-inch-doc* nil *qc-active-inch-dbx* nil)
  (princ (strcat "\n>> metric DIMLFAC=" (qc:fmt metricLfac) "  inch DIMLFAC=" (qc:fmt inchLfac)
                 "  (if metric=25.4 the converter used display-scaling, not geometry rescale)"))

  ;; ----- DIMENSIONS:  handle match -> position match -> value rescue -----
  (setq dimRes   (qc:match-by-handle inchDims metricDims 2)
        dMatched (car dimRes) dMiss (cadr dimRes) dExtra (caddr dimRes))
  (princ (strcat "\n>> dim handle-matches=" (itoa (length dMatched))
                 "  handle-leftover inch=" (itoa (length dMiss))
                 "  metric=" (itoa (length dExtra))))
  (setq dimRes   (qc:match (qc:dedup-coincident dMiss) (qc:dedup-coincident dExtra)
                           *qc-dim-gate* 'qc:dim-valok)
        dMatched (append dMatched (car dimRes))
        dMiss    (cadr dimRes) dExtra (caddr dimRes))
  (setq dResc    (qc:value-rescue dMiss dExtra 'qc:dim-valok)
        dMatched (append dMatched (car dResc))
        dMiss    (cadr dResc) dExtra (caddr dResc))

  (princ "\n\n=== DIMENSIONS ===")
  (princ "\n--- MATCHED ---")
  (foreach pr dMatched
    (setq ie (car pr) me (cadr pr) iv (cadr ie) mv (cadr me)
          exp (* (abs iv) *qc-conv*) df (abs (- (abs mv) exp)))
    (princ (strcat "\n  " (if (<= df *qc-tol*) "PASS  " "FAIL  ")
                   (qc:fmt iv) "\" -> " (qc:fmt mv) "mm  (exp " (qc:fmt exp)
                   "  diff " (qc:fmt df) ")")))
  (princ "\n--- MISSING (converted value found NOWHERE in metric) ---")
  (foreach ie dMiss
    (princ (strcat "\n  " (qc:fmt (cadr ie)) "\"  exp " (qc:fmt (* (abs (cadr ie)) *qc-conv*)) "mm")))
  (princ "\n--- EXTRA (metric dim, no inch source) ---")
  (foreach me dExtra
    (princ (strcat "\n  " (qc:fmt (cadr me)) "mm")))

  ;; ----- TEXT / CALLOUTS:  handle match -> position match -> value rescue -----
  (setq txtRes   (qc:match-by-handle inchTexts metricTexts 3)
        tMatched (car txtRes) tMiss (cadr txtRes) tExtra (caddr txtRes))
  (princ (strcat "\n>> text handle-matches=" (itoa (length tMatched))
                 "  handle-leftover inch=" (itoa (length tMiss))
                 "  metric=" (itoa (length tExtra))))
  (setq txtRes   (qc:match (qc:dedup-coincident tMiss) (qc:dedup-coincident tExtra)
                           *qc-txt-gate* 'qc:txt-valok)
        tMatched (append tMatched (car txtRes))
        tMiss    (cadr txtRes) tExtra (caddr txtRes))
  (setq tResc    (qc:value-rescue tMiss tExtra 'qc:txt-valok)
        tMatched (append tMatched (car tResc))
        tMiss    (cadr tResc) tExtra (caddr tResc))

  (princ "\n\n=== TEXT / CALLOUTS ===")
  (princ "\n--- MATCHED ---")
  (foreach pr tMatched
    (setq ie (car pr) me (cadr pr))
    (princ (strcat "\n  [" (qc:fmt (car (cadr ie))) "in] <-> ["
                   (qc:fmt (car (cadr me))) "mm]   inch=\"" (caddr ie)
                   "\"  metric=\"" (caddr me) "\"")))
  (princ "\n--- MISSING (converted value found NOWHERE in metric) ---")
  (foreach ie tMiss
    (princ (strcat "\n  first=" (qc:fmt (car (cadr ie))) "in  src=\"" (caddr ie) "\"")))
  (princ "\n--- EXTRA (metric text, no inch source) ---")
  (foreach me tExtra
    (princ (strcat "\n  first=" (qc:fmt (car (cadr me))) "mm  src=\"" (caddr me) "\"")))

  (princ "\n\n=== end MQC_DIAG ===\n")
  (setq *error* oldError)
  (princ)
)

;;; ===================================================================
;;; MQC_CLEAR
;;; ===================================================================
(defun c:mqc_clear ()
  (vl-load-com)
  (qc:clear-layers)
  (vla-Regen (vla-get-ActiveDocument (vlax-get-acad-object)) 2)
  (princ "\nMC_PASS / MC_ERRORS balloons and layers removed.")
  (princ)
)
(defun c:metric_clear () (c:mqc_clear))

;;; ===================================================================
;;; MQC_TEST  --  self test
;;; ===================================================================
(defun qc:check (name ok)
  (princ (strcat "\n  " (if ok "PASS " "FAIL ") name))
  ok
)
(defun c:mqc_test (/ pass fail ok nums res m)
  (setq pass 0 fail 0)
  (princ "\nMETRIC_QC v2.3 self-test")

  (setq nums (qc:dim-numbers "%%C.03 [.76]"))
  (setq ok (and (= (length nums) 2) (equal (car nums) 0.03 1e-8)))
  (if (qc:check "diameter decimals extracted" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq nums (qc:dim-numbers "SHEET 12 REV 3"))
  (if (qc:check "title-block noise ignored" (null nums)) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq nums (qc:dim-numbers "5/8-18 NF"))
  (if (qc:check "thread callout ignored" (null nums)) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq nums (qc:dim-numbers "-.125"))
  (setq ok (and nums (equal (car nums) -0.125 1e-8)))
  (if (qc:check "signed decimal -.125" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; transform matrix: rotate 90deg about origin, point (1,0) -> (0,1)
  (setq m (qc:m-from-insert (list 0.0 0.0) 1.0 1.0 (/ pi 2.0)))
  (setq res (qc:m-apply m (list 1.0 0.0)))
  (setq ok (and (equal (car res) 0.0 1e-6) (equal (cadr res) 1.0 1e-6)))
  (if (qc:check "transform matrix rotate 90" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; compose: translate then the above; point through identity*M = M
  (setq res (qc:m-apply (qc:m-compose (qc:m-identity) m) (list 1.0 0.0)))
  (setq ok (and (equal (car res) 0.0 1e-6) (equal (cadr res) 1.0 1e-6)))
  (if (qc:check "matrix compose with identity" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; POSITION MATCH: identical coordinates, value verify
  (setq res (qc:match
              (list (list (list 0.0 0.0) 2.0)    (list (list 100.0 0.0) 0.03))
              (list (list (list 0.0 0.0) 50.8)   (list (list 100.0 0.0) 0.76))
              0.20 'qc:dim-valok))
  (setq ok (= (length (car res)) 2))
  (if (qc:check "position match pairs both dims" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; missing detection: inch has a dim with no metric partner nearby
  (setq res (qc:match
              (list (list (list 0.0 0.0) 2.0) (list (list 999.0 999.0) 5.0))
              (list (list (list 0.0 0.0) 50.8))
              0.20 'qc:dim-valok))
  (setq ok (and (= (length (car res)) 1) (= (length (cadr res)) 1)))
  (if (qc:check "missing dim detected as unmatched inch" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; SHIFT TOLERANCE: metric dim nudged a little still pairs (not false-missing).
  ;; spread ~ 50 here; a 3-unit shift = 0.06 normalized < 0.20 gate.
  (setq res (qc:match
              (list (list (list 0.0 0.0) 2.0) (list (list 100.0 0.0) 1.0))
              (list (list (list 3.0 0.0) 50.8) (list (list 100.0 0.0) 25.4))
              0.20 'qc:dim-valok))
  (setq ok (and (= (length (car res)) 2) (= (length (cadr res)) 0)))
  (if (qc:check "slightly shifted dim still pairs" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; VALUE TIE-BREAKER: inch dim sits between two close metric dims that are
  ;; both inside the gate (a far anchor gives the set real spread).  The
  ;; correct-value candidate must win over the wrong-value one.
  (setq res (qc:match
              (list (list (list 202.0 0.0) 2.0))
              (list (list (list 0.0   0.0) 25.4)   ; far anchor (creates spread)
                    (list (list 200.0 0.0) 99.9)   ; wrong value, near inch
                    (list (list 204.0 0.0) 50.8))  ; correct value, near inch
              0.10 'qc:dim-valok))
  (setq ok (and (= (length (car res)) 1)
                (equal (cadr (cadr (car (car res)))) 50.8 1e-6)))
  (if (qc:check "value tie-breaker picks correct-value neighbour" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; UN-CONVERTED dim at SAME spot must still match (so it gets flagged),
  ;; not be skipped in favour of a far correct-value dim (gate tests raw
  ;; distance, so the far correct dim never enters as a candidate).
  (setq res (qc:match
              (list (list (list 0.0 0.0) 2.0))
              (list (list (list 0.0 0.0) 2.0)      ; same spot, NOT converted
                    (list (list 8.0 0.0) 50.8))    ; correct value but far
              0.10 'qc:dim-valok))
  (setq ok (and (= (length (car res)) 1)
                (equal (cadr (cadr (car (car res)))) 2.0 1e-6)))
  (if (qc:check "un-converted same-spot dim still matched (flagged)" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; value verify: 0.03 * 25.4 = 0.762 ~ 0.76 within tol
  (setq res (qc:best-pairs (list 0.03) (list 0.76)))
  (setq ok (and res (<= (cadddr (car res)) *qc-tol*)))
  (if (qc:check "0.03in -> 0.76mm passes tolerance" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; NTS catch: 703 paired with 7030 -> large diff -> fail (but paired)
  (setq res (qc:best-pairs (list 703.0) (list 7030.0)))
  (setq ok (and res (> (cadddr (car res)) *qc-tol*)))
  (if (qc:check "NTS 703->7030 flagged as fail" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; VALUE-RESCUE: inch .771 stranded by position (far metric partner) but its
  ;; converted value 19.5834 exists in an unmatched metric -> rescued, NOT missing.
  (setq res (qc:value-rescue
              (list (list (list 0.0 0.0) (list 0.771) "CBORE .771"))   ; stranded inch text
              (list (list (list 900.0 900.0) (list 19.58) "CBORE 19.58")) ; same value, far away
              'qc:txt-valok))
  (setq ok (and (= (length (car res)) 1) (= (length (cadr res)) 0)))
  (if (qc:check "value-rescue: .771 paired (not false MISSING)" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; REAL miss preserved: inch .771 but NO matching converted value anywhere.
  (setq res (qc:value-rescue
              (list (list (list 0.0 0.0) (list 0.771) "CBORE .771"))
              (list (list (list 5.0 5.0) (list 99.9) "UNRELATED 99.9"))
              'qc:txt-valok))
  (setq ok (and (= (length (car res)) 0) (= (length (cadr res)) 1)))
  (if (qc:check "value-rescue: genuine missing stays MISSING" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; COINCIDENT DEDUP: two identical dims at the same spot collapse to one;
  ;; a distinct dim elsewhere is kept.
  (setq res (qc:dedup-coincident
              (list (list (list 100.0 100.0) 2.0)
                    (list (list 100.0 100.0) 2.0)     ; exact duplicate
                    (list (list 300.0 100.0) 2.0))))  ; same value, different spot -> keep
  (setq ok (= (length res) 2))
  (if (qc:check "coincident duplicate collapsed, distinct kept" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; DEDUP must NOT merge two distinct same-value dims that are merely close.
  (setq res (qc:dedup-coincident
              (list (list (list 0.0 0.0)  5.0)
                    (list (list 50.0 0.0) 5.0)
                    (list (list 100.0 0.0) 5.0))))
  (setq ok (= (length res) 3))
  (if (qc:check "dedup keeps distinct same-value dims apart" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; HANDLE MATCH: same handle pairs regardless of position; every entity 1:1,
  ;; no EXTRA/MISSING.  Dim entry shape = (pos val handle), so handle index = 2.
  (setq res (qc:match-by-handle
              (list (list (list 0.0 0.0)   2.0 "A1")
                    (list (list 50.0 0.0)  7.13 "B2"))
              (list (list (list 999.0 9.0) 7.13 "B2")    ; far away but SAME handle
                    (list (list 0.0 0.0)   50.8 "A1"))
              2))
  (setq ok (and (= (length (car res)) 2)
                (= (length (cadr res)) 0)
                (= (length (caddr res)) 0)))
  (if (qc:check "handle match: 1:1 by handle, zero extra/missing" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; HANDLE MATCH preserves the un-converted FAIL: B2 inch 7.13 <-> metric 7.13
  ;; (handle B2) must pair so it can be flagged FAIL, not hidden.
  (setq ok nil)
  (foreach pr (car res)
    (if (and (= (cadr (car pr)) 7.13) (= (cadr (cadr pr)) 7.13)) (setq ok T)))
  (if (qc:check "handle match: un-converted dim still paired (flaggable)" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  ;; HANDLE MATCH falls back gracefully when handles are missing (nil).
  (setq res (qc:match-by-handle
              (list (list (list 0.0 0.0) 2.0 nil))
              (list (list (list 0.0 0.0) 50.8 nil))
              2))
  (setq ok (and (= (length (car res)) 0) (= (length (cadr res)) 1) (= (length (caddr res)) 1)))
  (if (qc:check "handle match: no-handle entries fall through to position" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (princ (strcat "\n  " (itoa pass) " passed, " (itoa fail) " failed."))
  (princ)
)

(princ "\nMETRIC_QC.LSP v2.3 loaded.")
(princ "\n  METRIC_CHECK (or MQC) -- run QC, place balloons on metric dwg")
(princ "\n  MQC_DIAG              -- text dump: matched / missing / extra dims")
(princ "\n  MQC_CLEAR             -- erase QC balloons + layers")
(princ "\n  MQC_TEST              -- self test")
(princ)
