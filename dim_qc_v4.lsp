;;; ============================================================================
;;;  dim_qc_v5.lsp  -  Engineering Dual-Unit Dimension QC  (Balloon Edition)
;;;  Version 5.0
;;;
;;;  FIXES vs 4.4:
;;;    - Accuracy: DQC:ok? now uses AND (not OR) so the 0.08 mm absolute gate
;;;      is ALWAYS enforced. Old OR let 3 % relative tolerance accept a 2 mm
;;;      error on a 717 mm dimension. Now 28.229" [719.02] -> FAIL.
;;;    - Trailing zeros: DQC:dimdec falls back to live DIMDEC system variable
;;;      instead of returning nil. Hard-coded default of 3 removed. This stops
;;;      8.50 from inflating to 8.500 when the VLA style read failed.
;;;    - Tolerances (full-proof): DQC:extract-tol now cascades through
;;;        (a) ~...~ stacked blocks (\S format codes) - existing
;;;        (b) %%p<num> symmetric plus/minus plain text     - NEW
;;;        (c) +<num>/<sep>-<num> asymmetric plain text     - NEW
;;;      All three patterns are detected on both inch and mm sides.
;;;
;;;  COMMANDS
;;;    DIMQC        Open settings GUI -> run check -> place balloons
;;;    DIMQC-RESET  Erase all balloons
;;;    DIMQC-DIAG   Command-line diagnostic
;;;
;;;  LAYERS
;;;    DIM_QC_PASS  colour 3  (green)
;;;    DIM_QC_FAIL  colour 1  (red)
;;; ============================================================================

(vl-load-com)

(setq DQC:MM/IN    25.4)
(setq DQC:REL-TOL  0.03)
(setq DQC:ABS-TOL  0.08)
(setq DQC:TXT-H    nil)
(setq DQC:OFFSET   nil)
(setq DQC:PASS-LAYER "DIM_QC_PASS")
(setq DQC:FAIL-LAYER "DIM_QC_FAIL")
(setq DQC:PASS-COLOR 3)
(setq DQC:FAIL-COLOR 1)


;;; ============================================================================
;;;  PART 1 - UTILITIES
;;; ============================================================================

(defun DQC:trim (s)
  (if (or (null s) (/= (type s) 'STR)) (setq s ""))
  (while (and (> (strlen s) 0) (= (substr s 1 1) " "))
    (setq s (substr s 2)))
  (while (and (> (strlen s) 0) (= (substr s (strlen s) 1) " "))
    (setq s (substr s 1 (1- (strlen s)))))
  s)

(defun DQC:find-char (s c pos / i)
  (setq i pos)
  (while (and (<= i (strlen s)) (/= (substr s i 1) c))
    (setq i (1+ i)))
  (if (<= i (strlen s)) i 0))

(defun DQC:ok? (primary alt factor / exp diff)
  ;; BOTH conditions must hold (AND, not OR).
  ;; With OR the 3 % relative gate used to pass a 2 mm error on a 717 mm dim.
  ;; With AND the absolute gate (0.08 mm) is always enforced regardless of size.
  (if (< (abs primary) 1e-9)
    (< (abs alt) 0.1)
    (progn
      (setq exp  (* (abs primary) factor)
            diff (abs (- exp (abs alt))))
      (and (<= (/ diff exp) DQC:REL-TOL)
           (<= diff DQC:ABS-TOL)))))

(defun DQC:ensure-layer (name aci doc / layers lay)
  (setq layers (vla-get-Layers doc))
  (setq lay
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers name)))
      (vla-add layers name)
      (vla-item layers name)))
  (vla-put-Color lay aci)
  lay)

;;; Detect R / diameter prefix from a raw string
(defun DQC:dim-prefix (s / su)
  (setq s (DQC:trim s) su (strcase s))
  (cond
    ((= (strlen s) 0) "")
    ((and (>= (strlen su) 3)
          (= (substr su 1 2) "%%")
          (wcmatch (substr su 3 1) "C,D")) "%%c")
    ((= (substr su 1 1) "R") "R")
    (T "")))

;;; Count decimal places from the RAW token string exactly as written.
;;; No atof, no rounding. Stops at tolerance markers.
;;; "6.2195" -> 4,  "6.00" -> 2,  "R.005" -> 3,  ".311" -> 3,  "25" -> 0
(defun DQC:count-dp-in-token (tok / s i c dot count prev-digit)
  (setq s (DQC:trim tok))
  (setq i 1)
  ;; skip %%x prefix sequences
  (while (and (<= (+ i 1) (strlen s))
              (= (substr s i 1) "%")
              (= (substr s (1+ i) 1) "%"))
    (setq i (+ i 3)))
  ;; skip non-numeric prefix (R, space, etc.)
  (while (and (<= i (strlen s))
              (setq c (substr s i 1))
              (not (wcmatch c "#"))
              (/= c "-")
              (/= c "."))
    (setq i (1+ i)))
  ;; count decimal places literally
  (setq dot 0 count 0 prev-digit nil)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (cond
      ((= c ".")
       (setq dot i i (1+ i)))
      ((wcmatch c "#")
       (if (> dot 0) (setq count (1+ count)))
       (setq prev-digit T i (1+ i)))
      ;; stop at tolerance suffix
      ((and prev-digit (or (= c "+") (= c "-") (= c "/") (= c " ") (= c "~")))
       (setq i (1+ (strlen s))))
      (T (setq i (1+ i)))))
  count)

(defun DQC:fmt (val dp)
  (rtos val 2 dp))

;;; Read DIMDEC from named dim style.
;;; Falls back to the live DIMDEC system variable so we NEVER return nil.
;;; (nil caused style-dp to default to hard-coded 3 -> spurious trailing zeros)
(defun DQC:dimdec (sname doc / styles sobj dec live)
  (setq live (fix (getvar "DIMDEC")))
  (if (null sname) live
    (progn
      (setq styles (vl-catch-all-apply 'vla-get-DimStyles (list doc)))
      (if (vl-catch-all-error-p styles) live
        (progn
          (setq sobj (vl-catch-all-apply 'vla-item (list styles sname)))
          (if (vl-catch-all-error-p sobj) live
            (progn
              (setq dec (vl-catch-all-apply
                          'vlax-get (list sobj 'PrimaryUnitsPrecision)))
              (if (or (vl-catch-all-error-p dec) (null dec)) live
                (fix dec)))))))))


;;; ============================================================================
;;;  PART 2 - MTEXT FORMAT-CODE STRIPPER
;;;
;;;  After stripping, the output string uses:
;;;    "|"        = paragraph/line break  (was \P)
;;;    "~hi^lo~"  = stacked tolerance block (was \S+hi^-lo;)
;;;                 The full content is kept so the tolerance parser
;;;                 can extract +hi and -lo values later.
;;;  Normal text passes through unchanged.
;;; ============================================================================

(defun DQC:strip (s meas alt-meas / out i ch nx sc frac)
  (setq out "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (cond
      ((= ch "\\")
       (if (> (strlen s) i)
         (progn
           (setq nx (strcase (substr s (1+ i) 1)))
           (cond
             ;; \P paragraph break or \p paragraph format code
             ((= nx "P")
              (if (and (< (+ i 2) (strlen s))
                       (wcmatch (substr s (+ i 2) 1) "@,#,,")
                       (> (DQC:find-char s ";" (+ i 2)) 0))
                (progn
                  (setq sc (DQC:find-char s ";" (+ i 2)))
                  (setq i (1+ sc)))
                (setq out (strcat out "|") i (+ i 2))))
             ((= nx "X")
              (setq out (strcat out "|") i (+ i 2)))
             ((= nx "~")
              (setq out (strcat out " ") i (+ i 2)))
             ((= nx "\\")
              (setq out (strcat out "\\") i (+ i 2)))
             ;; \S stacked fraction -> ~content~ so tolerance parser sees it
             ((= nx "S")
              (setq sc (DQC:find-char s ";" (+ i 2)))
              (if (= sc 0)
                (setq i (+ i 2))
                (progn
                  (setq frac (substr s (+ i 2) (- sc i 2)))
                  (setq out (strcat out "~" frac "~"))
                  (setq i (1+ sc)))))
             ;; format codes with semicolon argument - discard
             ((wcmatch nx "H,A,C,T,Q,W,F")
              (setq sc (DQC:find-char s ";" (+ i 2)))
              (if (= sc 0) (setq i (+ i 2)) (setq i (1+ sc))))
             ;; \U+xxxx unicode
             ((= nx "U")
              (if (and (<= (+ i 6) (strlen s)) (= (substr s (+ i 2) 1) "+"))
                (setq out (strcat out " ") i (+ i 7))
                (setq i (+ i 2))))
             ;; toggle codes
             ((wcmatch nx "L,O,K")
              (setq i (+ i 2)))
             (T
              (setq out (strcat out (substr s (1+ i) 1)) i (+ i 2)))))
         (setq i (1+ i))))

      ;; <> measurement placeholder
      ((and (= ch "<") (<= (1+ i) (strlen s)) (= (substr s (1+ i) 1) ">"))
       (if (and meas (numberp meas))
         (setq out (strcat out (rtos (abs meas) 2 6)))
         (setq out (strcat out "<>")))
       (setq i (+ i 2)))

      ;; [] or [ ] empty alternate placeholder
      ((and (= ch "[")
            (progn
              (setq sc (1+ i))
              (while (and (<= sc (strlen s)) (= (substr s sc 1) " "))
                (setq sc (1+ sc)))
              (and (<= sc (strlen s)) (= (substr s sc 1) "]"))))
       (if (and alt-meas (numberp alt-meas))
         (setq out (strcat out "[" (rtos (abs alt-meas) 2 6) "]"))
         (setq out (strcat out "[]")))
       (setq i (1+ sc)))

      ;; grouping braces - discard
      ((or (= ch "{") (= ch "}"))
       (setq i (1+ i)))

      (T
       (setq out (strcat out ch) i (1+ i)))))
  out)


;;; ============================================================================
;;;  PART 3 - TOLERANCE PARSER
;;;
;;;  Extracts the FIRST ~...~ tolerance block from a stripped string.
;;;  Content inside ~...~ is "+hi^-lo" or "+val" or "val".
;;;  The ^ separates upper and lower parts.
;;;
;;;  Returns (hi-str lo-str) or nil.
;;;  e.g.  "~+.010^-.000~"  ->  ("+.010" "-.000")
;;;        "~+.25^-0.00~"   ->  ("+.25"  "-0.00")
;;;        "~.005~"         ->  (".005"  "-.005")  symmetric
;;; ============================================================================

(defun DQC:parse-tol-block (content / caret-pos hi lo)
  (setq caret-pos (DQC:find-char content "^" 1))
  (if (> caret-pos 0)
    (list (DQC:trim (substr content 1 (1- caret-pos)))
          (DQC:trim (substr content (1+ caret-pos))))
    ;; single value = symmetric: show as +val/-val
    (progn
      (setq hi (DQC:trim content))
      (if (= (strlen hi) 0) nil
        (if (= (substr hi 1 1) "+")
          (list hi (strcat "-" (substr hi 2)))
          (list (strcat "+" hi) (strcat "-" hi)))))))

;;; Extract first tolerance block from a stripped string segment.
;;; Returns (hi-str lo-str) or nil.
;;;
;;; Strategy (in order):
;;;   1. ~...~ stacked block  (from \S format codes)  already the main path
;;;   2. %%p<num>             symmetric plus/minus
;;;   3. +<num>/<sep>-<num>  explicit asymmetric plain text
(defun DQC:extract-tol (s / t1 t2 content result)
  (setq t1 (DQC:find-char s "~" 1))
  (if (= t1 0)
    ;; No stacked block - fall through to plain-text patterns
    (DQC:extract-plain-tol s)
    (progn
      (setq t2 (DQC:find-char s "~" (1+ t1)))
      (if (= t2 0)
        (DQC:extract-plain-tol s)
        (progn
          (setq content (substr s (1+ t1) (- t2 t1 1)))
          (setq result (DQC:parse-tol-block content))
          (if result result
            ;; Stacked block was malformed - try plain text
            (DQC:extract-plain-tol s)))))))

;;; Scan s for plain-text tolerance patterns.
;;; Handles:
;;;   %%p<num>           ->  ("+<num>" "-<num>")   symmetric (plus/minus glyph)
;;;   +<num>/<sep>-<num> ->  ("+<num>" "-<num>")   asymmetric (after nominal)
;;; Returns (hi-str lo-str) or nil.
(defun DQC:extract-plain-tol (s / su i j c hi lo num-s found)
  (setq su (strcase s) i 1 found nil)

  ;; --- 1. %%P (plus/minus) symmetric ---
  (setq j 1)
  (while (and (<= j (- (strlen su) 2)) (null found))
    (if (and (= (substr su j 1) "%")
             (= (substr su (+ j 1) 1) "%")
             (= (substr su (+ j 2) 1) "P"))
      (progn
        (setq i (+ j 3) num-s "")
        ;; skip optional space
        (while (and (<= i (strlen s)) (= (substr s i 1) " "))
          (setq i (1+ i)))
        ;; read optional leading sign
        (if (and (<= i (strlen s)) (wcmatch (substr s i 1) "-,+"))
          (setq num-s (strcat num-s (substr s i 1)) i (1+ i)))
        ;; read digits
        (while (and (<= i (strlen s))
                    (or (wcmatch (substr s i 1) "#") (= (substr s i 1) ".")))
          (setq num-s (strcat num-s (substr s i 1)) i (1+ i)))
        (if (> (strlen num-s) 0)
          (progn
            ;; strip any leading minus for a clean absolute value
            (setq num-s
              (if (= (substr num-s 1 1) "-") (substr num-s 2) num-s))
            (setq found (list (strcat "+" num-s) (strcat "-" num-s))))
          (setq j (1+ j))))
      (setq j (1+ j))))

  ;; --- 2. +<hi>/<sep>-<lo> explicit asymmetric ---
  (if (null found)
    (progn
      (setq i 1)
      (while (and (<= i (strlen s)) (null found))
        (setq c (substr s i 1))
        (cond
          ((= c "+")
           ;; read hi value
           (setq j (1+ i) hi "+")
           (while (and (<= j (strlen s))
                       (or (wcmatch (substr s j 1) "#") (= (substr s j 1) ".")))
             (setq hi (strcat hi (substr s j 1)) j (1+ j)))
           (if (> (strlen hi) 1)   ; at least one digit after +
             (progn
               ;; skip separator chars: / ^ space | line-break
               (while (and (<= j (strlen s))
                           (or (= (substr s j 1) "/")
                               (= (substr s j 1) "^")
                               (= (substr s j 1) " ")
                               (= (substr s j 1) "|")))
                 (setq j (1+ j)))
               ;; must be followed by -lo to qualify
               (if (and (<= j (strlen s)) (= (substr s j 1) "-"))
                 (progn
                   (setq lo "-" j (1+ j))
                   (while (and (<= j (strlen s))
                               (or (wcmatch (substr s j 1) "#") (= (substr s j 1) ".")))
                     (setq lo (strcat lo (substr s j 1)) j (1+ j)))
                   (if (> (strlen lo) 1)
                     (setq found (list hi lo))
                     (setq i (1+ i))))
                 (setq i (1+ i))))
             (setq i (1+ i))))
          (T (setq i (1+ i)))))))
  found)


;;; Format a tolerance pair as a compact string for the balloon label.
;;; ("+.010" "-.000") -> " +.010/-.000"
;;; nil               -> ""
(defun DQC:fmt-tol (tolpair / hi lo)
  (if (null tolpair) ""
    (progn
      (setq hi (car tolpair) lo (cadr tolpair))
      (if (or (null hi) (= (strlen hi) 0)) ""
        (if (or (null lo) (= (strlen lo) 0))
          (strcat " " hi)
          (strcat " " hi "/" lo))))))


;;; ============================================================================
;;;  PART 4 - TEXT PARSER
;;; ============================================================================

;;; Remove all ~...~ tolerance wrappers, leaving only the nominal number.
(defun DQC:drop-tol (s / out i ch in-tol)
  (setq out "" i 1 in-tol nil)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (cond
      ((= ch "~") (setq in-tol (not in-tol) i (1+ i)))
      (in-tol      (setq i (1+ i)))
      (T           (setq out (strcat out ch) i (1+ i)))))
  out)

;;; Strip leading non-numeric prefix (R, %%c, space etc.). Stop at digit/./-.
(defun DQC:strip-pfx (tok / i c)
  (setq tok (DQC:trim tok) i 1)
  (while (and (<= (+ i 1) (strlen tok))
              (= (substr tok i 1) "%")
              (= (substr tok (1+ i) 1) "%"))
    (setq i (+ i 3)))
  (while (and (<= i (strlen tok))
              (setq c (substr tok i 1))
              (not (wcmatch c "#"))
              (/= c "-")
              (/= c "."))
    (setq i (1+ i)))
  (if (> i (strlen tok)) "" (substr tok i)))

;;; Parse nominal number from a token (drops tolerance wrappers, strips prefix).
(defun DQC:first-num (tok / v)
  (setq tok (DQC:drop-tol (DQC:trim tok)))
  (setq tok (DQC:strip-pfx tok))
  (if (= (strlen tok) 0) nil
    (progn
      (setq v (atof tok))
      (if (= v 0.0)
        (if (wcmatch (substr tok 1 1) "#") v nil)
        v))))

;;; Parse metric value - drops tolerance wrappers, strips prefix, then
;;; truncates at any plain-text +/-/space after a digit (un-stacked tols).
(defun DQC:parse-metric (s / v i ch prev)
  (setq s (DQC:drop-tol (DQC:trim s)))
  (setq s (DQC:strip-pfx s))
  (if (= (strlen s) 0) nil
    (progn
      (setq i 1 prev nil)
      (while (<= i (strlen s))
        (setq ch (substr s i 1))
        (cond
          ((or (wcmatch ch "#") (= ch "."))
           (setq prev 'digit i (1+ i)))
          ((and prev (or (= ch "+") (= ch "-") (= ch "/") (= ch " ")))
           (setq s (substr s 1 (1- i)) i (1+ (strlen s))))
          ((and (null prev) (or (= ch "-") (= ch "+")))
           (setq i (1+ i)))
          (T (setq s (substr s 1 (1- i)) i (1+ (strlen s))))))
      (if (= (strlen s) 0) nil
        (progn
          (setq v (atof s))
          (if (and (= v 0.0) (not (wcmatch (substr s 1 1) "#"))) nil v))))))

;;; Main parser.
;;; Returns: (primary alt-real in-tol-pair mm-tol-pair)
;;;   primary     = real (inch nominal)
;;;   alt-real    = real (mm nominal) | symbol EMPTY
;;;   in-tol-pair = (hi-str lo-str) | nil
;;;   mm-tol-pair = (hi-str lo-str) | nil
;;; Returns nil if no [ ] bracket found.
(defun DQC:parse (txt / open close alt-s prim-s p after-s m in-tol mm-tol)
  (setq txt (DQC:trim txt))
  (setq open (DQC:find-char txt "[" 1))
  (if (= open 0) nil
    (progn
      (setq close (DQC:find-char txt "]" (1+ open)))
      (if (= close 0) nil
        (progn
          (setq alt-s  (DQC:trim (substr txt (1+ open) (- close open 1)))
                prim-s (DQC:trim (substr txt 1 (1- open))))
          ;; Extract tolerances from each side BEFORE dropping them
          (setq in-tol (DQC:extract-tol prim-s))
          (setq mm-tol (DQC:extract-tol alt-s))
          ;; Parse nominal numbers
          (setq p (DQC:first-num prim-s))
          (if (null p)
            (progn
              (setq after-s (DQC:trim (substr txt (1+ close))))
              (setq p (DQC:first-num after-s))))
          (if (null p) nil
            (progn
              (setq m (if (> (strlen (DQC:drop-tol alt-s)) 0)
                        (DQC:parse-metric alt-s)
                        nil))
              (list p (if m m 'EMPTY) in-tol mm-tol))))))))


;;; ============================================================================
;;;  PART 5 - DIMENSION GEOMETRY HELPERS
;;; ============================================================================

(defun DQC:dim-textpt (ename / ed pt obj txpt etype)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (if (null ed) nil
    (progn
      (setq etype (strcase (cdr (assoc 0 ed))))
      (setq obj (vlax-ename->vla-object ename))
      (cond
        ;; MTEXT / TEXT: use DXF 10 directly. TextPosition returns (0 0 0)
        ;; on non-DIMENSION entities which sends balloons to drawing origin.
        ((wcmatch etype "MTEXT,TEXT,ATTDEF,ATTRIB")
         (setq pt (cdr (assoc 10 ed))))

        ;; LEADER: try TextLocation VLA, fall back to DXF 10
        ((wcmatch etype "*LEADER*")
         (setq txpt (vl-catch-all-apply 'vlax-get (list obj 'TextLocation)))
         (if (and (not (vl-catch-all-error-p txpt))
                  txpt
                  (not (equal txpt '(0.0 0.0 0.0))))
           (setq pt txpt)
           (setq pt (cdr (assoc 10 ed)))))

        ;; DIMENSION: TextPosition is reliable
        (T
         (setq txpt (vl-catch-all-apply 'vlax-get (list obj 'TextPosition)))
         (if (and (not (vl-catch-all-error-p txpt))
                  txpt
                  (not (equal txpt '(0.0 0.0 0.0))))
           (setq pt txpt))
         (if (null pt) (setq pt (cdr (assoc 11 ed))))
         (if (null pt) (setq pt (cdr (assoc 10 ed))))))
      pt)))

(defun DQC:dim-txth (ename / ed h obj)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (setq h (if ed (cdr (assoc 140 ed)) nil))
  (if (or (null h) (< h 0.001))
    (progn
      (setq obj (vlax-ename->vla-object ename))
      (setq h (vl-catch-all-apply 'vlax-get (list obj 'TextHeight)))
      (if (vl-catch-all-error-p h) (setq h nil))))
  (if (or (null h) (< h 0.001)) (setq h (getvar "DIMTXT")))
  (if (or (null h) (< h 0.001)) (setq h 2.5))
  h)

(defun DQC:dim-meas (ename / ed)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (if ed (cdr (assoc 42 ed)) nil))

(defun DQC:dim-style (ename / ed)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (if ed (cdr (assoc 3 ed)) nil))

(defun DQC:lfac (sname doc / styles sobj lf)
  (if (null sname) 1.0
    (progn
      (setq styles (vl-catch-all-apply 'vla-get-DimStyles (list doc)))
      (if (vl-catch-all-error-p styles) 1.0
        (progn
          (setq sobj (vl-catch-all-apply 'vla-item (list styles sname)))
          (if (vl-catch-all-error-p sobj) 1.0
            (progn
              (setq lf (vl-catch-all-apply
                         'vla-get-LinearScaleFactor (list sobj)))
              (if (or (vl-catch-all-error-p lf) (null lf) (zerop lf))
                1.0 (abs lf)))))))))

(defun DQC:has-meas-token (s / pos-meas)
  (setq pos-meas (vl-string-search "<>" s))
  (if pos-meas T nil))


;;; ============================================================================
;;;  PART 6 - BALLOON PLACEMENT
;;; ============================================================================

(defun DQC:rot2 (dx dy ang / ca sa)
  (setq ca (cos ang) sa (sin ang))
  (list (- (* dx ca) (* dy sa))
        (+ (* dx sa) (* dy ca))))

(defun DQC:place-balloon (txtpt txth dimang label layer / bh offx offy ovec ins bw ent-data)
  (setq bh (if (and DQC:TXT-H (> DQC:TXT-H 0))
             DQC:TXT-H
             (* txth 0.85)))
  (if (< bh 0.5) (setq bh 0.5))
  (setq offx (* (if (and DQC:OFFSET (> DQC:OFFSET 0))
                  DQC:OFFSET (* bh 2.5)) 1.0)
        offy (* bh 1.2))
  (setq ovec (DQC:rot2 offx offy (if dimang dimang 0.0)))
  (setq ins (list (+ (car  txtpt) (car  ovec))
                  (+ (cadr txtpt) (cadr ovec))
                  (if (caddr txtpt) (caddr txtpt) 0.0)))
  (setq bw (* (strlen label) bh 0.7))
  (if (< bw (* bh 3)) (setq bw (* bh 3)))
  (setq ent-data
    (list (cons 0   "MTEXT")
          (cons 100 "AcDbEntity")
          (cons 8   layer)
          (cons 100 "AcDbMText")
          (cons 10  ins)
          (cons 40  bh)
          (cons 41  bw)
          (cons 71  7)
          (cons 72  1)
          (cons 1   label)))
  (if (vl-catch-all-error-p (vl-catch-all-apply 'entmake (list ent-data)))
    nil (entlast)))


;;; ============================================================================
;;;  PART 7 - PROCESS ONE ENTITY
;;; ============================================================================

(defun DQC:process (ename doc / obj ed etype entlay
                              meas sname lfac primary-auto is-dim
                              ts to g1 flags70 alt-on dimaltf
                              pair stripped raw from-text from-meas-sub
                              pfx in-dp mm-dp style-dp
                              bracket-pos inch-seg
                              primary alt expected ok label layer
                              in-tol mm-tol tol-str
                              txtpt txth dimang)

  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (vl-catch-all-error-p obj)
    (list 'SKIP "" nil nil nil "")

    (progn
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))

      ;; Skip our own balloon layers
      (setq entlay (if ed (cdr (assoc 8 ed)) nil))
      (if (and entlay
               (or (= (strcase entlay) (strcase DQC:PASS-LAYER))
                   (= (strcase entlay) (strcase DQC:FAIL-LAYER))))
        (list 'SKIP "" nil nil nil "")

      (progn
        (setq etype (if ed (strcase (cdr (assoc 0 ed))) "?"))
        (setq is-dim
          (wcmatch etype
            "DIMENSION,ROTATED*,LINEAR*,ALIGNED*,ANG*,DIAMETR*,RADIAL*,ORDINATE*"))

        (setq meas (if ed (cdr (assoc 42 ed)) nil))
        (if (null meas) (setq meas 0.0))
        (setq sname (DQC:dim-style ename)
              lfac  (DQC:lfac sname doc))
        (setq primary-auto (* (abs meas) lfac))

        ;; Collect text sources
        (setq ts (vl-catch-all-apply 'vlax-get (list obj 'TextString)))
        (if (or (vl-catch-all-error-p ts) (null ts)) (setq ts ""))
        (setq to (vl-catch-all-apply 'vlax-get (list obj 'TextOverride)))
        (if (or (vl-catch-all-error-p to) (null to)) (setq to ""))
        (setq g1 (if ed (cdr (assoc 1 ed)) nil))
        (if (null g1) (setq g1 ""))

        (setq alt-on (vl-catch-all-apply 'vlax-get (list obj 'AlternateUnits)))
        (if (vl-catch-all-error-p alt-on) (setq alt-on nil))
        (if (and (null alt-on) ed)
          (progn
            (setq flags70 (cdr (assoc 70 ed)))
            (if (and flags70 (= (logand flags70 2) 2))
              (setq alt-on :vlax-true))))
        (setq dimaltf (if ed (cdr (assoc 143 ed)) nil))
        (if (or (null dimaltf) (zerop dimaltf)) (setq dimaltf DQC:MM/IN))

        ;; ---- Parse attempts ---------------------------------------------
        (setq pair nil from-text nil from-meas-sub nil stripped "" raw "")

        ;; Attempt 1: TextString (strip format codes first)
        (if (and (null pair) (> (strlen (DQC:trim ts)) 0))
          (progn
            (setq stripped (DQC:strip ts primary-auto (* primary-auto DQC:MM/IN)))
            (setq raw stripped)
            (setq pair (DQC:parse stripped))
            (if pair (setq from-text T from-meas-sub nil))))

        ;; Attempt 2: TextOverride
        (if (and (null pair) (> (strlen (DQC:trim to)) 0))
          (progn
            (setq stripped (DQC:strip to primary-auto (* primary-auto DQC:MM/IN)))
            (setq raw stripped)
            (setq pair (DQC:parse stripped))
            (if pair (setq from-text T from-meas-sub (DQC:has-meas-token to)))))

        ;; Attempt 3: DXF group 1
        (if (and (null pair) (= (type g1) 'STR) (> (strlen (DQC:trim g1)) 0))
          (progn
            (setq stripped (DQC:strip g1 primary-auto (* primary-auto DQC:MM/IN)))
            (setq raw stripped)
            (setq pair (DQC:parse stripped))
            (if pair (setq from-text T from-meas-sub (DQC:has-meas-token g1)))))

        ;; Attempts 4 & 5: synthesis - ONLY for native DIMENSION entities
        (if is-dim
          (progn
            (if (and (null pair) (or (eq alt-on :vlax-true) (= alt-on -1)))
              (progn
                (setq stripped (strcat (rtos primary-auto 2 6)
                                       " [" (rtos (* primary-auto dimaltf) 2 6) "]"))
                (setq raw stripped)
                (setq pair (DQC:parse stripped))
                (if pair (setq from-meas-sub T))))
            (if (and (null pair) (> primary-auto 0.0001))
              (progn
                (setq stripped (strcat (rtos primary-auto 2 6)
                                       " [" (rtos (* primary-auto DQC:MM/IN) 2 6) "]"))
                (setq raw stripped)
                (setq pair (DQC:parse stripped))
                (if pair (setq from-meas-sub T))))))

        ;; ---- Prefix detection ------------------------------------------
        (setq pfx (DQC:dim-prefix stripped))
        (if (= pfx "") (setq pfx (DQC:dim-prefix ts)))
        (if (= pfx "") (setq pfx (DQC:dim-prefix to)))
        (if (= pfx "") (setq pfx (DQC:dim-prefix g1)))

        ;; ---- FIX: Decimal precision ------------------------------------
        ;; When text is literally typed (not a <> substitution), count dp
        ;; from the RAW inch-side token string so no digits are lost.
        ;; Use bracket-pos (separate variable!) to find "[" - do NOT reuse
        ;; in-dp as a scratch variable before calling count-dp-in-token.
        (setq style-dp (DQC:dimdec sname doc))
        ;; DQC:dimdec now always returns an integer (falls back to DIMDEC sysvar)
        ;; but guard defensively to avoid using a nil/0 value
        (if (or (null style-dp) (< style-dp 0))
          (setq style-dp (fix (getvar "DIMDEC"))))

        (if (and from-text (not from-meas-sub))
          (progn
            (setq bracket-pos (DQC:find-char stripped "[" 1))
            (setq inch-seg
              (if (= bracket-pos 0) stripped
                (DQC:trim (substr stripped 1 (1- bracket-pos)))))
            ;; Count dp from the raw string - preserves ALL digits as written
            (setq in-dp (DQC:count-dp-in-token inch-seg)))
          ;; <> substitution or synthesis: use dim style precision
          (setq in-dp style-dp))

        ;; mm side: one fewer dp than inch, minimum 1
        (setq mm-dp (if (= in-dp 0) 0 (max 1 (1- in-dp))))

        ;; ---- Tolerance strings for balloon label -----------------------
        (setq in-tol (if pair (nth 2 pair) nil))
        (setq mm-tol (if pair (nth 3 pair) nil))
        ;; Format: " +.010/-.000 | +0.25/-0.00"  (inch | mm)
        (setq tol-str
          (if (or in-tol mm-tol)
            (strcat " [" (DQC:fmt-tol in-tol) " | " (DQC:fmt-tol mm-tol) "]")
            ""))

        ;; ---- Geometry --------------------------------------------------
        (setq txtpt (DQC:dim-textpt ename))
        (setq txth  (DQC:dim-txth  ename))
        (setq dimang (vl-catch-all-apply 'vlax-get (list obj 'TextRotation)))
        (if (vl-catch-all-error-p dimang)
          (setq dimang (if ed (cdr (assoc 53 ed)) 0.0)))
        (if (null dimang) (setq dimang 0.0))

        ;; ---- Classify and place balloon --------------------------------
        (cond
          ((null pair)
           (list 'SKIP "" nil nil nil raw))

          ((eq (cadr pair) 'EMPTY)
           (setq primary  (car pair)
                 expected (* (abs primary) DQC:MM/IN)
                 label    (strcat pfx (DQC:fmt primary in-dp)
                                  "\" [?] XX (exp " (DQC:fmt expected mm-dp) ")"
                                  tol-str)
                 layer    DQC:FAIL-LAYER)
           (if txtpt (DQC:place-balloon txtpt txth dimang label layer))
           (list 'FAIL label primary nil expected raw))

          (T
           (setq primary  (car  pair)
                 alt      (cadr pair)
                 expected (* (abs primary) DQC:MM/IN)
                 ok       (DQC:ok? primary alt DQC:MM/IN))
           (if ok
             (setq label (strcat pfx (DQC:fmt primary in-dp)
                                 "\" [" (DQC:fmt alt mm-dp) "] OK" tol-str)
                   layer DQC:PASS-LAYER)
             (setq label (strcat pfx (DQC:fmt primary in-dp)
                                 "\" [" (DQC:fmt alt mm-dp)
                                 "] XX (exp " (DQC:fmt expected mm-dp) ")" tol-str)
                   layer DQC:FAIL-LAYER))
           (if txtpt (DQC:place-balloon txtpt txth dimang label layer))
           (list (if ok 'PASS 'FAIL) label primary alt expected raw))
        )
      ))
    )
  )
)


;;; ============================================================================
;;;  PART 8 - DCL DIALOG
;;; ============================================================================

(defun DQC:write-dcl ( / path f)
  (setq path (strcat (getvar "TEMPPREFIX") "dim_qc_v4.dcl"))
  (setq f (open path "w"))
  (write-line "dqc_settings : dialog {" f)
  (write-line "  label = \"DIM QC v5.0  -  Balloon Edition\";" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Conversion factor\";" f)
  (write-line "    : row {" f)
  (write-line "      : text  { label = \"1 inch  =  ? mm  (normally 25.4) :\"; }" f)
  (write-line "      : edit_box { key = \"factor\"; width = 10; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Match tolerance (nominal value is QC-checked)\";" f)
  (write-line "    : row {" f)
  (write-line "      : text  { label = \"Relative tolerance  (%):        \"; }" f)
  (write-line "      : edit_box { key = \"rel_tol\"; width = 8; }" f)
  (write-line "    }" f)
  (write-line "    : row {" f)
  (write-line "      : text  { label = \"Absolute tolerance (mm):        \"; }" f)
  (write-line "      : edit_box { key = \"abs_tol\"; width = 8; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Balloon size  (0 = auto from dim style)\";" f)
  (write-line "    : row {" f)
  (write-line "      : text  { label = \"Text height  (0 = auto):        \"; }" f)
  (write-line "      : edit_box { key = \"txth\"; width = 8; }" f)
  (write-line "    }" f)
  (write-line "    : row {" f)
  (write-line "      : text  { label = \"Offset from dim text (0 = auto):\"; }" f)
  (write-line "      : edit_box { key = \"offset\"; width = 8; }" f)
  (write-line "    }" f)
  (write-line "  }" f)
  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Balloon key\";" f)
  (write-line "    : text { label = \"  .311\\\" [7.90] OK [+.010/-.000 | +.25/-.00] = PASS\"; }" f)
  (write-line "    : text { label = \"  .311\\\" [7.85] XX (exp 7.90) [tol...]       = FAIL\"; }" f)
  (write-line "    : text { label = \"  .311\\\" [?]   XX (exp 7.90)                 = EMPTY\"; }" f)
  (write-line "    : text { label = \"Tolerances shown informational; QC is on nominal only.\"; }" f)
  (write-line "    : text { label = \"Freeze DIM_QC_PASS/FAIL to hide.  DIMQC-RESET removes.\"; }" f)
  (write-line "  }" f)
  (write-line "  : row {" f)
  (write-line "    : button { key = \"run\";    label = \"Run QC + Place Balloons\"; is_default = true; width = 24; }" f)
  (write-line "    : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; width = 12; }" f)
  (write-line "  }" f)
  (write-line "}" f)
  (write-line "" f)
  (write-line "dqc_results : dialog {" f)
  (write-line "  label = \"DIM QC v5.0  -  Results\";" f)
  (write-line "  : text { key = \"sum_line\"; label = \" \"; }" f)
  (write-line "  : list_box {" f)
  (write-line "    key        = \"res_list\";" f)
  (write-line "    label      = \"All dual-unit dimensions checked:\";" f)
  (write-line "    height     = 22;" f)
  (write-line "    width      = 72;" f)
  (write-line "    multiple_select = false;" f)
  (write-line "  }" f)
  (write-line "  : text { label = \"Balloons placed in drawing.  Plot as normal.\"; }" f)
  (write-line "  : text { label = \"Freeze DIM_QC_PASS / DIM_QC_FAIL to hide.  DIMQC-RESET to remove.\"; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)
  (close f)
  path)


;;; ============================================================================
;;;  PART 9 - MAIN COMMAND  C:DIMQC
;;; ============================================================================

(defun C:DIMQC ( / dcl-path dcl-id action doc ss len i ename res
                   total pass fail skip lines sumstr
                   f-str r-str a-str h-str o-str)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq dcl-path (DQC:write-dcl))
  (setq dcl-id   (load_dialog dcl-path))
  (if (< dcl-id 0) (progn (alert "Cannot load DCL file.") (exit)))
  (if (not (new_dialog "dqc_settings" dcl-id))
    (progn (unload_dialog dcl-id) (alert "Cannot open settings dialog.") (exit)))

  (set_tile "factor"  (rtos DQC:MM/IN 2 4))
  (set_tile "rel_tol" (rtos (* DQC:REL-TOL 100.0) 2 1))
  (set_tile "abs_tol" (rtos DQC:ABS-TOL 2 3))
  (set_tile "txth"    (if (and DQC:TXT-H   (> DQC:TXT-H   0)) (rtos DQC:TXT-H   2 3) "0"))
  (set_tile "offset"  (if (and DQC:OFFSET  (> DQC:OFFSET  0)) (rtos DQC:OFFSET  2 3) "0"))

  (setq action "cancel")
  (action_tile "run"    "(setq action \"run\")    (done_dialog 1)")
  (action_tile "cancel" "(setq action \"cancel\") (done_dialog 0)")
  (start_dialog)

  (setq f-str (get_tile "factor")  r-str (get_tile "rel_tol")
        a-str (get_tile "abs_tol") h-str (get_tile "txth")
        o-str (get_tile "offset"))
  (unload_dialog dcl-id)

  (if (= action "cancel") (progn (princ "\n Cancelled.\n") (princ) (exit)))

  (if (and f-str (> (strlen f-str) 0)) (setq DQC:MM/IN   (atof f-str)))
  (if (and r-str (> (strlen r-str) 0)) (setq DQC:REL-TOL (/ (atof r-str) 100.0)))
  (if (and a-str (> (strlen a-str) 0)) (setq DQC:ABS-TOL (atof a-str)))
  (setq DQC:TXT-H  (if (and h-str (> (atof h-str) 0)) (atof h-str) nil))
  (setq DQC:OFFSET (if (and o-str (> (atof o-str) 0)) (atof o-str) nil))

  (DQC:ensure-layer DQC:PASS-LAYER DQC:PASS-COLOR doc)
  (DQC:ensure-layer DQC:FAIL-LAYER DQC:FAIL-COLOR doc)
  (DQC:erase-balloons doc)

  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss) (progn (alert "No dimension entities found.") (princ) (exit)))

  (setq len (sslength ss) total 0 pass 0 fail 0 skip 0 lines (list))
  (setq i 0)
  (while (< i len)
    (setq ename (ssname ss i)
          res   (DQC:process ename doc)
          total (1+ total))
    (cond
      ((= (car res) 'PASS)
       (setq pass (1+ pass))
       (setq lines (append lines
         (list (strcat "PASS  |  "
                (if (nth 2 res) (strcat (rtos (nth 2 res) 2 4) "\"") "?")
                "  ->  "
                (if (nth 3 res) (rtos (nth 3 res) 2 3) "?") " mm"
                "  (exp " (if (nth 4 res) (rtos (nth 4 res) 2 3) "?") " mm)")))))
      ((= (car res) 'FAIL)
       (setq fail (1+ fail))
       (setq lines (append lines
         (list (strcat "FAIL  |  "
                (if (nth 2 res) (strcat (rtos (nth 2 res) 2 4) "\"") "?")
                "  ->  "
                (if (null (nth 3 res)) "[ ] EMPTY"
                  (strcat (rtos (nth 3 res) 2 3) " mm"))
                "  (exp " (if (nth 4 res) (rtos (nth 4 res) 2 3) "?") " mm)"
                (if (null (nth 3 res)) "  <- mm NOT ENTERED" "  <- MISMATCH"))))))
      (T (setq skip (1+ skip))))
    (setq i (1+ i)))

  (vla-Regen doc acAllViewports)

  (setq sumstr (strcat "Checked: " (itoa (+ pass fail))
                       "   PASS: " (itoa pass)
                       "   FAIL: " (itoa fail)
                       "   Skipped: " (itoa skip)))

  (setq dcl-id (load_dialog dcl-path))
  (if (not (new_dialog "dqc_results" dcl-id))
    (progn (unload_dialog dcl-id) (princ (strcat "\n" sumstr "\n")) (princ) (exit)))

  (set_tile "sum_line" sumstr)
  (start_list "res_list")
  (mapcar 'add_list lines)
  (end_list)
  (start_dialog)
  (unload_dialog dcl-id)

  (princ (strcat "\n " sumstr "\n"))
  (princ))


;;; ============================================================================
;;;  PART 10 - DIMQC-RESET
;;; ============================================================================

(defun DQC:erase-balloons (doc / ss len i)
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (setq ss (ssget "X" (list (cons 0 "MTEXT") (cons 8 lname))))
    (if ss
      (progn (setq len (sslength ss) i 0)
             (while (< i len)
               (vl-catch-all-apply 'entdel (list (ssname ss i)))
               (setq i (1+ i))))))
  (vl-catch-all-apply 'vla-Regen (list doc acAllViewports)))

(defun C:DIMQC-RESET ( / doc n ss len i)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq n 0)
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (setq ss (ssget "X" (list (cons 0 "MTEXT") (cons 8 lname))))
    (if ss
      (progn (setq len (sslength ss) i 0)
             (while (< i len)
               (vl-catch-all-apply 'entdel (list (ssname ss i)))
               (setq n (1+ n) i (1+ i))))))
  (vla-Regen doc acAllViewports)
  (princ (strcat "\n Removed " (itoa n) " balloon(s).\n\n"))
  (princ))


;;; ============================================================================
;;;  PART 11 - DIMQC-DIAG
;;; ============================================================================

(defun C:DIMQC-DIAG ( / doc ss len i ename obj ed etype sname lfac is-dim
                         meas raw stripped pair primary alt expected
                         ts to g alt-on dimaltf flags70)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n========== DIMQC DIAGNOSTIC v5.0 ==========\n")
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss) (progn (princ " No entities found.\n\n") (princ) (exit)))
  (setq len (sslength ss))
  (princ (strcat " " (itoa len) " entities found.\n\n"))
  (setq i 0)
  (while (< i len)
    (setq ename (ssname ss i))
    (setq obj   (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
    (setq ed    (vl-catch-all-apply 'entget (list ename)))
    (if (vl-catch-all-error-p ed) (setq ed nil))
    (setq etype (if ed (strcase (cdr (assoc 0 ed))) "?"))
    (setq is-dim (wcmatch etype
      "DIMENSION,ROTATED*,LINEAR*,ALIGNED*,ANG*,DIAMETR*,RADIAL*,ORDINATE*"))
    (setq sname (DQC:dim-style ename)
          lfac  (DQC:lfac sname doc)
          meas  (DQC:dim-meas ename))
    (if (null meas) (setq meas 0.0))

    (setq ts (vl-catch-all-apply 'vlax-get (list obj 'TextString)))
    (if (or (vl-catch-all-error-p ts) (null ts)) (setq ts ""))
    (setq to (vl-catch-all-apply 'vlax-get (list obj 'TextOverride)))
    (if (or (vl-catch-all-error-p to) (null to)) (setq to ""))
    (setq g (if ed (cdr (assoc 1 ed)) nil))
    (if (null g) (setq g ""))

    (setq alt-on (vl-catch-all-apply 'vlax-get (list obj 'AlternateUnits)))
    (if (vl-catch-all-error-p alt-on) (setq alt-on nil))
    (if (and (null alt-on) ed)
      (progn (setq flags70 (cdr (assoc 70 ed)))
             (if (and flags70 (= (logand flags70 2) 2)) (setq alt-on :vlax-true))))
    (setq dimaltf (if ed (cdr (assoc 143 ed)) nil))
    (if (or (null dimaltf) (zerop dimaltf)) (setq dimaltf DQC:MM/IN))

    (setq pair nil raw "" stripped "")

    (if (and (null pair) (> (strlen (DQC:trim ts)) 0))
      (progn
        (setq stripped (DQC:strip ts (* (abs meas) lfac) (* (* (abs meas) lfac) DQC:MM/IN)))
        (setq raw (strcat "TextString(stripped): " stripped))
        (setq pair (DQC:parse stripped))))

    (if (and (null pair) (> (strlen (DQC:trim to)) 0))
      (progn
        (setq stripped (DQC:strip to (* (abs meas) lfac) (* (* (abs meas) lfac) DQC:MM/IN)))
        (setq raw (strcat "TextOverride: " stripped))
        (setq pair (DQC:parse stripped))))

    (if (and (null pair) (= (type g) 'STR) (> (strlen (DQC:trim g)) 0))
      (progn
        (setq stripped (DQC:strip g (* (abs meas) lfac) (* (* (abs meas) lfac) DQC:MM/IN)))
        (setq raw (strcat "DXF1: " stripped))
        (setq pair (DQC:parse stripped))))

    (if is-dim
      (progn
        (if (and (null pair) (or (eq alt-on :vlax-true) (= alt-on -1)))
          (progn
            (setq stripped (strcat (rtos (* (abs meas) lfac) 2 6)
                                   " [" (rtos (* (* (abs meas) lfac) dimaltf) 2 6) "]"))
            (setq raw (strcat "DIMALT synth: " stripped))
            (setq pair (DQC:parse stripped))))
        (if (and (null pair) (> (* (abs meas) lfac) 0.0001))
          (progn
            (setq stripped (strcat (rtos (* (abs meas) lfac) 2 6)
                                   " [" (rtos (* (* (abs meas) lfac) DQC:MM/IN) 2 6) "]"))
            (setq raw (strcat "Meas synth: " stripped))
            (setq pair (DQC:parse stripped))))))

    (princ (strcat "ITEM #" (itoa (1+ i)) " [" etype "]\n"))
    (princ (strcat "  IsDim   : " (if is-dim "YES" "NO") "\n"))
    (princ (strcat "  Style   : " (if sname sname "?")
                   "  DIMDEC: " (vl-princ-to-string (DQC:dimdec sname doc)) "\n"))
    (princ (strcat "  TextStr : \"" ts "\"\n"))
    (princ (strcat "  TextOvr : \"" to "\"\n"))
    (princ (strcat "  DXF1    : \"" (if (= (type g) 'STR) g "") "\"\n"))
    (princ (strcat "  Raw     : \"" raw "\"\n"))
    (princ (strcat "  Meas    : " (rtos meas 2 6) "\n"))
    (princ
      (strcat "  Parse   : "
        (cond
          ((null pair) "no [ ] bracket  ->  SKIP")
          ((eq (cadr pair) 'EMPTY)
           (strcat "primary=" (rtos (car pair) 2 6)
                   "\"  mm=EMPTY"
                   "  in-tol=" (vl-princ-to-string (nth 2 pair))
                   "  exp=" (rtos (* (abs (car pair)) DQC:MM/IN) 2 4) " mm"))
          (T
           (setq primary (car pair) alt (cadr pair)
                 expected (* (abs primary) DQC:MM/IN))
           (strcat "primary=" (rtos primary 2 6) "\""
                   "  mm=" (rtos alt 2 4)
                   "  exp=" (rtos expected 2 4)
                   "  diff=" (rtos (abs (- expected alt)) 2 5)
                   "  in-tol=" (vl-princ-to-string (nth 2 pair))
                   "  mm-tol=" (vl-princ-to-string (nth 3 pair))
                   (if (DQC:ok? primary alt DQC:MM/IN) "  -> PASS" "  -> FAIL"))))
        "\n\n"))
    (setq i (1+ i))
    (if (= (rem i 20) 0)
      (getstring " --- ENTER for next batch --- ")))
  (princ "========== END ==========\n\n")
  (princ))


;;; ============================================================================
;;;  LOAD MESSAGE
;;; ============================================================================
(princ "\n")
(princ "================================================\n")
(princ " DIM QC v5.0  (Balloon Edition)  loaded.\n")
(princ "\n")
(princ "   DIMQC        Open GUI and run QC check\n")
(princ "   DIMQC-RESET  Remove all balloons\n")
(princ "   DIMQC-DIAG   Command-line diagnostic\n")
(princ "\n")
(princ " v5.0 fixes:\n")
(princ "   - Accuracy: AND logic in DQC:ok? - 0.08 mm gate\n")
(princ "     always enforced. 28.229\"[719.02] now FAILS.\n")
(princ "   - Trailing zeros: DQC:dimdec reads live DIMDEC.\n")
(princ "     8.50 stays 8.50, no spurious extra zeros.\n")
(princ "   - Tolerances: plain text +val/-val and %%p\n")
(princ "     now detected in addition to stacked \\S blocks.\n")
(princ "================================================\n\n")
(princ)