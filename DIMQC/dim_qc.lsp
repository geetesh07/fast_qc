;;; ============================================================================
;;;  dim_qc.lsp  -  Engineering Dual-Unit QC
;;;  Version 18.0-CANONICAL  (safe command wrapper; corrected Mode 3 counts; anchored leader marks)
;;;
;;;  COMMANDS
;;;    DIMQC        Mode selection menu
;;;    DIMQC-RESET  Erase all marks AND delete QC layers
;;;    DIMQC-DIAG   Command-line diagnostic for dimensions
;;;
;;;  MODES
;;;    1. Operating Conditions  - user selects text  (HP/kW, IN-LB/N-M)
;;;    2. MED Check             - user selects text  (LB/KG, torque, LB/IN, PSI; no RAD/compound stiffness)
;;;    3. Dimensions Check      - all entities scanned (inch [mm] only)
;;;    4. Notes Check           - user selects notes (LB/KG + inch/mm)
;;;
;;;  NUMBERS WITH COMMAS AS THOUSANDS SEPARATORS ARE HANDLED.
;;;  PRIMARY VALUE AND [ALT VALUE] MAY BE IN SEPARATE ENTITIES.
;;;
;;;  LAYERS (deleted on DIMQC-RESET)
;;;    DIM_QC_PASS  colour 3  (green)
;;;    DIM_QC_FAIL  colour 7  (white, bold)
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
(setq DQC:FAIL-COLOR 7)

;;; v17.2 mark placement controls
;;; Anchor dot + leader arrow + close PASS/FAIL mark identify the exact checked text/dimension.
(setq DQC:MARK-LEADER T)
(setq DQC:MARK-ANCHOR T)
(setq DQC:ANCHOR-RATIO 0.12)
(setq DQC:LEADER-COLOR 8)


;;; ============================================================================
;;;  PART 1 - CORE UTILITIES
;;; ============================================================================

(defun DQC:trim (s)
  (if (or (null s) (/= (type s) 'STR)) "" (vl-string-trim " " s)))

(defun DQC:find-char (s c pos / p)
  (if (setq p (vl-string-search c s (1- pos))) (1+ p) 0))

;;; Remove commas from a numeric token string: "9,317" -> "9317"
(defun DQC:strip-commas (s / out i ch)
  (setq out "" i 1)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (/= ch ",") (setq out (strcat out ch)))
    (setq i (1+ i)))
  out)

;;; Safe atof that handles comma thousands separators: "9,317.5" -> 9317.5
(defun DQC:atof-safe (s)
  (atof (DQC:strip-commas s)))

;;; Legacy tolerance for Mode 3 DIMENSION entities: ±0.5 mm absolute (replaces old 3%/0.08 combo)
;;; Kept as DQC:ok? so call-sites in DQC:process-dim do not need renaming.
(defun DQC:ok? (primary alt factor / ex df)
  (if (< (abs primary) 1e-9)
    (< (abs alt) 0.5)
    (progn
      (setq ex (* (abs primary) factor)
            df (abs (- ex (abs alt))))
      (<= df 0.5))))

;;; Tight tolerance for OP/MED: pass only if converted value rounds correctly (±0.5 unit)
(defun DQC:ok-abs? (primary alt factor tol / ex df)
  (if (< (abs primary) 1e-9)
    (< (abs alt) tol)
    (progn
      (setq ex (* (abs primary) factor)
            df (abs (- ex (abs alt))))
      (<= df tol))))

;;; Return half-a-unit-of-last-place tolerance for a numeric string.
;;; "3.18" -> 2 dp -> 0.005 ; "3.2" -> 1 dp -> 0.05 ; "318" -> 0 dp -> 0.5
;;; Used for range pair checks so that only proper rounding is accepted.
(defun DQC:rounding-tol (s / i dot cnt ch tol)
  (setq i 1 dot nil cnt 0)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (= ch ".") (setq dot T))
    (if (and dot (/= ch ".") (wcmatch ch "#")) (setq cnt (1+ cnt)))
    (setq i (1+ i)))
  ;; tol = 0.5 / 10^cnt
  (setq tol 1.0)
  (repeat cnt (setq tol (/ tol 10.0)))
  (/ tol 2.0))

;;; Accurate X-offset to hit-pos in a note string using AutoLISP textbox.
;;; Falls back to char-count estimate if textbox fails.
(defun DQC:note-x-offset (ename txt hit-pos th / ed wf sty prefix txbox)
  (if (<= hit-pos 1) 0.0
    (progn
      (setq prefix (substr txt 1 (1- hit-pos)))
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))
      (setq wf  (if ed (cdr (assoc 41 ed)) 1.0))
      (if (or (null wf) (zerop wf)) (setq wf 1.0))
      (setq sty (if ed (cdr (assoc 7 ed)) "STANDARD"))
      (if (null sty) (setq sty "STANDARD"))
      (setq txbox
        (vl-catch-all-apply 'textbox
          (list (list (cons 1 prefix) (cons 40 th) (cons 41 wf) (cons 7 sty)))))
      (if (and txbox (not (vl-catch-all-error-p txbox)) (cadr txbox))
        (car (cadr txbox))
        (* (strlen prefix) th 0.55)))))

(defun DQC:fmt (val dp) (rtos val 2 dp))

(defun DQC:ensure-layer (name aci doc / layers lay)
  (setq layers (vla-get-Layers doc))
  (setq lay
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list layers name)))
      (vla-add layers name)
      (vla-item layers name)))
  (vla-put-Color lay aci)
  lay)

(defun DQC:purge-layer (lname doc / ss len i layers lay)
  (setq ss (ssget "X" (list (cons 8 lname))))
  (if ss
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (vl-catch-all-apply 'entdel (list (ssname ss i)))
        (setq i (1+ i)))))
  (setq layers (vla-get-Layers doc))
  (setq lay (vl-catch-all-apply 'vla-item (list layers lname)))
  (if (not (vl-catch-all-error-p lay))
    (vl-catch-all-apply 'vla-delete (list lay))))

;;; Label builders
(defun DQC:pass-label () "{\\fArial|b1|i0;\\U+2713}")
(defun DQC:fail-label (body) (strcat "{\\fArial|b1|i0;" body "}"))

(defun DQC:dim-prefix (s / su)
  (setq s (DQC:trim s) su (strcase s))
  (cond
    ((= (strlen s) 0) "")
    ((and (>= (strlen su) 3) (= (substr su 1 2) "%%")
          (wcmatch (substr su 3 1) "C,D")) "%%c")
    ((= (substr su 1 1) "R") "R")
    (T "")))

(defun DQC:count-dp-in-token (tok / s i c dot cnt prev)
  (setq s (DQC:trim tok) i 1)
  (while (and (<= (+ i 1) (strlen s))
              (= (substr s i 1) "%") (= (substr s (1+ i) 1) "%"))
    (setq i (+ i 3)))
  (while (and (<= i (strlen s))
              (setq c (substr s i 1))
              (not (wcmatch c "#")) (/= c "-") (/= c "."))
    (setq i (1+ i)))
  (setq dot 0 cnt 0 prev nil)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (cond
      ((= c ".") (setq dot i i (1+ i)))
      ((= c ",") (setq i (1+ i)))
      ((wcmatch c "#")
       (if (> dot 0) (setq cnt (1+ cnt)))
       (setq prev T i (1+ i)))
      ((and prev (or (= c "+") (= c "-") (= c "/") (= c " ") (= c "~")))
       (setq i (1+ (strlen s))))
      (T (setq i (1+ i)))))
  cnt)

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
              (setq dec (vl-catch-all-apply 'vlax-get (list sobj 'PrimaryUnitsPrecision)))
              (if (or (vl-catch-all-error-p dec) (null dec)) live (fix dec)))))))))

;;; Get raw text from an entity - returns plain string (empty if none)
(defun DQC:get-text (ename / obj ed ts g1)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (vl-catch-all-error-p obj) ""
    (progn
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))
      (setq ts (vl-catch-all-apply 'vlax-get (list obj 'TextString)))
      (if (or (vl-catch-all-error-p ts) (null ts)) (setq ts ""))
      (setq g1 (if ed (cdr (assoc 1 ed)) nil))
      (if (null g1) (setq g1 ""))
      (if (> (strlen (DQC:trim ts)) 0) ts g1))))

(defun DQC:on-qc-layer? (ename / ed lay)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) nil
    (progn
      (setq lay (cdr (assoc 8 ed)))
      (if (null lay) nil
        (or (= (strcase lay) (strcase DQC:PASS-LAYER))
            (= (strcase lay) (strcase DQC:FAIL-LAYER)))))))


;;; ============================================================================
;;;  PART 2 - MTEXT FORMAT-CODE STRIPPER
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
             ((= nx "P")
              (if (and (< (+ i 2) (strlen s))
                       (wcmatch (substr s (+ i 2) 1) "@,#,,")
                       (> (DQC:find-char s ";" (+ i 2)) 0))
                (progn (setq sc (DQC:find-char s ";" (+ i 2))) (setq i (1+ sc)))
                (setq out (strcat out "\n") i (+ i 2))))
             ((= nx "X") (setq out (strcat out "\n") i (+ i 2)))
             ((= nx "~") (setq out (strcat out " ") i (+ i 2)))
             ((= nx "\\") (setq out (strcat out "\\") i (+ i 2)))
             ((= nx "S")
              (setq sc (DQC:find-char s ";" (+ i 2)))
              (if (= sc 0) (setq i (+ i 2))
                (progn
                  (setq frac (substr s (+ i 2) (- sc i 2)))
                  (setq out (strcat out "~" frac "~"))
                  (setq i (1+ sc)))))
             ((wcmatch nx "H,A,C,T,Q,W,F")
              (setq sc (DQC:find-char s ";" (+ i 2)))
              (if (= sc 0) (setq i (+ i 2)) (setq i (1+ sc))))
             ((= nx "U")
              (if (and (<= (+ i 6) (strlen s)) (= (substr s (+ i 2) 1) "+"))
                (setq out (strcat out " ") i (+ i 7))
                (setq i (+ i 2))))
             ((wcmatch nx "L,O,K") (setq i (+ i 2)))
             (T (setq out (strcat out (substr s (1+ i) 1)) i (+ i 2)))))
         (setq i (1+ i))))
      ((and (= ch "<") (<= (1+ i) (strlen s)) (= (substr s (1+ i) 1) ">"))
       (if (and meas (numberp meas))
         (setq out (strcat out (rtos (abs meas) 2 6)))
         (setq out (strcat out "<>")))
       (setq i (+ i 2)))
      ((and (= ch "[")
            (progn
              (setq sc (1+ i))
              (while (and (<= sc (strlen s)) (= (substr s sc 1) " ")) (setq sc (1+ sc)))
              (and (<= sc (strlen s)) (= (substr s sc 1) "]"))))
       (if (and alt-meas (numberp alt-meas))
         (setq out (strcat out "[" (rtos (abs alt-meas) 2 6) "]"))
         (setq out (strcat out "[]")))
       (setq i (1+ sc)))
      ((or (= ch "{") (= ch "}")) (setq i (1+ i)))
      (T (setq out (strcat out ch) i (1+ i)))))
  out)


;;; ============================================================================
;;;  PART 3 - TOLERANCE PARSER (for dimension text)
;;; ============================================================================

(defun DQC:parse-tol-block (content / cp hi)
  (setq cp (DQC:find-char content "^" 1))
  (if (> cp 0)
    (list (DQC:trim (substr content 1 (1- cp)))
          (DQC:trim (substr content (1+ cp))))
    (progn
      (setq hi (DQC:trim content))
      (if (= (strlen hi) 0) nil
        (if (= (substr hi 1 1) "+")
          (list hi (strcat "-" (substr hi 2)))
          (list (strcat "+" hi) (strcat "-" hi)))))))

(defun DQC:extract-tol (s / t1 t2 content result)
  (setq t1 (DQC:find-char s "~" 1))
  (if (= t1 0) (DQC:extract-plain-tol s)
    (progn
      (setq t2 (DQC:find-char s "~" (1+ t1)))
      (if (= t2 0) (DQC:extract-plain-tol s)
        (progn
          (setq content (substr s (1+ t1) (- t2 t1 1)))
          (setq result (DQC:parse-tol-block content))
          (if result result (DQC:extract-plain-tol s)))))))

(defun DQC:extract-plain-tol (s / su i j c hi lo ns found)
  (setq su (strcase s) i 1 found nil j 1)
  (while (and (<= j (- (strlen su) 2)) (null found))
    (if (and (= (substr su j 1) "%") (= (substr su (+ j 1) 1) "%")
             (= (substr su (+ j 2) 1) "P"))
      (progn
        (setq i (+ j 3) ns "")
        (while (and (<= i (strlen s)) (= (substr s i 1) " ")) (setq i (1+ i)))
        (if (and (<= i (strlen s)) (wcmatch (substr s i 1) "-,+"))
          (setq ns (strcat ns (substr s i 1)) i (1+ i)))
        (while (and (<= i (strlen s))
                    (or (wcmatch (substr s i 1) "#") (= (substr s i 1) ".")))
          (setq ns (strcat ns (substr s i 1)) i (1+ i)))
        (if (> (strlen ns) 0)
          (progn
            (setq ns (if (= (substr ns 1 1) "-") (substr ns 2) ns))
            (setq found (list (strcat "+" ns) (strcat "-" ns))))
          (setq j (1+ j))))
      (setq j (1+ j))))
  (if (null found)
    (progn
      (setq i 1)
      (while (and (<= i (strlen s)) (null found))
        (setq c (substr s i 1))
        (cond
          ((= c "+")
           (setq j (1+ i) hi "+")
           (while (and (<= j (strlen s))
                       (or (wcmatch (substr s j 1) "#") (= (substr s j 1) ".")))
             (setq hi (strcat hi (substr s j 1)) j (1+ j)))
           (if (> (strlen hi) 1)
             (progn
               (while (and (<= j (strlen s))
                           (or (= (substr s j 1) "/") (= (substr s j 1) "^")
                               (= (substr s j 1) " ") (= (substr s j 1) "|")))
                 (setq j (1+ j)))
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

(defun DQC:fmt-tol (tp / hi lo)
  (if (null tp) ""
    (progn
      (setq hi (car tp) lo (cadr tp))
      (if (or (null hi) (= (strlen hi) 0)) ""
        (if (or (null lo) (= (strlen lo) 0))
          (strcat " " hi)
          (strcat " " hi "/" lo))))))


;;; ============================================================================
;;;  PART 4 - DIMENSION TEXT PARSER (for Mode 3)
;;; ============================================================================

(defun DQC:drop-tol (s / out i ch in-t)
  (setq out "" i 1 in-t nil)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (cond
      ((= ch "~") (setq in-t (not in-t) i (1+ i)))
      (in-t (setq i (1+ i)))
      (T (setq out (strcat out ch) i (1+ i)))))
  out)

(defun DQC:strip-pfx (tok / su orig cur it)
  (setq cur (vl-string-trim " \t" tok) it 0)
  (while (< it 5)
    (setq su (strcase cur) orig cur)
    (cond
      ((wcmatch su "R#*")  (setq cur (substr cur 2)))
      ((wcmatch su "R.*")  (setq cur (substr cur 2)))
      ((wcmatch su "R *")  (setq cur (substr cur 2)))
      ((wcmatch su "SR#*") (setq cur (substr cur 3)))
      ((wcmatch su "SR.*") (setq cur (substr cur 3)))
      ((wcmatch su "SR *") (setq cur (substr cur 3)))
      ((wcmatch su "S#*")  (setq cur (substr cur 2)))
      ((wcmatch su "S.*")  (setq cur (substr cur 2)))
      ((wcmatch su "S *")  (setq cur (substr cur 2)))
      ((wcmatch su "M#*")  (setq cur (substr cur 2)))
      ((wcmatch su "M.*")  (setq cur (substr cur 2)))
      ((wcmatch su "M *")  (setq cur (substr cur 2)))
      ((wcmatch su "DIA*") (setq cur (substr cur 4)))
      ((wcmatch su "%%C*") (setq cur (substr cur 4)))
      ((wcmatch su "%%?*") (setq cur (substr cur 4))))
    (setq cur (vl-string-trim " \t" cur))
    (if (= orig cur) (setq it 5) (setq it (1+ it))))
  cur)

(defun DQC:first-num (tok / v)
  (setq tok (DQC:strip-pfx (DQC:drop-tol (DQC:trim tok))))
  (if (= (strlen tok) 0) nil
    (progn
      (setq v (DQC:atof-safe tok))
      (if (= v 0.0) (if (wcmatch (substr tok 1 1) "#") v nil) v))))

(defun DQC:parse-metric (s / v i ch prev)
  (setq s (DQC:strip-pfx (DQC:drop-tol (DQC:trim s))))
  (if (= (strlen s) 0) nil
    (progn
      (setq i 1 prev nil)
      (while (<= i (strlen s))
        (setq ch (substr s i 1))
        (cond
          ((or (wcmatch ch "#") (= ch ".") (= ch ",")) (setq prev 'digit i (1+ i)))
          ((and prev (or (= ch "+") (= ch "-") (= ch "/") (= ch " ")))
           (setq s (substr s 1 (1- i)) i (1+ (strlen s))))
          ((and (null prev) (or (= ch "-") (= ch "+"))) (setq i (1+ i)))
          (T (setq s (substr s 1 (1- i)) i (1+ (strlen s))))))
      (if (= (strlen s) 0) nil
        (progn
          (setq v (DQC:atof-safe s))
          (if (and (= v 0.0) (not (wcmatch (substr s 1 1) "#"))) nil v))))))

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
          (setq in-tol (DQC:extract-tol prim-s))
          (setq mm-tol (DQC:extract-tol alt-s))
          (setq p (DQC:first-num prim-s))
          (if (null p)
            (progn
              (setq after-s (DQC:trim (substr txt (1+ close))))
              (setq p (DQC:first-num after-s))))
          (if (null p) nil
            (progn
              (setq m (if (> (strlen (DQC:drop-tol alt-s)) 0)
                        (DQC:parse-metric alt-s) nil))
              (list p (if m m 'EMPTY) in-tol mm-tol))))))))



;;; ============================================================================
;;;  RANGE PARSER - handles "a/b [c/d]" style dimensions (bore/shaft tol ranges)
;;; ============================================================================

;;; Parse a string like ".0118/.0125" into (lo hi) both positive reals, or nil.
;;; Rejects anything with + or - signs (those are tolerances, not ranges).
(defun DQC:parse-range-pair (s / slash-pos left right lv rv)
  (setq s (DQC:trim (DQC:drop-tol s)))
  (setq s (DQC:strip-pfx s))
  (if (or (= (strlen s) 0)
          (wcmatch s "*+*")
          (= (substr s 1 1) "-"))
    nil
    (progn
      (setq slash-pos (DQC:find-char s "/" 1))
      (if (= slash-pos 0) nil
        (progn
          (setq left  (DQC:trim (substr s 1 (1- slash-pos)))
                right (DQC:trim (substr s (1+ slash-pos))))
          ;; Left must be purely numeric, right must be purely numeric
          (if (or (= (strlen left) 0) (= (strlen right) 0)) nil
            (progn
              (setq lv (DQC:atof-safe left))
              (setq rv (DQC:atof-safe right))
              ;; Accept only positive ascending ranges. Reversed ranges are
              ;; treated as invalid so the drawing gets reviewed instead of
              ;; silently normalizing a bad callout.
              (if (and (> lv 0.0) (> rv 0.0) (<= lv rv))
                (list lv rv)
                nil))))))))

;;; Check if the given dimension text is a range-style "a/b [c/d]" pattern.
;;; Returns (lo hi mm-lo mm-hi) or nil.
(defun DQC:parse-range-dim (txt / open close prim-s alt-s pr mr)
  (setq txt (DQC:trim txt))
  (setq open  (DQC:find-char txt "[" 1))
  (if (= open 0) nil
    (progn
      (setq close (DQC:find-char txt "]" (1+ open)))
      (if (= close 0) nil
        (progn
          (setq prim-s (DQC:trim (substr txt 1 (1- open))))
          (setq alt-s  (DQC:trim (substr txt (1+ open) (- close open 1))))
          (setq pr (DQC:parse-range-pair prim-s))
          (setq mr (DQC:parse-range-pair alt-s))
          (if (and pr mr)
            (list (car pr) (cadr pr) (car mr) (cadr mr))
            nil))))))

;;; ============================================================================
;;;  PART 5 - INLINE N [M] SCANNER (for Mode 3 text entities)
;;; ============================================================================

;;; Returns hits: (ns1 as pos) or (ns1 as pos RANGE ns2)
(defun DQC:scan-inline-dims (txt / results i len ch ns in-n dot j k as closed ns2 dot2)
  (setq results nil i 1 len (strlen txt))
  (while (<= i len)
    (setq ch (substr txt i 1))
    (if (or (wcmatch ch "#")
            (and (= ch ".") (<= (1+ i) len) (wcmatch (substr txt (1+ i) 1) "#")))
      (progn
        ;; scan first number
        (setq ns "" j i dot nil in-n T)
        (while (and (<= j len) in-n)
          (setq ch (substr txt j 1))
          (cond
            ((wcmatch ch "#") (setq ns (strcat ns ch) j (1+ j)))
            ((and (= ch ".") (not dot)) (setq ns (strcat ns ch) dot T j (1+ j)))
            ((= ch ",") (setq ns (strcat ns ch) j (1+ j)))
            (T (setq in-n nil))))
        (setq k j)
        (cond
          ;; Case A: immediate slash - try N1/N2 [M1/M2]
          ((and (<= k len) (= (substr txt k 1) "/"))
           (setq k (1+ k))
           (while (and (<= k len) (= (substr txt k 1) " ")) (setq k (1+ k)))
           (setq ns2 "" dot2 nil in-n T)
           (while (and (<= k len) in-n)
             (setq ch (substr txt k 1))
             (cond
               ((wcmatch ch "#") (setq ns2 (strcat ns2 ch) k (1+ k)))
               ((and (= ch ".") (not dot2)) (setq ns2 (strcat ns2 ch) dot2 T k (1+ k)))
               ((= ch ",") (setq ns2 (strcat ns2 ch) k (1+ k)))
               (T (setq in-n nil))))
           (if (> (strlen ns2) 0)
             (progn
               (while (and (<= k len) (= (substr txt k 1) " ")) (setq k (1+ k)))
               (if (and (<= k len) (= (substr txt k 1) "["))
                 (progn
                   (setq k (1+ k) as "" closed nil)
                   (while (and (<= k len) (not closed))
                     (setq ch (substr txt k 1))
                     (if (= ch "]") (setq closed T)
                       (setq as (strcat as ch) k (1+ k))))
                   (if closed
                     (progn
                       (setq results (append results (list (list ns as i 'RANGE ns2))))
                       (setq i k))
                     (setq i j)))
                 (setq i j)))
             (setq i j)))
          ;; Case B: optional spaces then [ - simple N [M]
          (T
           (while (and (<= k len) (= (substr txt k 1) " ")) (setq k (1+ k)))
           (if (and (<= k len) (= (substr txt k 1) "["))
             (progn
               (setq k (1+ k) as "" closed nil)
               (while (and (<= k len) (not closed))
                 (setq ch (substr txt k 1))
                 (if (= ch "]") (setq closed T)
                   (setq as (strcat as ch) k (1+ k))))
               (if closed
                 (progn
                   (setq results (append results (list (list ns as i))))
                   (setq i k))
                 (setq i j)))
             (setq i j))))
        )
      (setq i (1+ i))))
  results)


;;; ============================================================================
;;;  PART 6 - GEOMETRY HELPERS
;;; ============================================================================

(defun DQC:dim-textpt (ename / ed pt obj txpt etype)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (if (null ed) nil
    (progn
      (setq etype (strcase (cdr (assoc 0 ed))))
      (setq obj (vlax-ename->vla-object ename))
      (cond
        ((wcmatch etype "MTEXT,TEXT,ATTDEF,ATTRIB")
         (setq pt (cdr (assoc 10 ed))))
        ((wcmatch etype "*LEADER*")
         (setq txpt (vl-catch-all-apply 'vlax-get (list obj 'TextLocation)))
         (if (and (not (vl-catch-all-error-p txpt)) txpt
                  (not (equal txpt '(0.0 0.0 0.0))))
           (setq pt txpt)
           (setq pt (cdr (assoc 10 ed)))))
        (T
         (setq txpt (vl-catch-all-apply 'vlax-get (list obj 'TextPosition)))
         (if (and (not (vl-catch-all-error-p txpt)) txpt
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
              (setq lf (vl-catch-all-apply 'vla-get-LinearScaleFactor (list sobj)))
              (if (or (vl-catch-all-error-p lf) (null lf) (zerop lf)) 1.0 (abs lf)))))))))

(defun DQC:has-meas-token (s)
  (if (vl-string-search "<>" s) T nil))

;;; Distance between two 2D/3D points (ignore Z)
(defun DQC:dist2d (p1 p2 / dx dy)
  (setq dx (- (car p1) (car p2))
        dy (- (cadr p1) (cadr p2)))
  (sqrt (+ (* dx dx) (* dy dy))))

;;; Text/MText rotation helper for accurate inline note hit anchors.
(defun DQC:text-rotation (ename / ed obj r)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (setq r (if ed (cdr (assoc 50 ed)) nil))
  (if (null r)
    (progn
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
      (if (not (vl-catch-all-error-p obj))
        (progn
          (setq r (vl-catch-all-apply 'vlax-get (list obj 'Rotation)))
          (if (vl-catch-all-error-p r)
            (progn
              (setq r (vl-catch-all-apply 'vlax-get (list obj 'TextRotation)))
              (if (vl-catch-all-error-p r) (setq r nil))))))))
  (if r r 0.0))



;;; ============================================================================
;;;  PART 7 - MARK PLACEMENT
;;; ============================================================================

(defun DQC:rot2 (dx dy ang / ca sa)
  (setq ca (cos ang) sa (sin ang))
  (list (- (* dx ca) (* dy sa)) (+ (* dx sa) (* dy ca))))

(defun DQC:place-balloon (txtpt txth dimang label layer / bh bw offx offy ovec ins ed color anchor-rad)
  ;; v17.2: every QC mark is anchored to the exact checked point.
  ;;        A small anchor dot is placed at txtpt, a short leader arrow/line
  ;;        connects to the tick/cross mark, and the mark stays close to the dot.
  (if (null txtpt) nil
    (progn
      (setq bh (if (and DQC:TXT-H (> DQC:TXT-H 0)) DQC:TXT-H (* txth 0.85)))
      (if (< bh 0.5) (setq bh 0.5))

      ;; Keep the bottom-center of the QC mark close to the anchor circle.
      ;; DQC:OFFSET can still override horizontal spacing if needed.
      (setq offx (if (and DQC:OFFSET (> DQC:OFFSET 0)) DQC:OFFSET (* bh 0.45))
            offy (* bh 0.40))
      (setq ovec (DQC:rot2 offx offy (if dimang dimang 0.0)))
      (setq ins (list (+ (car txtpt) (car ovec))
                      (+ (cadr txtpt) (cadr ovec))
                      (if (caddr txtpt) (caddr txtpt) 0.0)))
      (setq color (if (= layer DQC:FAIL-LAYER) DQC:FAIL-COLOR DQC:PASS-COLOR))

      ;; Anchor dot exactly at checked point.
      (if DQC:MARK-ANCHOR
        (progn
          (setq anchor-rad (* bh DQC:ANCHOR-RATIO))
          (if (< anchor-rad 0.08) (setq anchor-rad 0.08))
          (entmake
            (list (cons 0 "CIRCLE")
                  (cons 100 "AcDbEntity")
                  (cons 8 layer)
                  (cons 62 color)
                  (cons 100 "AcDbCircle")
                  (cons 10 txtpt)
                  (cons 40 anchor-rad)))))

      ;; Leader arrow/line from checked point to the tick/cross mark.
      ;; Kept short because the mark itself is now close to the anchor circle.
      (if DQC:MARK-LEADER
        (entmake
          (list (cons 0 "LINE")
                (cons 100 "AcDbEntity")
                (cons 8 layer)
                (cons 62 color)
                (cons 10 txtpt)
                (cons 11 ins))))

      ;; QC label. Attachment 8 = bottom-center, so the bottom of the tick/cross
      ;; stays close to the anchor circle and leader endpoint.
      (setq bw (* (strlen label) bh 0.8))
      (if (< bw (* bh 3.0)) (setq bw (* bh 3.0)))
      (setq ed
        (list (cons 0   "MTEXT")
              (cons 100 "AcDbEntity")
              (cons 8   layer)
              (cons 62  color)
              (cons 100 "AcDbMText")
              (cons 10  ins)
              (cons 40  bh)
              (cons 41  bw)
              (cons 71  8)
              (cons 72  1)
              (cons 1   label)))
      (if (vl-catch-all-error-p (vl-catch-all-apply 'entmake (list ed)))
        nil (entlast)))))


;;; ============================================================================
;;;  PART 8 - KEYWORD / NUMBER SCANNING PRIMITIVES
;;; ============================================================================

;;; Find keyword kw in uppercase string su from 1-based pos 'from'.
;;; Returns 1-based start or 0.
(defun DQC:kw-find (su kw from / p)
  (setq p (vl-string-search kw su (1- from)))
  (if p (1+ p) 0))

;;; Read number backward from kw-pos (1-based) in string s.
;;; Supports commas, digits, decimal, AND scientific notation.
(defun DQC:num-before-kw (s kw-pos / i ns dot sci-val segment)
  ;; First check for scientific notation before the keyword
  (setq segment (substr s 1 (1- kw-pos)))
  (setq sci-val (DQC:extract-scientific segment))
  (if sci-val
    (list sci-val)
    (progn
      ;; Original number reading logic
      (setq i (1- kw-pos))
      (while (and (> i 0) (= (substr s i 1) " ")) (setq i (1- i)))
      (if (<= i 0) nil
        (progn
          (setq ns "" dot nil)
          (while (and (> i 0)
                      (or (wcmatch (substr s i 1) "#")
                          (and (= (substr s i 1) ".") (not dot))
                          (= (substr s i 1) ",")))
            (if (= (substr s i 1) ".") (setq dot T))
            (setq ns (strcat (substr s i 1) ns) i (1- i)))
          (if (= (strlen ns) 0) nil (list (DQC:atof-safe ns))))))))

;;; Read number forward from 1-based pos in string s.
;;; Returns real or nil.
(defun DQC:num-after-pos (s pos / i ns dot)
  (setq i pos)
  (while (and (<= i (strlen s)) (= (substr s i 1) " ")) (setq i (1+ i)))
  (setq ns "" dot nil)
  (while (and (<= i (strlen s))
              (or (wcmatch (substr s i 1) "#")
                  (and (= (substr s i 1) ".") (not dot))
                  (= (substr s i 1) ",")))
    (if (= (substr s i 1) ".") (setq dot T))
    (setq ns (strcat ns (substr s i 1)) i (1+ i)))
  (if (= (strlen ns) 0) nil (DQC:atof-safe ns)))

;;; Extract the COEFFICIENT from scientific notation like "8.6 X 10^6 <removed-compound-unit>"
;;; Returns ONLY the base coefficient (e.g. 8.6), ignoring the x10^N exponent.
;;; Both the primary and converted values share the same x10^N scale factor, so
;;; comparing coefficients directly is correct (e.g. 8.6 x10^6 in-lb/rad -> 0.94 x10^6 N-m/rad:
;;; we check 8.6 * 0.112985 ≈ 0.97 vs 0.94, both understood as x10^6).
;;; Returns nil if no "X 10^" or "X10^" pattern is detected.
(defun DQC:extract-scientific (s / su x-pos caret-pos base-str base)
  (setq su (strcase s))
  ;; Accept "X10^" or "X 10^" (the X may or may not have a space before 1)
  (setq x-pos (vl-string-search "X 1" su 0))
  (if (null x-pos)
    (setq x-pos (vl-string-search "X1" su 0)))
  (if (null x-pos) nil
    (progn
      ;; Confirm there is a caret after, proving this is 10^N notation
      (setq caret-pos (vl-string-search "^" su x-pos))
      (if (null caret-pos) nil
        (progn
          ;; Everything before the X  ->  pull out the trailing coefficient
          (setq base-str (substr s 1 x-pos))
          (setq base (DQC:trailing-number-in-seg base-str))
          ;; Return just the coefficient; 10^N is intentionally ignored
          base)))))

;;; Helper function: get trailing number from string
(defun DQC:trailing-number-in-seg (s / i ns dot)
  (setq i (strlen s) ns "" dot nil)
  (while (and (>= i 1) (= (substr s i 1) " "))
    (setq i (1- i)))
  (while (and (>= i 1)
              (or (wcmatch (substr s i 1) "#")
                  (and (= (substr s i 1) ".") (not dot))
                  (= (substr s i 1) ",")))
    (if (= (substr s i 1) ".") (setq dot T))
    (setq ns (strcat (substr s i 1) ns))
    (setq i (1- i)))
  (if (= (strlen ns) 0) nil
    (DQC:atof-safe ns)))

;;; NEW: Detect numeric-only text entities (e.g. "9,317")
;;; Used for Operating Conditions split-text matching
(defun DQC:number-only-text (s / v)
  (setq s (DQC:trim s))
  (if (and (> (strlen s) 0)
           (not (wcmatch (strcase s) "*[A-Z]*")))
    (progn
      (setq v (DQC:atof-safe s))
      (if (> v 0.0) v nil))
    nil))

;;; Check if text contains any primary unit keyword (HP, IN-LB, etc.)
;;; Check if text contains any primary unit keyword (HP, IN-LB, etc.) as a standalone unit.
;;; Uses the same compound-unit boundary rules as DQC:find-prim-hits so split text
;;; does not treat compound stiffness units as LB/IN-LB hits.
(defun DQC:unit-text-has? (txt kw-list / su found kw hits pos left-ok right-ok left-char right-char kw-end)
  (setq su (strcase txt) found nil)

  (foreach kw kw-list
    (if (not found)
      (progn
        (setq hits (DQC:kw-all-positions su kw))
        (foreach pos hits
          (if (not found)
            (progn
              (setq left-ok T)
              (if (> pos 1)
                (progn
                  (setq left-char (substr su (1- pos) 1))
                  (if (or (wcmatch left-char "@,#")
                          (= left-char "-")
                          (= left-char "/")
                          (= left-char "^"))
                    (setq left-ok nil))))

              (setq right-ok T)
              (setq kw-end (+ pos (strlen kw)))
              (if (<= kw-end (strlen su))
                (progn
                  (setq right-char (substr su kw-end 1))
                  (if (or (wcmatch right-char "@,#")
                          (= right-char "-")
                          (= right-char "/")
                          (= right-char "^"))
                    (setq right-ok nil))))

              (if (and left-ok right-ok)
                (setq found T))))))))
  found)
;;; ============================================================================
;;;  PART 9 - CROSS-ENTITY UNIT-PAIR MATCHER
;;;
;;;  Takes a list of ENAME-INFO records (one per selected entity) where each
;;;  record is:  (ename stripped-text insertion-point text-height)
;;;
;;;  Finds every primary-unit occurrence (e.g. "9,317 HP") in any entity,
;;;  then searches ALL entities (same first, then nearest-neighbor) for the
;;;  matching [alt-unit] value.  Places balloon on the entity that contains
;;;  the primary-unit occurrence.
;;;
;;;  Rule tuple:  (kw-list alt-list factor prim-label alt-label dp-p dp-a)
;;; ============================================================================

;;; Return list of (prim-val kw-pos kw-len) found in a stripped string.
;;; Only counts standalone occurrences (not part of longer identifier).
;;; Return list of (prim-val kw-pos kw-len kw) found in a stripped string.
;;; Only counts standalone occurrences, not compound units.
;;; This prevents false hits such as:
;;;   - IN-LB inside compound stiffness units
;;;   - LB inside compound stiffness units or FT-LB
(defun DQC:find-prim-hits (s kw-list / su results kw hits pos v
                                  left-ok right-ok left-char right-char kw-end)
  (setq su (strcase s) results nil)

  (foreach kw kw-list
    (setq hits (DQC:kw-all-positions su kw))

    (foreach pos hits
      (setq left-ok T)
      (if (> pos 1)
        (progn
          (setq left-char (substr su (1- pos) 1))
          (if (or (wcmatch left-char "@,#")
                  (= left-char "-")
                  (= left-char "/")
                  (= left-char "^"))
            (setq left-ok nil))))

      (setq right-ok T)
      (setq kw-end (+ pos (strlen kw)))
      (if (<= kw-end (strlen su))
        (progn
          (setq right-char (substr su kw-end 1))
          (if (or (wcmatch right-char "@,#")
                  (= right-char "-")
                  (= right-char "/")
                  (= right-char "^"))
            (setq right-ok nil))))

      (if (and left-ok right-ok)
        (progn
          (setq v (DQC:num-before-kw s pos))
          (if v
            (setq results
                  (append results
                          (list (list (car v) pos (strlen kw) kw)))))))))
  results)

;;; Return all 1-based positions of keyword kw in uppercase string su.
(defun DQC:kw-all-positions (su kw / positions p start)
  (setq positions nil start 0)
  (while (setq p (vl-string-search kw su start))
    (setq positions (append positions (list (1+ p))))
    (setq start (+ p 1)))
  positions)

;;; Find first alt-unit value in a string, starting search from 'from-pos'.
;;; Looks for [<number>] ... <alt-kw> pattern.
;;; Returns alt-value (real) or nil.
(defun DQC:find-alt-in-string (s alt-list from-pos / su brk-pos close-pos
                                     content after-close window alt-p nm alt-val)
  (setq su (strcase s) alt-val nil)
  (if (> from-pos (strlen s)) nil
    (progn
      ;; start searching for [ from (from-pos - 1) as 0-based offset
      (setq brk-pos (vl-string-search "[" s (max 0 (1- from-pos))))
      (if (null brk-pos) nil
        (progn
          (setq close-pos (vl-string-search "]" s (1+ brk-pos)))
          (if (null close-pos) nil
            (progn
              ;; bracket content (0-based: from brk-pos+1 to close-pos-1)
              (setq content (substr s (+ brk-pos 2) (- close-pos brk-pos 1)))
              ;; Build a window = content + up to 15 chars after close bracket.
              ;; The alt keyword must be found within this window, either inside
              ;; the brackets (e.g. "[6,948 kW]") or immediately after
              ;; (e.g. "[6948] kW").
              (setq after-close
                    (if (< (1+ close-pos) (strlen s))
                      (substr s (+ close-pos 2) (min 15 (- (strlen s) close-pos 1)))
                      ""))
              (setq window (strcase (strcat content " " after-close)))
              (setq alt-p nil)
              (foreach nm alt-list
                (if (null alt-p)
                  (setq alt-p (vl-string-search nm window 0))))
              (if alt-p
                (setq alt-val (DQC:parse-num-from-content content))
                nil))))))))

;;; Extract the first number from a bracket-content string (handles commas AND sci notation).
(defun DQC:parse-num-from-content (s / i ns dot ch sci-val)
  ;; First try scientific notation
  (setq sci-val (DQC:extract-scientific s))
  (if sci-val
    sci-val
    (progn
      ;; Original number extraction logic
      (setq i 1 ns "" dot nil)
      ;; skip non-number prefix
      (while (and (<= i (strlen s))
                  (not (wcmatch (substr s i 1) "#"))
                  (not (and (= (substr s i 1) ".") (<= (1+ i) (strlen s))
                            (wcmatch (substr s (1+ i) 1) "#"))))
        (setq i (1+ i)))
      (while (and (<= i (strlen s))
                  (or (wcmatch (substr s i 1) "#")
                      (and (= (substr s i 1) ".") (not dot))
                      (= (substr s i 1) ",")))
        (if (= (substr s i 1) ".") (setq dot T))
        (setq ns (strcat ns (substr s i 1)) i (1+ i)))
      (if (= (strlen ns) 0) nil (DQC:atof-safe ns)))))


;;; Extract first number from a [bracket] pattern - no keyword required.
;;; Used for cross-entity inch->mm detection in MED.
(defun DQC:extract-bracket-number (s / brk close content)
  (setq brk (vl-string-search "[" s 0))
  (if (null brk) nil
    (progn
      (setq close (vl-string-search "]" s (1+ brk)))
      (if (null close) nil
        (progn
          (setq content (substr s (+ brk 2) (- close brk 1)))
          (DQC:parse-num-from-content content))))))

;;; Main cross-entity matcher.
;;; ent-info-list = list of (ename stripped-text ins-pt txth)
;;; rules = list of rule tuples
;;; Returns (total pass fail).  Places balloons.
(defun DQC:match-cross-entity (ent-info-list rules
                               / total pass fail rec ename txt pt th
                                 rule kwl altl fac pl al dp-p dp-a tol
                                 hits hit pv alt-val expected ok label layer
                                 unit-rec unit-pt unit-txt
                                 alt-rec alt-pt alt-txt
                                 dx dy dx2 best-dx
                                 nb-rec nb-txt nb-pt nb-dy nb-dx
                                 nb-best-dx _nb-alt)
  (setq total 0 pass 0 fail 0)

  (foreach rec ent-info-list
    (setq ename (nth 0 rec)
          txt   (nth 1 rec)
          pt    (nth 2 rec)
          th    (nth 3 rec))

    (foreach rule rules
      (setq kwl  (nth 0 rule)
            altl (nth 1 rule)
            fac  (nth 2 rule)
            pl   (nth 3 rule)
            al   (nth 4 rule)
            dp-p (nth 5 rule)
            dp-a (nth 6 rule)
            tol  (if (nth 7 rule) (nth 7 rule) 0.5)
            alt-val nil)

      ;; Normal inline case
      (setq hits (DQC:find-prim-hits txt kwl))

      ;; Split-number case
      (if (and (null hits) pt (DQC:number-only-text txt))
        (progn
          (setq pv (DQC:number-only-text txt))
          (setq best-dx 1e99 hits nil)

          ;; Find UNIT to right
          (foreach unit-rec ent-info-list
            (setq unit-txt (nth 1 unit-rec)
                  unit-pt  (nth 2 unit-rec))

            (if (and unit-pt
                     (not (eq (nth 0 unit-rec) ename))
                     (DQC:unit-text-has? unit-txt kwl))
              (progn
                (setq dy (abs (- (cadr unit-pt) (cadr pt)))
                      dx (- (car unit-pt) (car pt)))

                (if (and (> dx 0.0) (<= dy (* th 1.1)))
                  (progn
                    ;; Unit contains alt
                    (setq alt-val (DQC:find-alt-in-string unit-txt altl 1))

                    ;; Alt in separate block
                    (if (null alt-val)
                      (foreach alt-rec ent-info-list
                        (setq alt-txt (nth 1 alt-rec)
                              alt-pt  (nth 2 alt-rec))
                        (if (and alt-pt
                                 (not (member (nth 0 alt-rec)
                                              (list ename (nth 0 unit-rec)))))
                          (progn
                            (setq dy (abs (- (cadr alt-pt) (cadr unit-pt)))
                                  dx2 (- (car alt-pt) (car unit-pt)))
                            (if (and (> dx2 0.0) (<= dy (* th 1.1)) (null alt-val))
                              (setq alt-val
                                    (DQC:find-alt-in-string alt-txt altl 1)))))))

                    (if (and alt-val (< dx best-dx))
                      (setq best-dx dx
                            hits (list (list pv 1 0 (car kwl))))))))))))

      ;; Evaluate
      (foreach hit hits
        (setq pv (car hit)
              alt-val (if (DQC:find-alt-in-string txt altl 1)
                        (DQC:find-alt-in-string txt altl 1)
                        alt-val))
        ;; Cross-entity: primary+unit in one box, [alt] in closest right-side box
        (if (and (null alt-val) pt)
          (progn
            (setq nb-best-dx 1e99)
            (foreach nb-rec ent-info-list
              (setq nb-txt (nth 1 nb-rec)
                    nb-pt  (nth 2 nb-rec))
              (if (and nb-pt
                       (not (eq (nth 0 nb-rec) ename)))
                (progn
                  (setq nb-dy (abs (- (cadr nb-pt) (cadr pt)))
                        nb-dx (- (car nb-pt) (car pt)))
                  (if (and (> nb-dx 0.0) (<= nb-dy (* th 1.2)) (< nb-dx nb-best-dx))
                    (progn
                      (setq _nb-alt (DQC:find-alt-in-string nb-txt altl 1))
                      (if _nb-alt
                        (setq nb-best-dx nb-dx
                              alt-val _nb-alt)))))))))

        (if alt-val
          (progn
            (setq expected (* pv fac)
                  ok (DQC:ok-abs? pv alt-val fac tol)
                  total (1+ total))
            (if ok
              (setq pass (1+ pass)
                    label (DQC:pass-label)
                    layer DQC:PASS-LAYER)
              (setq fail (1+ fail)
                    label (DQC:fail-label
                            (strcat "\\U+2717 "
                                    (rtos pv 2 dp-p) " " pl " ["
                                    (rtos alt-val 2 dp-a) " " al
                                    "] exp "
                                    (rtos expected 2 dp-a) " " al))
                    layer DQC:FAIL-LAYER))
            (DQC:place-balloon pt th 0.0 label layer))
          (progn
            (setq total (1+ total)
                  fail (1+ fail))
            (DQC:place-balloon pt th 0.0
              (DQC:fail-label
                (strcat "\\U+2717 " (rtos pv 2 dp-p) " " pl " [?]"))
              DQC:FAIL-LAYER))))))

  (list total pass fail))
;;; ============================================================================
;;;  PART 10 - MODE 1: OPERATING CONDITIONS CHECK
;;; ============================================================================

(defun DQC:run-opcond (doc / ss ent-info-list i len ename txt pt th res
                             rules total pass fail)
  (princ "\n Select ALL Operating Conditions text lines (window or pick), ENTER when done:")
  (setq ss (ssget '((0 . "MTEXT,TEXT"))))
  (if (null ss)
    (progn (princ "\n Nothing selected.\n") (list 0 0 0 0))
    (progn
      (setq ent-info-list nil len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (if (not (DQC:on-qc-layer? ename))
          (progn
            (setq txt (DQC:get-text ename))
            (setq pt  (DQC:dim-textpt ename))
            (setq th  (DQC:dim-txth ename))
            (setq ent-info-list
                  (append ent-info-list (list (list ename txt pt th))))))
        (setq i (1+ i)))
      (setq rules
        (list
          ;; HP -> kW  (tol=5.0 for OP: only unit-digit rounding matters)
          (list (list "HP") (list "KW") 0.7457 "HP" "kW" 1 2 5.0)
          ;; IN-LB -> N-M  (tol=5.0 for OP)
          (list (list "IN-LB" "IN.LB" "IN LB" "INLB")
                (list "N-M" "N.M" "NM" "N M") 0.112985 "IN-LB" "N-M" 0 2 5.0)))
      (setq res (DQC:match-cross-entity ent-info-list rules))
      (list (car res) (cadr res) (caddr res) 0))))


;;; ============================================================================
;;;  PART 11 - MODE 2: MED CHECK
;;; ============================================================================

;;; Cross-entity inch [mm] check for MED: pure-number entity | [bracket] entity.
;;; Only fires when the bracket value is plausibly an inch->mm result (within 20%).
(defun DQC:med-cross-inch-mm (ent-info-list / total pass fail
                                  rec ename txt pt th pv
                                  nb-rec nb-txt nb-pt nb-dy nb-dx
                                  nb-best-dx _nb-alt
                                  alt-val expected ok label layer df)
  (setq total 0 pass 0 fail 0)
  (foreach rec ent-info-list
    (setq ename (nth 0 rec)
          txt   (nth 1 rec)
          pt    (nth 2 rec)
          th    (nth 3 rec))
    (setq pv (DQC:number-only-text txt))
    (if (and pv pt (> pv 0.0))
      (progn
        (setq alt-val nil nb-best-dx 1e99)
        (foreach nb-rec ent-info-list
          (setq nb-txt (nth 1 nb-rec)
                nb-pt  (nth 2 nb-rec))
          (if (and nb-pt
                   (not (eq (nth 0 nb-rec) ename)))
            (progn
              (setq nb-dy (abs (- (cadr nb-pt) (cadr pt)))
                    nb-dx (- (car nb-pt) (car pt)))
              (if (and (> nb-dx 0.0) (<= nb-dy (* th 1.2)) (< nb-dx nb-best-dx))
                (progn
                  (setq _nb-alt (DQC:extract-bracket-number nb-txt))
                  (if _nb-alt
                    (setq nb-best-dx nb-dx
                          alt-val _nb-alt)))))))
        (if alt-val
          (progn
            (setq expected (* pv DQC:MM/IN)
                  df (abs (- expected (abs alt-val))))
            ;; Pre-filter: ratio must be plausibly inch->mm (within 20% of expected)
            (if (< df (* expected 0.2))
              (progn
                (setq ok (<= df 0.5) total (1+ total))
                (if ok
                  (setq pass (1+ pass) label (DQC:pass-label) layer DQC:PASS-LAYER)
                  (setq fail (1+ fail)
                        label (DQC:fail-label
                                (strcat "\\U+2717 " (rtos pv 2 3) " IN ["
                                        (rtos alt-val 2 1) "] exp "
                                        (rtos expected 2 1) " MM"))
                        layer DQC:FAIL-LAYER))
                (DQC:place-balloon pt th 0.0 label layer))))))))
  (list total pass fail))

(defun DQC:run-med (doc / ss ent-info-list i len ename txt pt th res rec
                          rules total pass fail r2 r3)
  (princ "\n Select ALL MED data text lines (window or pick), ENTER when done:")
  (setq ss (ssget '((0 . "MTEXT,TEXT"))))
  (if (null ss)
    (progn (princ "\n Nothing selected.\n") (list 0 0 0 0))
    (progn
      (setq ent-info-list nil len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (if (not (DQC:on-qc-layer? ename))
          (progn
            (setq txt (DQC:strip (DQC:get-text ename) 0.0 0.0))
            (setq pt  (DQC:dim-textpt ename))
            (setq th  (DQC:dim-txth ename))
            (setq ent-info-list
                  (append ent-info-list (list (list ename txt pt th))))))
        (setq i (1+ i)))

      ;; MED rules with RAD/DEG/compound stiffness detection removed.
      ;; Compound-unit false hits are blocked by DQC:find-prim-hits and DQC:unit-text-has?.
      (setq rules
        (list
          ;; Torque: FT-LB -> N-M
          (list (list "FT-LB" "FT LB" "FT.LB" "FTLB")
                (list "N-M" "N.M" "NM" "N M") 1.355818 "FT-LB" "N-M" 1 2)

          ;; Torque: IN-LB -> N-M
          ;; Does NOT match compound stiffness units.
          (list (list "IN-LB" "IN.LB" "IN LB" "INLB")
                (list "N-M" "N.M" "NM" "N M") 0.112985 "IN-LB" "N-M" 0 2)

          ;; Linear stiffness: LB/IN -> N/MM
          (list (list "LB/IN") (list "N/MM") 0.175127 "LB/IN" "N/MM" 1 2)

          ;; Weight: LB -> KG
          ;; Does NOT match compound stiffness units.
          (list (list "LBS" "LB") (list "KG") 0.453592 "LB" "KG" 1 2)

          ;; Pressure: PSI -> KPA
          (list (list "PSI") (list "KPA") 6.89476 "PSI" "kPa" 0 1)))

      (setq res (DQC:match-cross-entity ent-info-list rules))
      ;; Also scan inline inch [mm] in each entity
      (setq total (car res) pass (cadr res) fail (caddr res))
      (foreach rec ent-info-list
        (setq r2 (DQC:check-inline-mm-info rec total pass fail))
        (setq total (car r2) pass (cadr r2) fail (caddr r2)))
      ;; Cross-entity inch [mm]: pure-number box | [mm bracket] box
      (setq r3 (DQC:med-cross-inch-mm ent-info-list))
      (setq total (+ total (car r3))
            pass  (+ pass  (cadr r3))
            fail  (+ fail  (caddr r3)))
      (list total pass fail 0))))


;;; ============================================================================
;;;  PART 12 - MODE 3: DIMENSIONS CHECK (all dimension entities)
;;; ============================================================================

(defun DQC:process-dim (ename doc / obj ed etype
                              meas sname lfac primary-auto is-dim
                              ts to g1 flags70 alt-on dimaltf
                              pair stripped raw from-text from-meas-sub
                              pfx in-dp mm-dp style-dp bracket-pos inch-seg
                              primary alt expected ok label layer
                              in-tol mm-tol tol-str txtpt txth dimang
                              range-data r-lo r-hi r-mlo r-mhi r-ok-lo r-ok-hi)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (vl-catch-all-error-p obj)
    (list 'SKIP "" nil nil nil "")
    (progn
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))
      (if (DQC:on-qc-layer? ename)
        (list 'SKIP "" nil nil nil "")
        (progn
          (setq etype (if ed (strcase (cdr (assoc 0 ed))) "?"))
          (setq is-dim
            (wcmatch etype
              "DIMENSION,ROTATED*,LINEAR*,ALIGNED*,ANG*,DIAMETR*,RADIAL*,ORDINATE*"))
          (setq meas (if ed (cdr (assoc 42 ed)) nil))
          (if (null meas) (setq meas 0.0))
          (setq sname (DQC:dim-style ename) lfac (DQC:lfac sname doc))
          (setq primary-auto (* (abs meas) lfac))
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
              (if (and flags70 (= (logand flags70 2) 2)) (setq alt-on :vlax-true))))
          (setq dimaltf (if ed (cdr (assoc 143 ed)) nil))
          (if (or (null dimaltf) (zerop dimaltf)) (setq dimaltf DQC:MM/IN))
          (if (or (wcmatch (strcase ts) "*TAPER*")
                  (wcmatch (strcase to) "*TAPER*")
                  (wcmatch (strcase g1) "*TAPER*"))
            (list 'SKIP "" nil nil nil "TAPER")
            (progn
              (setq pair nil from-text nil from-meas-sub nil stripped "" raw "")
              (if (and (null pair) (> (strlen (DQC:trim ts)) 0))
                (progn
                  (setq stripped (DQC:strip ts primary-auto (* primary-auto DQC:MM/IN)))
                  (setq raw stripped pair (DQC:parse stripped))
                  (if pair (setq from-text T from-meas-sub nil))))
              (if (and (null pair) (> (strlen (DQC:trim to)) 0))
                (progn
                  (setq stripped (DQC:strip to primary-auto (* primary-auto DQC:MM/IN)))
                  (setq raw stripped pair (DQC:parse stripped))
                  (if pair (setq from-text T from-meas-sub (DQC:has-meas-token to)))))
              (if (and (null pair) (= (type g1) 'STR) (> (strlen (DQC:trim g1)) 0))
                (progn
                  (setq stripped (DQC:strip g1 primary-auto (* primary-auto DQC:MM/IN)))
                  (setq raw stripped pair (DQC:parse stripped))
                  (if pair (setq from-text T from-meas-sub (DQC:has-meas-token g1)))))
              (if is-dim
                (progn
                  (if (and (null pair) (or (eq alt-on :vlax-true) (= alt-on -1)))
                    (progn
                      (setq stripped (strcat (rtos primary-auto 2 6)
                                             " [" (rtos (* primary-auto dimaltf) 2 6) "]"))
                      (setq raw stripped pair (DQC:parse stripped))
                      (if pair (setq from-meas-sub T))))
                  (if (and (null pair) (> primary-auto 0.0001))
                    (progn
                      (setq stripped (strcat (rtos primary-auto 2 6)
                                             " [" (rtos (* primary-auto DQC:MM/IN) 2 6) "]"))
                      (setq raw stripped pair (DQC:parse stripped))
                      (if pair (setq from-meas-sub T))))))
              (setq pfx (DQC:dim-prefix stripped))
              (if (= pfx "") (setq pfx (DQC:dim-prefix ts)))
              (if (= pfx "") (setq pfx (DQC:dim-prefix to)))
              (if (= pfx "") (setq pfx (DQC:dim-prefix g1)))
              (setq style-dp (DQC:dimdec sname doc))
              (if (or (null style-dp) (< style-dp 0)) (setq style-dp (fix (getvar "DIMDEC"))))
              (if (and from-text (not from-meas-sub))
                (progn
                  (setq bracket-pos (DQC:find-char stripped "[" 1))
                  (setq inch-seg
                    (if (= bracket-pos 0) stripped
                      (DQC:trim (substr stripped 1 (1- bracket-pos)))))
                  (setq in-dp (DQC:count-dp-in-token inch-seg)))
                (setq in-dp style-dp))
              (setq mm-dp (if (= in-dp 0) 0 (max 1 (1- in-dp))))
              (setq in-tol (if pair (nth 2 pair) nil))
              (setq mm-tol (if pair (nth 3 pair) nil))
              (setq tol-str
                (if (or in-tol mm-tol)
                  (strcat " [" (DQC:fmt-tol in-tol) " | " (DQC:fmt-tol mm-tol) "]") ""))
              (setq txtpt (DQC:dim-textpt ename))
              (setq txth  (DQC:dim-txth  ename))
              (setq dimang (vl-catch-all-apply 'vlax-get (list obj 'TextRotation)))
              (if (vl-catch-all-error-p dimang)
                (setq dimang (if ed (cdr (assoc 53 ed)) 0.0)))
              (if (null dimang) (setq dimang 0.0))
              ;; ---- Range dimension check: "a/b [c/d]" style ---------------
              ;; e.g. ".0118/.0125 [0.300/0.318]"  bore/shaft tolerance ranges
              (setq range-data (DQC:parse-range-dim stripped))
              (if range-data
                (progn
                  (setq r-lo    (nth 0 range-data)
                        r-hi   (nth 1 range-data)
                        r-mlo  (nth 2 range-data)
                        r-mhi  (nth 3 range-data))
                  (setq r-ok-lo (DQC:ok? r-lo r-mlo DQC:MM/IN))
                  (setq r-ok-hi (DQC:ok? r-hi r-mhi DQC:MM/IN))
                  (setq ok (and r-ok-lo r-ok-hi))
                  (if ok
                    (setq label (DQC:pass-label) layer DQC:PASS-LAYER)
                    (setq label (DQC:fail-label
                                  (strcat "\\U+2717 "
                                          (rtos r-lo 2 in-dp) "/" (rtos r-hi 2 in-dp)
                                          " [" (rtos r-mlo 2 mm-dp) "/" (rtos r-mhi 2 mm-dp)
                                          "] exp "
                                          (rtos (* r-lo DQC:MM/IN) 2 mm-dp) "/"
                                          (rtos (* r-hi DQC:MM/IN) 2 mm-dp) " mm"))
                          layer DQC:FAIL-LAYER))
                  (DQC:place-balloon txtpt txth dimang label layer)
                  (list (if ok 'PASS 'FAIL) label r-lo r-mlo (* r-lo DQC:MM/IN) raw))
                (cond
                  ((null pair) (list 'SKIP "" nil nil nil raw))
                  ((eq (cadr pair) 'EMPTY)
                   (setq primary  (car pair)
                         expected (* (abs primary) DQC:MM/IN)
                         label    (DQC:fail-label
                                    (strcat "\\U+2717 " pfx (DQC:fmt primary in-dp)
                                            " [?] exp " (DQC:fmt expected mm-dp) " mm" tol-str))
                         layer    DQC:FAIL-LAYER)
                   (DQC:place-balloon txtpt txth dimang label layer)
                   (list 'FAIL label primary nil expected raw))
                  (T
                   (setq primary  (car pair)
                         alt      (cadr pair)
                         expected (* (abs primary) DQC:MM/IN)
                         ok       (DQC:ok? primary alt DQC:MM/IN))
                   (if ok
                     (setq label (DQC:pass-label) layer DQC:PASS-LAYER)
                     (setq label (DQC:fail-label
                                   (strcat "\\U+2717 " pfx (DQC:fmt primary in-dp)
                                           " [" (DQC:fmt alt mm-dp) "] exp "
                                           (DQC:fmt expected mm-dp) " mm" tol-str))
                           layer DQC:FAIL-LAYER))
                   (DQC:place-balloon txtpt txth dimang label layer)
                   (list (if ok 'PASS 'FAIL) label primary alt expected raw)))))))))))

;;; Inline N [mm] check using an ent-info rec (ename stripped-txt pt th)
(defun DQC:count-newlines-before (s pos / n i)
  ;; Count newline chars in s up to (but not including) 1-based pos
  (setq n 0 i 1)
  (while (< i pos)
    (if (<= i (strlen s))
      (if (= (substr s i 1) "\n") (setq n (1+ n))))
    (setq i (1+ i)))
  n)

(defun DQC:check-inline-mm-info (rec total pass fail / ename txt pt th
                                       hits hit pv av expected ok label layer
                                       hit-pos hit-pt df
                                       pv1 pv2 av1 av2 ok1 ok2 sla tol1 tol2 ov)
  (setq ename (nth 0 rec) txt (nth 1 rec) pt (nth 2 rec) th (nth 3 rec))
  (setq hits (DQC:scan-inline-dims txt))
  (foreach hit hits
    (setq hit-pos (nth 2 hit))
    (cond
      ;; ── Range hit: N1/N2 [M1/M2] ─────────────────────────────────────
      ((eq (nth 3 hit) 'RANGE)
       (setq pv1 (DQC:atof-safe (car hit))
             pv2 (DQC:atof-safe (nth 4 hit))
             sla (DQC:find-char (cadr hit) "/" 1))
       (if (and (> pv1 0.0) (> pv2 0.0) (> sla 0))
         (progn
           (setq av1  (DQC:atof-safe (substr (cadr hit) 1 (1- sla)))
                 av2  (DQC:atof-safe (substr (cadr hit) (1+ sla)))
                 tol1 (DQC:rounding-tol (DQC:trim (substr (cadr hit) 1 (1- sla))))
                 tol2 (DQC:rounding-tol (DQC:trim (substr (cadr hit) (1+ sla)))))
           (if (and (> av1 0.0) (> av2 0.0))
             (progn
               (setq ok1 (<= (abs (- (* pv1 DQC:MM/IN) av1)) tol1)
                     ok2 (<= (abs (- (* pv2 DQC:MM/IN) av2)) tol2)
                     ok  (and ok1 ok2)
                     total (1+ total))
               (if ok
                 (setq pass (1+ pass) label (DQC:pass-label) layer DQC:PASS-LAYER)
                 (setq fail (1+ fail)
                       label (DQC:fail-label
                               (strcat "\\U+2717 "
                                       (car hit) "/" (nth 4 hit)
                                       " [" (rtos av1 2 3) "/" (rtos av2 2 3)
                                       "] exp "
                                       (rtos (* pv1 DQC:MM/IN) 2 3) "/"
                                       (rtos (* pv2 DQC:MM/IN) 2 3) " mm"))
                       layer DQC:FAIL-LAYER))
               (if (and pt th hit-pos)
                 (progn
                   (setq ov (DQC:rot2 (DQC:note-x-offset ename txt hit-pos th)
                                      0.0
                                      (DQC:text-rotation ename)))
                   (setq hit-pt (list (+ (car pt) (car ov))
                                      (+ (cadr pt) (cadr ov))
                                      (if (caddr pt) (caddr pt) 0.0)))
                   (DQC:place-balloon hit-pt th 0.0 label layer))
                 (DQC:place-balloon pt th 0.0 label layer)))))))
      ;; ── Simple hit: N [M] ────────────────────────────────────────────
      (T
       (setq pv (DQC:atof-safe (car hit)) av (DQC:atof-safe (cadr hit)))
       (if (and (> pv 0.0) (> av 0.0))
         (progn
           (setq expected (* (abs pv) DQC:MM/IN)
                 df (abs (- expected (abs av)))
                 ok (<= df 0.5)
                 total (1+ total))
           (if ok
             (setq pass (1+ pass) label (DQC:pass-label) layer DQC:PASS-LAYER)
             (setq fail (1+ fail)
                   label (DQC:fail-label
                           (strcat "\\U+2717 " (car hit) " [" (cadr hit)
                                   "] exp " (rtos expected 2 2) " mm"))
                   layer DQC:FAIL-LAYER))
           (if (and pt th hit-pos)
             (progn
               (setq ov (DQC:rot2 (DQC:note-x-offset ename txt hit-pos th)
                                  0.0
                                  (DQC:text-rotation ename)))
               (setq hit-pt (list (+ (car pt) (car ov))
                                  (+ (cadr pt) (cadr ov))
                                  (if (caddr pt) (caddr pt) 0.0)))
               (DQC:place-balloon hit-pt th 0.0 label layer))
             (DQC:place-balloon pt th 0.0 label layer)))))))
  (list total pass fail))

(defun DQC:run-dims (doc / ss len i ename ed etype res total pass fail skip
                           txt pt th rec r2)
  (setq total 0 pass 0 fail 0 skip 0)
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss)
    (princ "\n No dimension entities found.\n")
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (if (not (DQC:on-qc-layer? ename))
          (progn
            (setq ed (vl-catch-all-apply 'entget (list ename)))
            (if (vl-catch-all-error-p ed) (setq ed nil))
            (setq etype (if ed (strcase (cdr (assoc 0 ed))) ""))
            (if (wcmatch etype "MTEXT,TEXT")
              (progn
                ;; Text/MText can contain multiple inline checks. Count actual
                ;; checks, not the parent entity, and do not run process-dim on
                ;; the same text again.
                (setq txt (DQC:strip (DQC:get-text ename) 0.0 0.0))
                (setq pt (DQC:dim-textpt ename))
                (setq th (DQC:dim-txth ename))
                (setq rec (list ename txt pt th))
                (setq r2 (DQC:check-inline-mm-info rec 0 0 0))
                (setq total (+ total (car r2))
                      pass  (+ pass  (cadr r2))
                      fail  (+ fail  (caddr r2)))
                (if (= (car r2) 0) (setq skip (1+ skip)))
              )
              (progn
                (setq res (DQC:process-dim ename doc))
                (cond
                  ((= (car res) 'PASS) (setq total (1+ total) pass (1+ pass)))
                  ((= (car res) 'FAIL) (setq total (1+ total) fail (1+ fail)))
                  (T (setq skip (1+ skip))))
              )
            )))
        (setq i (1+ i)))))
  (list total pass fail skip))


;;; ============================================================================
;;;  PART 13 - MODE 4: NOTES CHECK
;;; ============================================================================

(defun DQC:run-notes (doc / ss ent-info-list i len ename txt pt th res
                            rules total pass fail r2 rec)
  (princ "\n Select ALL Notes text lines (window or pick), ENTER when done:")
  (setq ss (ssget '((0 . "MTEXT,TEXT"))))
  (if (null ss)
    (progn (princ "\n Nothing selected.\n") (list 0 0 0 0))
    (progn
      (setq ent-info-list nil len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (if (not (DQC:on-qc-layer? ename))
          (progn
            (setq txt (DQC:strip (DQC:get-text ename) 0.0 0.0))
            (setq pt  (DQC:dim-textpt ename))
            (setq th  (DQC:dim-txth ename))
            (setq ent-info-list
                  (append ent-info-list (list (list ename txt pt th))))))
        (setq i (1+ i)))
      (setq rules
        (list
          ;; Weight: LB -> KG
          (list (list "LBS" "LB") (list "KG") 0.453592 "LB" "KG" 1 2)))
      (setq res (DQC:match-cross-entity ent-info-list rules))
      (setq total (car res) pass (cadr res) fail (caddr res))
      ;; Inch [mm] inline check per entity
      (foreach rec ent-info-list
        (setq r2 (DQC:check-inline-mm-info rec total pass fail))
        (setq total (car r2) pass (cadr r2) fail (caddr r2)))
      (list total pass fail 0))))


;;; ============================================================================
;;;  PART 14 - RESET
;;; ============================================================================

(defun DQC:erase-balloons (doc / ss len i lname)
  ;; v17.2 marks include MTEXT + LINE + CIRCLE, so erase everything on QC layers.
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (setq ss (ssget "X" (list (cons 8 lname))))
    (if ss
      (progn
        (setq len (sslength ss) i 0)
        (while (< i len)
          (vl-catch-all-apply 'entdel (list (ssname ss i)))
          (setq i (1+ i))))))
  (vl-catch-all-apply 'vla-Regen (list doc acAllViewports)))

(defun C:DIMQC-RESET ( / doc n ss len i lname)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq n 0)
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (setq ss (ssget "X" (list (cons 8 lname))))
    (if ss
      (progn
        (setq len (sslength ss) i 0)
        (while (< i len)
          (vl-catch-all-apply 'entdel (list (ssname ss i)))
          (setq n (1+ n) i (1+ i))))))
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (DQC:purge-layer lname doc))
  (vla-Regen doc acAllViewports)
  (princ (strcat "\n Removed " (itoa n) " QC mark entity/entities and deleted QC layers.\n"))
  (princ))


;;; ============================================================================
;;;  PART 15 - MAIN COMMAND C:DIMQC
;;; ============================================================================

(defun C:DIMQC ( / *error* oldError oldCmdecho doc mode result total pass fail skip sumstr)
  (vl-load-com)
  (setq oldError *error*
        oldCmdecho (getvar "CMDECHO"))
  (defun *error* (msg)
    (if doc (vl-catch-all-apply 'vla-Regen (list doc acAllViewports)))
    (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
    (setq *error* oldError)
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nDIMQC error: " msg))
    )
    (princ)
  )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (DQC:ensure-layer DQC:PASS-LAYER DQC:PASS-COLOR doc)
  (DQC:ensure-layer DQC:FAIL-LAYER DQC:FAIL-COLOR doc)
  (DQC:erase-balloons doc)

  (princ "\n============================================")
  (princ "\n  DIM QC v18.0-CANONICAL - Select Check Mode")
  (princ "\n============================================")
  (princ "\n  1. Operating Conditions Check")
  (princ "\n  2. MED Check")
  (princ "\n  3. Dimensions Check")
  (princ "\n  4. Notes Check")
  (princ "\n============================================")
  (setq mode (getint "\n Enter mode number (1-4): "))

  (if (or (null mode) (< mode 1) (> mode 4))
    (progn
      (princ "\n Invalid selection. Cancelled.\n")
      (setq result (list 0 0 0 0)))
    (cond
      ((= mode 1)
       (princ "\n Running Operating Conditions Check...\n")
       (setq result (DQC:run-opcond doc)))
      ((= mode 2)
       (princ "\n Running MED Check...\n")
       (setq result (DQC:run-med doc)))
      ((= mode 3)
       (princ "\n Running Dimensions Check (inch [mm])...\n")
       (setq result (DQC:run-dims doc)))
      ((= mode 4)
       (princ "\n Running Notes Check...\n")
       (setq result (DQC:run-notes doc)))))

  (if (null result) (setq result (list 0 0 0 0)))
  (setq total (car result)
        pass  (cadr result)
        fail  (caddr result)
        skip  (cadddr result))
  (if (null total) (setq total 0))
  (if (null pass)  (setq pass 0))
  (if (null fail)  (setq fail 0))
  (if (null skip)  (setq skip 0))

  (vla-Regen doc acAllViewports)
  (setq sumstr (strcat "Checked: " (itoa (+ pass fail))
                       "   PASS: " (itoa pass)
                       "   FAIL: " (itoa fail)
                       "   Skipped: " (itoa skip)))
  (princ (strcat "\n " sumstr "\n"))
  (if oldCmdecho (setvar "CMDECHO" oldCmdecho))
  (setq *error* oldError)
  (princ))


;;; ============================================================================
;;;  PART 16 - DIMQC-DIAG  (Mode 3 debug output)
;;; ============================================================================

(defun C:DIMQC-DIAG ( / doc ss len i ename obj ed etype sname lfac is-dim
                         meas raw stripped pair primary alt expected
                         ts to g alt-on dimaltf flags70)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n========== DIMQC DIAGNOSTIC v18.0-CANONICAL ==========\n")
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss)
    (princ " No entities found.\n")
    (progn
      (setq len (sslength ss))
      (princ (strcat " " (itoa len) " entities found.\n\n"))
      (setq i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
        (setq ed  (vl-catch-all-apply 'entget (list ename)))
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
        (setq pair nil raw "" stripped "")
        (if (and (null pair) (> (strlen (DQC:trim ts)) 0))
          (progn
            (setq stripped (DQC:strip ts (* (abs meas) lfac) (* (* (abs meas) lfac) DQC:MM/IN)))
            (setq raw (strcat "TextString: " stripped))
            (setq pair (DQC:parse stripped))))
        (princ (strcat "ITEM #" (itoa (1+ i)) " [" etype "]\n"))
        (princ (strcat "  Raw    : " raw "\n"))
        (princ (strcat "  Meas   : " (rtos meas 2 6) "\n"))
        (princ "\n")
        (setq i (1+ i))
        (if (= (rem i 20) 0) (getstring " --- ENTER for next batch --- ")))
      (princ "========== END ==========\n")))
  (princ))


;;; ============================================================================
;;;  PART 17 - SELF TESTS
;;; ============================================================================

(defun DQC:selftest-check (name ok /)
  (princ (strcat "\n  " (if ok "PASS " "FAIL ") name))
  ok)

(defun C:DQC_SELFTEST ( / pass fail ok v range hits)
  (setq pass 0 fail 0)
  (princ "\nDIMQC self-test")

  (setq v (DQC:rounding-tol "3.18"))
  (setq ok (equal v 0.005 1e-8))
  (if (DQC:selftest-check "half-unit rounding tolerance" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq range (DQC:parse-range-dim ".0118/.0125 [0.300/0.318]"))
  (setq ok (and range (equal (car range) 0.0118 1e-8) (equal (cadr range) 0.0125 1e-8)))
  (if (DQC:selftest-check "ascending range parse" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq range (DQC:parse-range-dim ".0125/.0118 [0.318/0.300]"))
  (setq ok (null range))
  (if (DQC:selftest-check "reversed range rejected" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq hits (DQC:scan-inline-dims "2X .125 [3.18]"))
  (setq ok (and hits (equal (car (car hits)) ".125") (equal (cadr (car hits)) "3.18")))
  (if (DQC:selftest-check "inline count prefix ignored" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq v (DQC:extract-scientific "8.6 X 10^6 IN-LB"))
  (setq ok (equal v 8.6 1e-8))
  (if (DQC:selftest-check "scientific coefficient extraction" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (setq hits (DQC:find-prim-hits "8.6 IN-LB/RAD" (list "IN-LB")))
  (setq ok (null hits))
  (if (DQC:selftest-check "compound unit boundary rejection" ok) (setq pass (1+ pass)) (setq fail (1+ fail)))

  (princ (strcat "\nDIMQC self-test done: " (itoa pass) " pass, " (itoa fail) " fail."))
  (princ))


;;; ============================================================================
;;;  LOAD MESSAGE
;;; ============================================================================
(princ "\n================================================\n")
(princ " DIM QC v18.0-CANONICAL Loaded.\n")
(princ "   DIMQC        Mode menu:\n")
(princ "     1 = Operating Conditions (HP/kW, IN-LB/N-M)\n")
(princ "     2 = MED Check (LB/KG, torque, LB/IN, PSI; no RAD/compound stiffness)\n")
(princ "                  x10^N values: coefficient-only comparison\n")
(princ "     3 = Dimensions Check (inch [mm]) - all entities\n")
(princ "     4 = Notes Check (LB/KG + inch/mm)\n")
(princ "   DIMQC-RESET  Remove marks + delete QC layers\n")
(princ "   DIMQC-DIAG   Command-line diagnostic\n")
(princ "   DQC_SELFTEST  Parser/rule self-test\n")
(princ "================================================\n")
(princ)
