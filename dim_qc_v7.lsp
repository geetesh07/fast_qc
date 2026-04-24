;;; ============================================================================
;;;  dim_qc_v7.lsp  -  Engineering Dual-Unit Dimension QC
;;;  Version 7.0
;;;
;;;  COMMANDS
;;;    DIMQC        Mode selection menu -> run selected check -> place marks
;;;    DIMQC-RESET  Erase all marks AND delete QC layers
;;;    DIMQC-DIAG   Command-line diagnostic (inch/mm dimensions)
;;;
;;;  MODES
;;;    1. Operating Conditions  - user selects text, checks HP/kW and IN-LB/N-M
;;;    2. MED Check             - user selects text, checks LB/KG, stiffness, etc.
;;;    3. Dimensions Check      - all dimension entities, inch [mm] only
;;;    4. Notes Check           - user selects notes, checks weight + inch/mm
;;;
;;;  LAYERS (deleted on DIMQC-RESET)
;;;    DIM_QC_PASS  colour 3  (green)
;;;    DIM_QC_FAIL  colour 7  (white, bold)
;;; ============================================================================

(vl-load-com)

;;; Global constants
(setq DQC:MM/IN    25.4)
(setq DQC:REL-TOL  0.03)
(setq DQC:ABS-TOL  0.08)
(setq DQC:TXT-H    nil)
(setq DQC:OFFSET   nil)
(setq DQC:PASS-LAYER "DIM_QC_PASS")
(setq DQC:FAIL-LAYER "DIM_QC_FAIL")
(setq DQC:PASS-COLOR 3)
(setq DQC:FAIL-COLOR 7)


;;; ============================================================================
;;;  PART 1 - CORE UTILITIES
;;; ============================================================================

(defun DQC:trim (s)
  (if (or (null s) (/= (type s) 'STR)) "" (vl-string-trim " " s)))

(defun DQC:find-char (s c pos / p)
  (if (setq p (vl-string-search c s (1- pos))) (1+ p) 0))

;;; Tolerance check: primary in first unit, alt in second unit, factor converts first->second
(defun DQC:ok? (primary alt factor / ex df)
  (if (< (abs primary) 1e-9)
    (< (abs alt) 0.1)
    (progn
      (setq ex (* (abs primary) factor)
            df (abs (- ex (abs alt))))
      (if (equal factor 25.4 1e-5)
        (and (<= (/ df ex) DQC:REL-TOL) (<= df DQC:ABS-TOL))
        (<= (/ df ex) DQC:REL-TOL)))))

(defun DQC:fmt (val dp) (rtos val 2 dp))

(defun DQC:ensure-layer (name aci doc / layers lay)
  (setq layers (vla-get-Layers doc))
  (setq lay
    (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-item (list layers name)))
      (vla-add layers name)
      (vla-item layers name)))
  (vla-put-Color lay aci)
  lay)

(defun DQC:purge-layer (lname doc / ss len i layers lay err)
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

;;; Label builders
(defun DQC:pass-label () "{\\fArial|b1|i0;\\U+2713}")
(defun DQC:fail-label (body) (strcat "{\\fArial|b1|i0;" body "}"))

;;; Get text string from entity - returns plain string
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

;;; Is entity on a QC layer?
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
                (setq out (strcat out "|") i (+ i 2))))
             ((= nx "X") (setq out (strcat out "|") i (+ i 2)))
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
;;;  PART 3 - TOLERANCE PARSER
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
;;;  PART 4 - TEXT / NUMBER PARSER
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
      (setq v (atof tok))
      (if (= v 0.0) (if (wcmatch (substr tok 1 1) "#") v nil) v))))

(defun DQC:parse-metric (s / v i ch prev)
  (setq s (DQC:strip-pfx (DQC:drop-tol (DQC:trim s))))
  (if (= (strlen s) 0) nil
    (progn
      (setq i 1 prev nil)
      (while (<= i (strlen s))
        (setq ch (substr s i 1))
        (cond
          ((or (wcmatch ch "#") (= ch ".")) (setq prev 'digit i (1+ i)))
          ((and prev (or (= ch "+") (= ch "-") (= ch "/") (= ch " ")))
           (setq s (substr s 1 (1- i)) i (1+ (strlen s))))
          ((and (null prev) (or (= ch "-") (= ch "+"))) (setq i (1+ i)))
          (T (setq s (substr s 1 (1- i)) i (1+ (strlen s))))))
      (if (= (strlen s) 0) nil
        (progn
          (setq v (atof s))
          (if (and (= v 0.0) (not (wcmatch (substr s 1 1) "#"))) nil v))))))

;;; Main bracket parser: returns (primary alt in-tol mm-tol) or nil
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
;;;  PART 4B - INLINE N [M] SCANNER (for dimension values embedded in text)
;;; ============================================================================

(defun DQC:scan-inline-dims (txt / results i len ch ns in-n dot j as k closed)
  (setq results nil i 1 len (strlen txt))
  (while (<= i len)
    (setq ch (substr txt i 1))
    (if (or (wcmatch ch "#")
            (and (= ch ".") (<= (1+ i) len) (wcmatch (substr txt (1+ i) 1) "#")))
      (progn
        (setq ns "" j i dot nil in-n T)
        (while (and (<= j len) in-n)
          (setq ch (substr txt j 1))
          (cond
            ((wcmatch ch "#") (setq ns (strcat ns ch) j (1+ j)))
            ((and (= ch ".") (not dot)) (setq ns (strcat ns ch) dot T j (1+ j)))
            (T (setq in-n nil))))
        (setq k j)
        (while (and (<= k len) (= (substr txt k 1) " ")) (setq k (1+ k)))
        (if (and (<= k len) (= (substr txt k 1) "["))
          (progn
            (setq k (1+ k) as "" closed nil)
            (while (and (<= k len) (not closed))
              (setq ch (substr txt k 1))
              (if (= ch "]")
                (setq closed T)
                (setq as (strcat as ch) k (1+ k))))
            (if closed
              (progn
                (setq results (append results (list (list ns as i))))
                (setq i k))
              (setq i j)))
          (setq i j)))
      (setq i (1+ i))))
  results)


;;; ============================================================================
;;;  PART 5 - GEOMETRY HELPERS
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


;;; ============================================================================
;;;  PART 6 - MARK PLACEMENT
;;; ============================================================================

(defun DQC:rot2 (dx dy ang / ca sa)
  (setq ca (cos ang) sa (sin ang))
  (list (- (* dx ca) (* dy sa)) (+ (* dx sa) (* dy ca))))

(defun DQC:place-balloon (txtpt txth dimang label layer / bh bw offx offy ovec ins ed)
  (setq bh (if (and DQC:TXT-H (> DQC:TXT-H 0)) DQC:TXT-H (* txth 0.85)))
  (if (< bh 0.5) (setq bh 0.5))
  (setq offx (if (and DQC:OFFSET (> DQC:OFFSET 0)) DQC:OFFSET 0.0)
        offy (* bh 0.7))
  (setq ovec (DQC:rot2 offx offy (if dimang dimang 0.0)))
  (setq ins (list (+ (car txtpt) (car ovec))
                  (+ (cadr txtpt) (cadr ovec))
                  (if (caddr txtpt) (caddr txtpt) 0.0)))
  (setq bw (* (strlen label) bh 0.8))
  (if (< bw (* bh 3.0)) (setq bw (* bh 3.0)))
  (setq ed
    (list (cons 0   "MTEXT")
          (cons 100 "AcDbEntity")
          (cons 8   layer)
          (cons 62  (if (= layer DQC:FAIL-LAYER) DQC:FAIL-COLOR DQC:PASS-COLOR))
          (cons 100 "AcDbMText")
          (cons 10  ins)
          (cons 40  bh)
          (cons 41  bw)
          (cons 71  8)
          (cons 72  1)
          (cons 1   label)))
  (if (vl-catch-all-error-p (vl-catch-all-apply 'entmake (list ed)))
    nil (entlast)))


;;; ============================================================================
;;;  PART 7 - KEYWORD SCANNER (shared by Modes 1, 2, 4)
;;; ============================================================================

;;; Find keyword kw (already uppercase) in uppercase string su from pos (1-based).
;;; Returns 1-based start position or 0.
(defun DQC:kw-find (su kw from / p)
  (setq p (vl-string-search kw su (1- from)))
  (if p (1+ p) 0))

;;; Read number backward from kw-pos in original-case string s.
;;; Returns (value) list or nil.
(defun DQC:num-before-kw (s kw-pos / i ns dot)
  (setq i (1- kw-pos))
  (while (and (> i 0) (= (substr s i 1) " ")) (setq i (1- i)))
  (if (<= i 0) nil
    (progn
      (setq ns "" dot nil)
      (while (and (> i 0)
                  (or (wcmatch (substr s i 1) "#")
                      (and (= (substr s i 1) ".") (not dot))))
        (if (= (substr s i 1) ".") (setq dot T))
        (setq ns (strcat (substr s i 1) ns) i (1- i)))
      (if (= (strlen ns) 0) nil (list (atof ns))))))

;;; Read number forward from pos in string s.
;;; Returns real or nil.
(defun DQC:num-after-pos (s pos / i ns dot)
  (setq i pos)
  (while (and (<= i (strlen s)) (= (substr s i 1) " ")) (setq i (1+ i)))
  (setq ns "" dot nil)
  (while (and (<= i (strlen s))
              (or (wcmatch (substr s i 1) "#")
                  (and (= (substr s i 1) ".") (not dot))))
    (if (= (substr s i 1) ".") (setq dot T))
    (setq ns (strcat ns (substr s i 1)) i (1+ i)))
  (if (= (strlen ns) 0) nil (atof ns)))

;;; Generic unit-pair scanner.
;;; kw-list   = list of primary unit keywords (uppercase)
;;; alt-list  = list of alternate unit keywords (uppercase)
;;; factor    = conversion factor primary->alt
;;; prim-label = string for display e.g. "HP"
;;; alt-label  = string for display e.g. "kW"
;;; dp-prim   = decimal places for primary display
;;; dp-alt    = decimal places for alt display
;;; Returns list of (prim-val alt-val ok expected) entries.
(defun DQC:scan-unit-pairs (s kw-list alt-list factor
                            / su i kw-start kw-end
                              kw-val-lst srch-s brk-p alt-p alt-val
                              expected ok result kw j srch-len nm)
  (setq su (strcase s) result nil i 1)
  (while (<= i (strlen su))
    (setq kw-start 0 kw-end 0)
    (foreach kw kw-list
      (setq j (DQC:kw-find su kw i))
      (if (and (> j 0) (or (= kw-start 0) (< j kw-start)))
        (setq kw-start j kw-end (+ j (strlen kw)))))
    (if (= kw-start 0)
      (setq i (1+ (strlen su)))
      (progn
        (setq kw-val-lst (DQC:num-before-kw s kw-start))
        (if kw-val-lst
          (progn
            (setq srch-len (min 250 (max 1 (- (strlen su) kw-end -1))))
            (setq srch-s (substr su kw-end srch-len))
            (setq brk-p (vl-string-search "[" srch-s 0))
            (setq alt-p nil alt-val nil)
            (if brk-p
              (progn
                (foreach nm alt-list
                  (if (null alt-p)
                    (setq alt-p (vl-string-search nm srch-s (1+ brk-p)))))
                (if alt-p
                  (setq alt-val (DQC:num-after-pos srch-s (1+ brk-p))))))
            (if (and alt-val (car kw-val-lst))
              (progn
                (setq expected (* (car kw-val-lst) factor)
                      ok (DQC:ok? (car kw-val-lst) alt-val factor))
                (setq result (append result
                               (list (list (car kw-val-lst) alt-val ok expected))))))))
        (setq i (1+ kw-end)))))
  result)


;;; ============================================================================
;;;  PART 8 - PROCESS ONE ENTITY FOR USER-SELECTED TEXT MODES
;;;
;;;  Runs a list of unit-pair rules against one text entity.
;;;  rules = list of (kw-list alt-list factor prim-label alt-label dp-p dp-a)
;;;  Returns (total pass fail) increments and places balloons.
;;; ============================================================================

(defun DQC:check-text-rules (ename rules total pass fail
                             / obj ed txt stripped
                               txtpt txth rule kwl altl fac pl al dp-p dp-a
                               hits hit ok expected label layer)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))
      (if (not (DQC:on-qc-layer? ename))
        (progn
          (setq txt (DQC:get-text ename))
          (setq stripped (DQC:strip txt 0.0 0.0))
          (setq txtpt (DQC:dim-textpt ename))
          (setq txth  (DQC:dim-txth  ename))
          (foreach rule rules
            (setq kwl  (nth 0 rule)
                  altl (nth 1 rule)
                  fac  (nth 2 rule)
                  pl   (nth 3 rule)
                  al   (nth 4 rule)
                  dp-p (nth 5 rule)
                  dp-a (nth 6 rule))
            (setq hits (DQC:scan-unit-pairs stripped kwl altl fac))
            (foreach hit hits
              (setq ok       (caddr hit)
                    expected (cadddr hit)
                    total    (1+ total))
              (if ok
                (progn
                  (setq pass  (1+ pass)
                        label (DQC:pass-label)
                        layer DQC:PASS-LAYER))
                (progn
                  (setq fail  (1+ fail)
                        label (DQC:fail-label
                                (strcat "\\U+2717 " pl ": "
                                        (rtos (car hit) 2 dp-p) " " pl " ["
                                        (rtos (cadr hit) 2 dp-a) " " al "] exp "
                                        (rtos expected 2 dp-a) " " al))
                        layer DQC:FAIL-LAYER)))
              (if txtpt (DQC:place-balloon txtpt txth 0.0 label layer))))))))
  (list total pass fail))


;;; ============================================================================
;;;  PART 9 - MODE 1: OPERATING CONDITIONS CHECK
;;;  User selects text. Checks HP/kW and IN-LB/N-M.
;;; ============================================================================

(defun DQC:run-opcond (doc / ss len i ename res total pass fail)
  (setq total 0 pass 0 fail 0)
  (princ "\n Select Operating Conditions text (pick objects, then ENTER):\n")
  (setq ss (ssget '((0 . "MTEXT,TEXT"))))
  (if (null ss)
    (princ "\n Nothing selected.\n")
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (setq res (DQC:check-text-rules ename
                    (list
                      (list (list "HP") (list "KW") 0.7457 "HP" "kW" 1 2)
                      (list (list "IN-LB" "IN.LB" "IN LB" "INLB")
                            (list "N-M" "N.M" "NM" "N M") 0.112985 "IN-LB" "N-M" 0 2))
                    total pass fail))
        (setq total (car res) pass (cadr res) fail (caddr res))
        (setq i (1+ i)))))
  (list total pass fail 0))


;;; ============================================================================
;;;  PART 10 - MODE 2: MED CHECK
;;;  User selects text. Checks: LB/KG, LB-IN/N-M stiffness,
;;;  LB-IN/N-MM stiffness per rev, FT-LB/N-M torque, PSI/KPA,
;;;  RPM stays as-is (no conversion needed), and inch [mm].
;;; ============================================================================

(defun DQC:run-med (doc / ss len i ename res total pass fail)
  (setq total 0 pass 0 fail 0)
  (princ "\n Select MED data text (pick objects, then ENTER):\n")
  (setq ss (ssget '((0 . "MTEXT,TEXT"))))
  (if (null ss)
    (princ "\n Nothing selected.\n")
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (setq res (DQC:check-text-rules ename
                    (list
                      (list (list "LBS" "LB") (list "KG") 0.453592 "LB" "KG" 1 2)
                      (list (list "FT-LB" "FT LB" "FT.LB" "FTLB")
                            (list "N-M" "N.M" "NM" "N M") 1.355818 "FT-LB" "N-M" 1 2)
                      (list (list "IN-LB" "IN.LB" "IN LB" "INLB")
                            (list "N-M" "N.M" "NM" "N M") 0.112985 "IN-LB" "N-M" 0 2)
                      (list (list "LB-IN/RAD" "LB IN/RAD" "LBIN/RAD")
                            (list "N-M/RAD" "NM/RAD") 0.112985 "LB-IN/RAD" "N-M/RAD" 1 2)
                      (list (list "LB/IN") (list "N/MM") 0.175127 "LB/IN" "N/MM" 1 2)
                      (list (list "PSI") (list "KPA") 6.89476 "PSI" "kPa" 0 1))
                    total pass fail))
        (setq total (car res) pass (cadr res) fail (caddr res))
        (setq res (DQC:check-inline-mm ename total pass fail))
        (setq total (car res) pass (cadr res) fail (caddr res))
        (setq i (1+ i)))))
  (list total pass fail 0))


;;; ============================================================================
;;;  PART 11 - MODE 3: DIMENSIONS CHECK (all entities, inch [mm] only)
;;; ============================================================================

(defun DQC:process-dim (ename doc / obj ed etype entlay
                              meas sname lfac primary-auto is-dim
                              ts to g1 flags70 alt-on dimaltf
                              pair stripped raw from-text from-meas-sub
                              pfx in-dp mm-dp style-dp bracket-pos inch-seg
                              primary alt expected ok label layer
                              in-tol mm-tol tol-str txtpt txth dimang)
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
              (cond
                ((null pair) (list 'SKIP "" nil nil nil raw))
                ((eq (cadr pair) 'EMPTY)
                 (setq primary  (car pair)
                       expected (* (abs primary) DQC:MM/IN)
                       label    (DQC:fail-label
                                  (strcat "\\U+2717 " pfx (DQC:fmt primary in-dp)
                                          " [?] exp " (DQC:fmt expected mm-dp) " mm" tol-str))
                       layer    DQC:FAIL-LAYER)
                 (if txtpt (DQC:place-balloon txtpt txth dimang label layer))
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
                 (if txtpt (DQC:place-balloon txtpt txth dimang label layer))
                 (list (if ok 'PASS 'FAIL) label primary alt expected raw)))))))))

;;; Process inline N [mm] in a single MTEXT/TEXT entity
(defun DQC:check-inline-mm (ename total pass fail / obj ed etype txt stripped
                                   hits hit pv av expected ok label layer txtpt txth)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))
      (setq etype (if ed (strcase (cdr (assoc 0 ed))) "?"))
      (if (and (wcmatch etype "MTEXT,TEXT") (not (DQC:on-qc-layer? ename)))
        (progn
          (setq txt (DQC:get-text ename))
          (setq stripped (DQC:strip txt 0.0 0.0))
          (setq hits (DQC:scan-inline-dims stripped))
          (setq txtpt (DQC:dim-textpt ename))
          (setq txth  (DQC:dim-txth  ename))
          (foreach hit hits
            (setq pv (atof (car hit)) av (atof (cadr hit)))
            (if (and (> pv 0.0) (> av 0.0))
              (progn
                (setq expected (* (abs pv) DQC:MM/IN)
                      ok (DQC:ok? pv av DQC:MM/IN)
                      total (1+ total))
                (if ok
                  (setq pass (1+ pass) label (DQC:pass-label) layer DQC:PASS-LAYER)
                  (setq fail (1+ fail)
                        label (DQC:fail-label
                                (strcat "\\U+2717 " (car hit) " [" (cadr hit)
                                        "] exp " (rtos expected 2 2) " mm"))
                        layer DQC:FAIL-LAYER))
                (if txtpt (DQC:place-balloon txtpt txth 0.0 label layer)))))))))
  (list total pass fail))

(defun DQC:run-dims (doc / ss len i ename res total pass fail skip)
  (setq total 0 pass 0 fail 0 skip 0)
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss)
    (princ "\n No dimension entities found.\n")
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i)
              res   (DQC:process-dim ename doc)
              total (1+ total))
        (cond
          ((= (car res) 'PASS) (setq pass (1+ pass)))
          ((= (car res) 'FAIL) (setq fail (1+ fail)))
          (T (setq skip (1+ skip))))
        (setq res (DQC:check-inline-mm ename 0 0 0))
        (setq pass (+ pass (cadr res)) fail (+ fail (caddr res)))
        (setq i (1+ i)))))
  (list total pass fail skip))


;;; ============================================================================
;;;  PART 12 - MODE 4: NOTES CHECK
;;;  User selects notes. Checks weight (LB/KG) and inch [mm].
;;; ============================================================================

(defun DQC:run-notes (doc / ss len i ename res total pass fail)
  (setq total 0 pass 0 fail 0)
  (princ "\n Select Notes text (pick objects, then ENTER):\n")
  (setq ss (ssget '((0 . "MTEXT,TEXT"))))
  (if (null ss)
    (princ "\n Nothing selected.\n")
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (setq res (DQC:check-text-rules ename
                    (list
                      (list (list "LBS" "LB") (list "KG") 0.453592 "LB" "KG" 1 2))
                    total pass fail))
        (setq total (car res) pass (cadr res) fail (caddr res))
        (setq res (DQC:check-inline-mm ename total pass fail))
        (setq total (car res) pass (cadr res) fail (caddr res))
        (setq i (1+ i)))))
  (list total pass fail 0))


;;; ============================================================================
;;;  PART 13 - RESET
;;; ============================================================================

(defun DQC:erase-balloons (doc / ss len i)
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (setq ss (ssget "X" (list (cons 0 "MTEXT") (cons 8 lname))))
    (if ss
      (progn
        (setq len (sslength ss) i 0)
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
      (progn
        (setq len (sslength ss) i 0)
        (while (< i len)
          (vl-catch-all-apply 'entdel (list (ssname ss i)))
          (setq n (1+ n) i (1+ i))))))
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (DQC:purge-layer lname doc))
  (vla-Regen doc acAllViewports)
  (princ (strcat "\n Removed " (itoa n) " mark(s) and deleted QC layers.\n"))
  (princ))


;;; ============================================================================
;;;  PART 14 - MAIN COMMAND C:DIMQC
;;; ============================================================================

(defun C:DIMQC ( / doc mode result total pass fail skip sumstr)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (DQC:ensure-layer DQC:PASS-LAYER DQC:PASS-COLOR doc)
  (DQC:ensure-layer DQC:FAIL-LAYER DQC:FAIL-COLOR doc)
  (DQC:erase-balloons doc)

  (princ "\n============================================")
  (princ "\n  DIM QC v7.0 - Select Check Mode")
  (princ "\n============================================")
  (princ "\n  1. Operating Conditions Check")
  (princ "\n  2. MED Check")
  (princ "\n  3. Dimensions Check")
  (princ "\n  4. Notes Check")
  (princ "\n============================================")
  (setq mode (getint "\n Enter mode number (1-4): "))

  (if (or (null mode) (< mode 1) (> mode 4))
    (progn (princ "\n Invalid selection. Cancelled.\n") (princ) (exit)))

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
     (setq result (DQC:run-notes doc))))

  (setq total (car result)
        pass  (cadr result)
        fail  (caddr result)
        skip  (cadddr result))

  (vla-Regen doc acAllViewports)
  (setq sumstr (strcat "Checked: " (itoa (+ pass fail))
                       "   PASS: " (itoa pass)
                       "   FAIL: " (itoa fail)
                       "   Skipped: " (itoa skip)))
  (princ (strcat "\n " sumstr "\n"))
  (princ))


;;; ============================================================================
;;;  PART 15 - DIMQC-DIAG
;;; ============================================================================

(defun C:DIMQC-DIAG ( / doc ss len i ename obj ed etype sname lfac is-dim
                         meas raw stripped pair primary alt expected
                         ts to g alt-on dimaltf flags70)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n========== DIMQC DIAGNOSTIC v7.0 ==========\n")
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss) (progn (princ " No entities found.\n\n") (princ) (exit)))
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
    (setq alt-on (vl-catch-all-apply 'vlax-get (list obj 'AlternateUnits)))
    (if (vl-catch-all-error-p alt-on) (setq alt-on nil))
    (if (and (null alt-on) ed)
      (progn
        (setq flags70 (cdr (assoc 70 ed)))
        (if (and flags70 (= (logand flags70 2) 2)) (setq alt-on :vlax-true))))
    (setq dimaltf (if ed (cdr (assoc 143 ed)) nil))
    (if (or (null dimaltf) (zerop dimaltf)) (setq dimaltf DQC:MM/IN))
    (setq pair nil raw "" stripped "")
    (if (and (null pair) (> (strlen (DQC:trim ts)) 0))
      (progn
        (setq stripped (DQC:strip ts (* (abs meas) lfac) (* (* (abs meas) lfac) DQC:MM/IN)))
        (setq raw (strcat "TextString: " stripped))
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
    (princ (strcat "  IsDim  : " (if is-dim "YES" "NO") "\n"))
    (princ (strcat "  Style  : " (if sname sname "?")
                   "  DIMDEC: " (vl-princ-to-string (DQC:dimdec sname doc)) "\n"))
    (princ (strcat "  TxtStr : \"" ts "\"\n"))
    (princ (strcat "  TxtOvr : \"" to "\"\n"))
    (princ (strcat "  DXF1   : \"" (if (= (type g) 'STR) g "") "\"\n"))
    (princ (strcat "  Raw    : \"" raw "\"\n"))
    (princ (strcat "  Meas   : " (rtos meas 2 6) "\n"))
    (princ
      (strcat "  Parse  : "
        (cond
          ((null pair) "no [ ] bracket -> SKIP")
          ((eq (cadr pair) 'EMPTY)
           (strcat "primary=" (rtos (car pair) 2 6)
                   "  mm=EMPTY  exp="
                   (rtos (* (abs (car pair)) DQC:MM/IN) 2 4) " mm"))
          (T
           (setq primary (car pair) alt (cadr pair)
                 expected (* (abs primary) DQC:MM/IN))
           (strcat "primary=" (rtos primary 2 6)
                   "  mm=" (rtos alt 2 4)
                   "  exp=" (rtos expected 2 4)
                   "  diff=" (rtos (abs (- expected alt)) 2 5)
                   (if (DQC:ok? primary alt DQC:MM/IN) "  -> PASS" "  -> FAIL"))))
        "\n\n"))
    (setq i (1+ i))
    (if (= (rem i 20) 0) (getstring " --- ENTER for next batch --- ")))
  (princ "========== END ==========\n\n")
  (princ)))


;;; ============================================================================
;;;  LOAD MESSAGE
;;; ============================================================================
(princ "\n================================================\n")
(princ " DIM QC v7.0 Loaded.\n")
(princ "   DIMQC        Mode menu:\n")
(princ "     1 = Operating Conditions (HP/kW, IN-LB/N-M) - select text\n")
(princ "     2 = MED Check (LB/KG, torque, stiffness, PSI) - select text\n")
(princ "     3 = Dimensions Check (inch [mm]) - all entities\n")
(princ "     4 = Notes Check (LB/KG + inch/mm) - select text\n")
(princ "   DIMQC-RESET  Remove marks + delete QC layers\n")
(princ "   DIMQC-DIAG   Command-line diagnostic\n")
(princ "================================================\n\n")
(princ)
