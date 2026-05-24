;;; ============================================================================
;;;  dim_qc_v17-2.lsp  -  Engineering Dual-Unit QC
;;;  Version 17.2
;;;
;;;  COMMANDS
;;;    DIMQC        Mode selection menu
;;;    DIMQC-RESET  Erase all marks AND delete QC layers
;;;    DIMQC-DIAG   Command-line diagnostic for dimensions
;;;
;;;  MODES
;;;    1. Operating Conditions  - user selects text  (HP/kW, IN-LB/N-M)
;;;       Values displayed as integers (no decimal places)
;;;    2. MED Check             - user selects text  (LB/KG, torque, stiffness)
;;;    3. Dimensions Check      - all entities scanned
;;;         inch [mm], FT-LB [N-M], LB [KG], block attributes
;;;         Tolerance validation, inch-only errors, bracket-only detection
;;;         Redundant metric orphan scan, double-tick prevention
;;;    4. Notes Check           - user selects notes (LB/KG + inch/mm)
;;;
;;;  v17.2 CHANGES
;;;    - BLOCK CHANGE GO HERE list: KF and similar blocks are ignored
;;;    - OP conditions (Mode 1): values displayed as integers
;;;    - Mode 2 & 3: LB-IN compound (lb-in2 moment of inertia) is skipped
;;;    - Mode 3: FT-LB [N-m] torque check (not IN-LB, avoids Mode 1 overlap)
;;;    - Mode 3: LB [KG] weight check in text entities
;;;    - Mode 3: INSERT/block attribute scanning for inch->mm
;;;    - Mode 3: Inch-only DIMENSION entity -> ? error (no metric bracket)
;;;    - Mode 3: Bracket-only [mm] with no inch -> ?? error
;;;    - Mode 3: Redundant metric unit orphan detection
;;;    - Mode 3: Tolerance values validated (not just nominal)
;;;    - Mode 3: Double-tick prevention (inline check skipped if already marked)
;;;    - Mode 3: FT-LB range rounding: mm must round to 2 dp
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

;;; Style cache: avoids repeated vla-get-DimStyles + vla-item calls per entity.
;;; Reset at the start of each DIMQC run.
(setq DQC:STYLE-LFAC-CACHE nil)   ; assoc list: (sname . lfac-value)
(setq DQC:STYLE-DEC-CACHE  nil)   ; assoc list: (sname . dec-value)

;;; v17 mark placement controls
(setq DQC:MARK-LEADER T)
(setq DQC:MARK-ANCHOR T)
(setq DQC:ANCHOR-RATIO 0.12)
(setq DQC:LEADER-COLOR 8)

;;; ============================================================================
;;; BLOCK CHANGE GO HERE
;;; Edit this list to add/remove block names that all DIMQC modes should skip.
;;; Names are matched case-insensitively.
(setq DQC:SKIP-BLOCK-NAMES (list "KF"))
;;; ============================================================================


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

;;; Legacy tolerance for Mode 3 DIMENSION entities: +-0.5 mm absolute
(defun DQC:ok? (primary alt factor / ex df)
  (if (< (abs primary) 1e-9)
    (< (abs alt) 0.5)
    (progn
      (setq ex (* (abs primary) factor)
            df (abs (- ex (abs alt))))
      (<= df 0.5))))

;;; Tight tolerance for OP/MED: pass only if converted value rounds correctly
(defun DQC:ok-abs? (primary alt factor tol / ex df)
  (if (< (abs primary) 1e-9)
    (< (abs alt) tol)
    (progn
      (setq ex (* (abs primary) factor)
            df (abs (- ex (abs alt))))
      (<= df tol))))

;;; Return one-unit-of-last-place tolerance for a numeric string.
;;; "3.18" -> 2 dp -> 0.01 ; "3.2" -> 1 dp -> 0.1 ; "318" -> 0 dp -> 1.0
(defun DQC:rounding-tol (s / i dot cnt ch tol)
  (setq i 1 dot nil cnt 0)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (if (= ch ".") (setq dot T))
    (if (and dot (/= ch ".") (wcmatch ch "#")) (setq cnt (1+ cnt)))
    (setq i (1+ i)))
  (setq tol 1.0)
  (repeat cnt (setq tol (/ tol 10.0)))
  tol)

;;; Accurate X-offset to hit-pos in a note string using AutoLISP textbox.
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

(defun DQC:dimdec (sname doc / styles sobj dec live hit)
  (setq live (fix (getvar "DIMDEC")))
  (if (null sname) live
    (progn
      ;; Cache hit: return previously computed value
      (setq hit (assoc sname DQC:STYLE-DEC-CACHE))
      (if hit
        (cdr hit)
        (progn
          (setq styles (vl-catch-all-apply 'vla-get-DimStyles (list doc)))
          (if (vl-catch-all-error-p styles) live
            (progn
              (setq sobj (vl-catch-all-apply 'vla-item (list styles sname)))
              (if (vl-catch-all-error-p sobj) live
                (progn
                  (setq dec (vl-catch-all-apply 'vlax-get (list sobj 'PrimaryUnitsPrecision)))
                  (setq dec (if (or (vl-catch-all-error-p dec) (null dec)) live (fix dec)))
                  (setq DQC:STYLE-DEC-CACHE
                        (cons (cons sname dec) DQC:STYLE-DEC-CACHE))
                  dec)))))))))

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

;;; Check if block name is in the global skip list (case-insensitive)
(defun DQC:skip-block? (bname)
  (if (or (null bname) (= (strlen bname) 0)) nil
    (member (strcase bname) (mapcar 'strcase DQC:SKIP-BLOCK-NAMES))))

;;; Detect if "LB" at kw-pos (1-based) in uppercase string su is part of
;;; a compound unit LB-IN or LB-FT (moment of inertia) - skip as weight.
(defun DQC:lbin-compound? (su kw-pos kw-len / tail)
  ;; Extract from the keyword start position to see full compound (e.g. "LB-IN")
  (setq tail (if (<= kw-pos (strlen su))
               (substr su kw-pos (min 6 (- (strlen su) kw-pos -1)))
               ""))
  (or (wcmatch tail "LB-IN*")
      (wcmatch tail "LB-FT*")
      (wcmatch tail "LB IN*")
      (wcmatch tail "LB FT*")))

;;; Extract numeric value from a tolerance string like "+0.005" or "-0.127"
(defun DQC:tol-val (s)
  (if (null s) 0.0
    (progn
      (setq s (DQC:trim s))
      (if (and (> (strlen s) 0)
               (or (= (substr s 1 1) "+") (= (substr s 1 1) "-")))
        (setq s (substr s 2)))
      (DQC:atof-safe s))))

;;; Scan stripped text for metric unit keywords OUTSIDE of brackets.
;;; Returns T if orphan metric units found (potential missing bracket warning).
(defun DQC:has-orphan-metric? (txt / su i ch depth found)
  (setq su (strcase txt) i 1 depth 0 found nil)
  (while (and (<= i (strlen su)) (not found))
    (setq ch (substr su i 1))
    (cond
      ((= ch "[") (setq depth (1+ depth) i (1+ i)))
      ((= ch "]") (setq depth (max 0 (1- depth)) i (1+ i)))
      ((> depth 0) (setq i (1+ i)))
      (T
       ;; Outside brackets - check for standalone metric keywords
       (cond
         ((and (<= (+ i 1) (strlen su)) (= (substr su i 2) "MM")
               (or (= i 1) (not (wcmatch (substr su (1- i) 1) "#@")))
               (or (> (+ i 2) (strlen su)) (not (wcmatch (substr su (+ i 2) 1) "#@"))))
          (setq found T))
         ((and (<= (+ i 2) (strlen su))
               (or (= (substr su i 3) "N-M") (= (substr su i 3) "NM "))
               (or (= i 1) (not (wcmatch (substr su (1- i) 1) "@"))))
          (setq found T))
         ((and (<= (+ i 1) (strlen su)) (= (substr su i 2) "KG")
               (or (= i 1) (not (wcmatch (substr su (1- i) 1) "@")))
               (or (> (+ i 2) (strlen su)) (not (wcmatch (substr su (+ i 2) 1) "@"))))
          (setq found T))
         ((and (<= (+ i 2) (strlen su)) (= (substr su i 3) "KPA")
               (or (= i 1) (not (wcmatch (substr su (1- i) 1) "@"))))
          (setq found T))
         (T (setq i (1+ i)))))))
  found)


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
;;;  RANGE PARSER - handles "a/b [c/d]" style dimensions
;;; ============================================================================

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
          (if (or (= (strlen left) 0) (= (strlen right) 0)) nil
            (progn
              (setq lv (DQC:atof-safe left))
              (setq rv (DQC:atof-safe right))
              (if (and (> lv 0.0) (> rv 0.0))
                (list lv rv)
                nil))))))))

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

(defun DQC:scan-inline-dims (txt / results i len ch ns in-n dot j k as closed ns2 dot2 full-inch)
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
          ;; Case B: optional spaces/tolerance text then [ - handles "N [M]" and "N+t/-t [M+t/-t]"
          (T
           (while (and (<= k len) (= (substr txt k 1) " ")) (setq k (1+ k)))
           ;; Skip tolerance characters (+  - . digits /) that may appear between number and [
           (while (and (<= k len)
                       (wcmatch (substr txt k 1) "+,-,#,.,/"))
             (setq k (1+ k)))
           (while (and (<= k len) (= (substr txt k 1) " ")) (setq k (1+ k)))
           (if (and (<= k len) (= (substr txt k 1) "["))
             (progn
               ;; full inch-side text: number + any tolerance notation before [
               (setq full-inch (DQC:trim (substr txt i (- k i))))
               (setq k (1+ k) as "" closed nil)
               (while (and (<= k len) (not closed))
                 (setq ch (substr txt k 1))
                 (if (= ch "]") (setq closed T)
                   (setq as (strcat as ch) k (1+ k))))
               (if closed
                 (progn
                   (setq results (append results (list (list ns as i nil full-inch))))
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

(defun DQC:lfac (sname doc / styles sobj lf hit)
  (if (null sname) 1.0
    (progn
      ;; Cache hit: return previously computed value
      (setq hit (assoc sname DQC:STYLE-LFAC-CACHE))
      (if hit
        (cdr hit)
        (progn
          (setq styles (vl-catch-all-apply 'vla-get-DimStyles (list doc)))
          (if (vl-catch-all-error-p styles) 1.0
            (progn
              (setq sobj (vl-catch-all-apply 'vla-item (list styles sname)))
              (if (vl-catch-all-error-p sobj) 1.0
                (progn
                  (setq lf (vl-catch-all-apply 'vla-get-LinearScaleFactor (list sobj)))
                  (setq lf (if (or (vl-catch-all-error-p lf) (null lf) (zerop lf))
                             1.0 (abs lf)))
                  (setq DQC:STYLE-LFAC-CACHE
                        (cons (cons sname lf) DQC:STYLE-LFAC-CACHE))
                  lf)))))))))

(defun DQC:has-meas-token (s)
  (if (vl-string-search "<>" s) T nil))

(defun DQC:dist2d (p1 p2 / dx dy)
  (setq dx (- (car p1) (car p2))
        dy (- (cadr p1) (cadr p2)))
  (sqrt (+ (* dx dx) (* dy dy))))

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
  (if (null txtpt) nil
    (progn
      (setq bh (if (and DQC:TXT-H (> DQC:TXT-H 0)) DQC:TXT-H (* txth 0.85)))
      (if (< bh 0.5) (setq bh 0.5))
      (setq offx (if (and DQC:OFFSET (> DQC:OFFSET 0)) DQC:OFFSET (* bh 0.35))
            offy (* bh 0.32))
      (setq ovec (DQC:rot2 offx offy (if dimang dimang 0.0)))
      (setq ins (list (+ (car txtpt) (car ovec))
                      (+ (cadr txtpt) (cadr ovec))
                      (if (caddr txtpt) (caddr txtpt) 0.0)))
      (setq color (if (= layer DQC:FAIL-LAYER) DQC:FAIL-COLOR DQC:PASS-COLOR))
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
                  (cons 40 anchor-rad)))
          (if DQC:MARK-LEADER
            (entmake
              (list (cons 0 "LINE")
                    (cons 100 "AcDbEntity")
                    (cons 8 layer)
                    (cons 62 color)
                    (cons 100 "AcDbLine")
                    (cons 10 txtpt)
                    (cons 11 ins))))))
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

(defun DQC:kw-find (su kw from / p)
  (setq p (vl-string-search kw su (1- from)))
  (if p (1+ p) 0))

;;; Read number backward from kw-pos (1-based) in string s.
(defun DQC:num-before-kw (s kw-pos / i ns dot sci-val segment)
  (setq segment (substr s 1 (1- kw-pos)))
  (setq sci-val (DQC:extract-scientific segment))
  (if sci-val
    (list sci-val)
    (progn
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

;;; Extract coefficient from scientific notation like "8.6 X 10^6 <UNIT>"
(defun DQC:extract-scientific (s / su x-pos caret-pos base-str base)
  (setq su (strcase s))
  (setq x-pos (vl-string-search "X 1" su 0))
  (if (null x-pos)
    (setq x-pos (vl-string-search "X1" su 0)))
  (if (null x-pos) nil
    (progn
      (setq caret-pos (vl-string-search "^" su x-pos))
      (if (null caret-pos) nil
        (progn
          (setq base-str (substr s 1 x-pos))
          (setq base (DQC:trailing-number-in-seg base-str))
          base)))))

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

(defun DQC:number-only-text (s / v)
  (setq s (DQC:trim s))
  (if (and (> (strlen s) 0)
           (not (wcmatch (strcase s) "*[A-Z]*")))
    (progn
      (setq v (DQC:atof-safe s))
      (if (> v 0.0) v nil))
    nil))

(defun DQC:unit-text-has? (txt kw-list / su found)
  (setq su (strcase txt) found nil)
  (foreach kw kw-list
    (if (and (not found)
             (vl-string-search kw su))
      (setq found T)))
  found)

;;; Find all 1-based positions of keyword kw in uppercase string su.
(defun DQC:kw-all-positions (su kw / positions p start)
  (setq positions nil start 0)
  (while (setq p (vl-string-search kw su start))
    (setq positions (append positions (list (1+ p))))
    (setq start (+ p 1)))
  positions)

;;; Find primary-unit hits in string s.
;;; Returns list of (prim-val kw-pos kw-len kw).
;;; Skips LB when it is part of a LB-IN or LB-FT compound unit.
(defun DQC:find-prim-hits (s kw-list / su results kw hits pos v right-ok right-char kw-end)
  (setq su (strcase s) results nil)
  (foreach kw kw-list
    (setq hits (DQC:kw-all-positions su kw))
    (foreach pos hits
      (if (or (= pos 1)
              (not (wcmatch (substr su (1- pos) 1) "@,#")))
        (progn
          (setq kw-end (+ pos (strlen kw)))
          (setq right-ok T)
          (if (<= kw-end (strlen su))
            (progn
              (setq right-char (substr su kw-end 1))
              (if (wcmatch right-char "@,#")
                (setq right-ok nil))))
          ;; Skip "LB" when it is part of LB-IN or LB-FT (moment of inertia)
          (if (and right-ok (= kw "LB") (DQC:lbin-compound? su pos (strlen kw)))
            (setq right-ok nil))
          ;; Skip IN-LB / INLB when followed by "/" - it is a stiffness unit (IN-LB/RAD), not torque
          (if (and right-ok
                   (or (= kw "IN-LB") (= kw "IN.LB") (= kw "IN LB") (= kw "INLB"))
                   (<= (+ pos (strlen kw)) (strlen su))
                   (= (substr su (+ pos (strlen kw)) 1) "/"))
            (setq right-ok nil))
          (if right-ok
            (progn
              (setq v (DQC:num-before-kw s pos))
              (if v
                (setq results
                      (append results
                              (list (list (car v) pos (strlen kw) kw)))))))))))
  results)

;;; Find first alt-unit value in a string from from-pos.
(defun DQC:find-alt-in-string (s alt-list from-pos / su brk-pos close-pos
                                     content after-close window alt-p nm alt-val)
  (setq su (strcase s) alt-val nil)
  (if (> from-pos (strlen s)) nil
    (progn
      (setq brk-pos (vl-string-search "[" s (max 0 (1- from-pos))))
      (if (null brk-pos) nil
        (progn
          (setq close-pos (vl-string-search "]" s (1+ brk-pos)))
          (if (null close-pos) nil
            (progn
              (setq content (substr s (+ brk-pos 2) (- close-pos brk-pos 1)))
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

(defun DQC:parse-num-from-content (s / i ns dot ch sci-val)
  (setq sci-val (DQC:extract-scientific s))
  (if sci-val
    sci-val
    (progn
      (setq i 1 ns "" dot nil)
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

(defun DQC:extract-bracket-number (s / brk close content)
  (setq brk (vl-string-search "[" s 0))
  (if (null brk) nil
    (progn
      (setq close (vl-string-search "]" s (1+ brk)))
      (if (null close) nil
        (progn
          (setq content (substr s (+ brk 2) (- close brk 1)))
          (DQC:parse-num-from-content content))))))


;;; ============================================================================
;;;  PART 9 - CROSS-ENTITY UNIT-PAIR MATCHER
;;;
;;;  Rule tuple (9 elements):
;;;    (kw-list alt-list factor prim-label alt-label dp-p dp-a tol no-miss-mark)
;;;  no-miss-mark (9th, optional): when T, silently skip if no alt found.
;;;    Used for Mode 3 FT-LB/LB checks where "no bracket = no tick" is correct.
;;; ============================================================================

;;; Return the 0-based position of the first match of any keyword in su (uppercase).
;;; Returns 0 if none found (safe default for num-before-kw call).
(defun DQC:kw-first-pos (su kw-list / kw p best)
  (setq best nil)
  (foreach kw kw-list
    (setq p (vl-string-search kw su 0))
    (if (and p (or (null best) (< p best)))
      (setq best p)))
  (if best best 0))

;;; ============================================================================
;;;  TOLERANCE HELPERS
;;; ============================================================================

;;; Parse +H/-L tolerance from the start of string s (skips leading spaces/newlines).
;;; Handles: "+H/-L", "+H\n-L", "+H" (symmetric hi=lo), "%%PH" (AutoCAD ±).
;;; Returns (hi-val lo-val) as positive numbers, or nil.
(defun DQC:tol-from-str (s / i su ns hi lo)
  (setq s (DQC:trim s) i 1)
  ;; Skip leading whitespace and newlines
  (while (and (<= i (strlen s))
              (member (substr s i 1) (list " " "\n" "\t")))
    (setq i (1+ i)))
  (if (> i (strlen s)) nil
    (progn
      (setq su (substr s i))
      (cond
        ;; %%P (AutoCAD ± code) followed by numeric value
        ((and (>= (strlen su) 4) (= (strcase (substr su 1 3)) "%%P"))
         (setq ns (DQC:trim (substr su 4)))
         (setq hi (DQC:atof-safe ns))
         (if (> hi 0.0) (list hi hi) nil))
        ;; +H then optional separator then optional -L
        ((= (substr su 1 1) "+")
         (setq i 2 ns "")
         (while (and (<= i (strlen su))
                     (or (wcmatch (substr su i 1) "#")
                         (= (substr su i 1) ".")
                         (= (substr su i 1) ",")))
           (setq ns (strcat ns (substr su i 1)) i (1+ i)))
         (setq hi (if (> (strlen ns) 0) (DQC:atof-safe ns) nil))
         ;; Skip separator: /, space, newline
         (while (and (<= i (strlen su))
                     (member (substr su i 1) (list "/" " " "\n" "\t")))
           (setq i (1+ i)))
         (if (and (<= i (strlen su)) (= (substr su i 1) "-"))
           (progn
             (setq i (1+ i) ns "")
             (while (and (<= i (strlen su))
                         (or (wcmatch (substr su i 1) "#")
                             (= (substr su i 1) ".")
                             (= (substr su i 1) ",")))
               (setq ns (strcat ns (substr su i 1)) i (1+ i)))
             (setq lo (if (> (strlen ns) 0) (DQC:atof-safe ns) nil)))
           (setq lo nil))
         (if hi (list hi (if lo lo hi)) nil))
        (T nil)))))

;;; Extract tolerance from entity text starting at 1-based position kw-end.
;;; (kw-end is the index of the first char AFTER the keyword.)
;;; Returns (hi lo) or nil.
(defun DQC:tol-after-kw (txt kw-end / rest)
  (if (or (null txt) (null kw-end) (>= kw-end (strlen txt))) nil
    (DQC:tol-from-str (substr txt (1+ kw-end)))))

;;; Extract tolerance from an entity text that contains [value unit].
;;; Looks inside the bracket for "+H/-L" after the numeric value,
;;; then falls back to text after the closing bracket (e.g. next MTEXT line).
;;; Returns (hi lo) or nil.
(defun DQC:tol-from-bracket-txt (txt / bpos cpos content p-plus _t)
  (setq bpos (vl-string-search "[" txt 0))
  (if (null bpos) nil
    (progn
      (setq cpos (vl-string-search "]" txt (1+ bpos)))
      (if (null cpos) nil
        (progn
          (setq content (substr txt (+ bpos 2) (- cpos bpos 1)))
          ;; Check inside bracket: scan for "+" after the leading number
          (setq p-plus (vl-string-search "+" content 0))
          (setq _t (if p-plus (DQC:tol-from-str (substr content (1+ p-plus))) nil))
          (if _t _t
            ;; Fallback: tolerance on the line(s) after the bracket
            (if (< (1+ cpos) (strlen txt))
              (DQC:tol-from-str (substr txt (+ cpos 2)))
              nil)))))))

(defun DQC:match-cross-entity (ent-info-list rules
                               / total pass fail rec ename txt pt th
                                 rule kwl altl fac pl al dp-p dp-a tol no-miss
                                 hits pv alt-val expected ok nom-ok label layer
                                 unit-rec unit-pt unit-txt
                                 alt-rec alt-pt alt-txt
                                 dx dy dx2 best-dx
                                 nb-rec nb-txt nb-pt nb-dy nb-dx
                                 nb-best-dx _nb-alt
                                 kw-end alt-src-txt
                                 prim-tol alt-tol tol-ok tol-str thi-exp tlo-exp)
  (setq total 0 pass 0 fail 0)

  (foreach rec ent-info-list
    (setq ename (nth 0 rec)
          txt   (nth 1 rec)
          pt    (nth 2 rec)
          th    (nth 3 rec))

    (foreach rule rules
      (setq kwl     (nth 0 rule)
            altl    (nth 1 rule)
            fac     (nth 2 rule)
            pl      (nth 3 rule)
            al      (nth 4 rule)
            dp-p    (nth 5 rule)
            dp-a    (nth 6 rule)
            tol     (if (nth 7 rule) (nth 7 rule) 0.5)
            no-miss (if (nth 8 rule) (nth 8 rule) nil)
            alt-val nil)

      (setq hits (DQC:find-prim-hits txt kwl))

      ;; Split-number case: pure-number text with unit in nearby entity
      (if (and (null hits) pt (DQC:number-only-text txt))
        (progn
          (setq pv (DQC:number-only-text txt))
          (setq best-dx 1e99 hits nil)
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
                    (setq alt-val (DQC:find-alt-in-string unit-txt altl 1))
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

      (foreach hit hits
        (setq pv          (car hit)
              kw-end      (+ (nth 1 hit) (nth 2 hit))  ; 1-based index after keyword
              alt-src-txt nil
              alt-val     nil)
        ;; Try to find alt in the same entity first
        (if (DQC:find-alt-in-string txt altl 1)
          (setq alt-val     (DQC:find-alt-in-string txt altl 1)
                alt-src-txt txt))
        ;; Cross-entity: any direction, Manhattan distance, dy<=8*th, dx<=50*th.
        ;; Also tries without brackets (fallback) for "67.8 N-m" style text.
        (if (and (null alt-val) pt)
          (progn
            (setq nb-best-dx 1e99)
            (foreach nb-rec ent-info-list
              (setq nb-txt (nth 1 nb-rec)
                    nb-pt  (nth 2 nb-rec))
              (if (and nb-pt (not (eq (nth 0 nb-rec) ename)))
                (progn
                  (setq nb-dy (abs (- (cadr nb-pt) (cadr pt)))
                        nb-dx (abs (- (car nb-pt) (car pt))))
                  (if (and (<= nb-dy (* th 8.0))
                           (<= nb-dx (* th 50.0))
                           (< (+ nb-dy nb-dx) nb-best-dx))
                    (progn
                      (setq _nb-alt (DQC:find-alt-in-string nb-txt altl 1))
                      (if (and (null _nb-alt) (DQC:unit-text-has? nb-txt altl))
                        (setq _nb-alt (DQC:num-before-kw nb-txt
                                        (1+ (DQC:kw-first-pos (strcase nb-txt) altl)))))
                      (if _nb-alt
                        (setq nb-best-dx (+ nb-dy nb-dx)
                              alt-val    _nb-alt
                              alt-src-txt nb-txt)))))))))

        (if alt-val
          (progn
            (setq expected (* pv fac)
                  nom-ok   (DQC:ok-abs? pv alt-val fac tol)
                  total    (1+ total))

            ;; ── Tolerance check ───────────────────────────────────────────────
            ;; Skip for split-number synthetic hits (kw-len = 0).
            (setq prim-tol nil alt-tol nil tol-ok T tol-str "")
            (if (> (nth 2 hit) 0)
              (progn
                ;; Tolerance in primary text: chars after the keyword
                (setq prim-tol (DQC:tol-after-kw txt kw-end))
                ;; Tolerance in alt text: inside bracket or after closing ]
                (if alt-src-txt
                  (setq alt-tol (DQC:tol-from-bracket-txt alt-src-txt)))
                (if (and prim-tol alt-tol)
                  (progn
                    (setq thi-exp (* (car prim-tol) fac)
                          tlo-exp (* (cadr prim-tol) fac)
                          tol-ok  (and (<= (abs (- thi-exp (car alt-tol)))
                                           (max tol (* thi-exp 0.06)))
                                       (<= (abs (- tlo-exp (cadr alt-tol)))
                                           (max tol (* tlo-exp 0.06)))))
                    ;; Build tolerance annotation for fail label
                    (if (not tol-ok)
                      (setq tol-str
                            (strcat " tol:+"
                                    (rtos (car alt-tol) 2 dp-a) "/-"
                                    (rtos (cadr alt-tol) 2 dp-a)
                                    " exp:+"
                                    (rtos thi-exp 2 dp-a) "/-"
                                    (rtos tlo-exp 2 dp-a))))))))

            (setq ok (and nom-ok tol-ok))
            (if ok
              (setq pass  (1+ pass)
                    label (DQC:pass-label)
                    layer DQC:PASS-LAYER)
              (progn
                (setq fail (1+ fail))
                (if nom-ok
                  ;; Nominal OK but tolerance conversion wrong
                  (setq label (DQC:fail-label
                                (strcat "\\U+2717 "
                                        (rtos pv 2 dp-p) " " pl
                                        " [" (rtos alt-val 2 dp-a) " " al "]"
                                        tol-str))
                        layer DQC:FAIL-LAYER)
                  ;; Nominal value wrong (show expected; append tol info if any)
                  (setq label (DQC:fail-label
                                (strcat "\\U+2717 "
                                        (rtos pv 2 dp-p) " " pl
                                        " [" (rtos alt-val 2 dp-a) " " al
                                        "] exp " (rtos expected 2 dp-a) " " al
                                        tol-str))
                        layer DQC:FAIL-LAYER))))
            (DQC:place-balloon pt th 0.0 label layer))

          ;; Alt not found
          (if (not no-miss)
            (progn
              (setq total (1+ total) fail (1+ fail))
              (DQC:place-balloon pt th 0.0
                (DQC:fail-label
                  (strcat "\\U+2717 " (rtos pv 2 dp-p) " " pl " [?]"))
                DQC:FAIL-LAYER))
            nil)))))

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
          ;; HP -> kW  (integers, tol=5.0 for rounding)
          (list (list "HP") (list "KW") 0.7457 "HP" "kW" 0 0 5.0)
          ;; IN-LB -> N-M  (integers, tol=5.0)
          (list (list "IN-LB" "IN.LB" "IN LB" "INLB")
                (list "N-M" "N.M" "NM" "N M") 0.112985 "IN-LB" "N-M" 0 0 5.0)))
      (setq res (DQC:match-cross-entity ent-info-list rules))
      (list (car res) (cadr res) (caddr res) 0))))


;;; ============================================================================
;;;  PART 11 - MODE 2: MED CHECK
;;; ============================================================================

;;; Cross-entity inch [mm] check for MED
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

(defun DQC:run-med (doc / ss ent-info-list i len ename txt pt th res
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
      ;; Rules ordered most-specific to least-specific to avoid false matches.
      ;; LB-IN compound unit (lb-in2 moment of inertia) is handled by the
      ;; DQC:lbin-compound? check inside DQC:find-prim-hits - "LB" in "LB-IN"
      ;; will not be matched as a weight unit.
      (setq rules
        (list
          ;; Stiffness: LB-IN/RAD -> NM/RAD
          (list (list "<removed-compound-unit>" "LB IN/RAD" "LBIN/RAD" "<removed-compound-unit>")
                (list "<removed-compound-unit>" "NM/RAD" "N.M/RAD") 0.112985 "<removed-compound-unit>" "<removed-compound-unit>" 1 2)
          ;; Angular stiffness
          (list (list "<removed-compound-unit>" "<removed-compound-unit>" "INLB/DEG")
                (list "<removed-compound-unit>" "NM/DEG" "N.M/DEG") 0.112985 "<removed-compound-unit>" "<removed-compound-unit>" 0 2)
          ;; Torque: FT-LB -> N-M
          (list (list "FT-LB" "FT LB" "FT.LB" "FTLB")
                (list "N-M" "N.M" "NM" "N M") 1.355818 "FT-LB" "N-M" 1 2)
          ;; Torque: IN-LB -> N-M
          (list (list "IN-LB" "IN.LB" "IN LB" "INLB")
                (list "N-M" "N.M" "NM" "N M") 0.112985 "IN-LB" "N-M" 0 2)
          ;; Linear stiffness: LB/IN -> N/MM
          (list (list "LB/IN") (list "N/MM") 0.175127 "LB/IN" "N/MM" 1 2)
          ;; Weight: LB -> KG
          (list (list "LBS" "LB") (list "KG") 0.453592 "LB" "KG" 1 2)
          ;; Pressure: PSI -> KPA
          (list (list "PSI") (list "KPA") 6.89476 "PSI" "kPa" 0 1)))
      (setq res (DQC:match-cross-entity ent-info-list rules))
      (setq total (car res) pass (cadr res) fail (caddr res))
      (foreach rec ent-info-list
        (setq r2 (DQC:check-inline-mm-info rec total pass fail))
        (setq total (car r2) pass (cadr r2) fail (caddr r2)))
      (setq r3 (DQC:med-cross-inch-mm ent-info-list))
      (setq total (+ total (car r3))
            pass  (+ pass  (cadr r3))
            fail  (+ fail  (caddr r3)))
      (list total pass fail 0))))


;;; ============================================================================
;;;  PART 12 - MODE 3: DIMENSIONS CHECK
;;; ============================================================================

(defun DQC:process-dim (ename doc / obj ed etype
                              meas sname lfac primary-auto is-dim
                              ts to g1 flags70 alt-on dimaltf
                              pair stripped raw from-text from-meas-sub
                              pfx in-dp mm-dp style-dp bracket-pos inch-seg
                              primary alt expected ok label layer
                              in-tol mm-tol tol-str txtpt txth dimang
                              range-data r-lo r-hi r-mlo r-mhi r-ok-lo r-ok-hi
                              in-hi-v in-lo-v mm-hi-ex mm-lo-ex
                              mm-hi-ac mm-lo-ac tol-hi tol-lo tol-ok)
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
              ;; mm always has one fewer dp than inch (e.g. 3 -> 2, 1 -> 0)
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
              ;; ---- Range dimension check: "a/b [c/d]" ----
              (setq range-data (DQC:parse-range-dim stripped))
              (if range-data
                (progn
                  (setq r-lo   (nth 0 range-data)
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
                  ;; Bracket found but metric value missing/empty
                  ((eq (cadr pair) 'EMPTY)
                   (setq primary  (car pair)
                         expected (* (abs primary) DQC:MM/IN)
                         label    (DQC:fail-label
                                    (strcat "\\U+2717 " pfx (DQC:fmt primary in-dp)
                                            " [?] exp " (DQC:fmt expected mm-dp) " mm" tol-str))
                         layer    DQC:FAIL-LAYER)
                   (DQC:place-balloon txtpt txth dimang label layer)
                   (list 'FAIL label primary nil expected raw))
                  ;; Full pair found - validate nominal AND tolerances
                  (T
                   (setq primary  (car pair)
                         alt      (cadr pair)
                         expected (* (abs primary) DQC:MM/IN)
                         ok       (DQC:ok? primary alt DQC:MM/IN))
                   ;; Tolerance validation: both sides must convert correctly
                   (setq tol-ok T)
                   (if (and ok in-tol mm-tol)
                     (progn
                       (setq in-hi-v  (DQC:tol-val (car in-tol))
                             in-lo-v  (DQC:tol-val (cadr in-tol))
                             mm-hi-ex (* in-hi-v DQC:MM/IN)
                             mm-lo-ex (* in-lo-v DQC:MM/IN)
                             mm-hi-ac (DQC:tol-val (car mm-tol))
                             mm-lo-ac (DQC:tol-val (cadr mm-tol))
                             tol-hi   (max (DQC:rounding-tol (DQC:trim (car mm-tol))) 0.01)
                             tol-lo   (max (DQC:rounding-tol (DQC:trim (cadr mm-tol))) 0.01))
                       (if (not (and (<= (abs (- mm-hi-ex mm-hi-ac)) tol-hi)
                                     (<= (abs (- mm-lo-ex mm-lo-ac)) tol-lo)))
                         (setq tol-ok nil ok nil))))
                   (if ok
                     (setq label (DQC:pass-label) layer DQC:PASS-LAYER)
                     (setq label (DQC:fail-label
                                   (strcat "\\U+2717 " pfx (DQC:fmt primary in-dp)
                                           " [" (DQC:fmt alt mm-dp) "] exp "
                                           (DQC:fmt expected mm-dp) " mm"
                                           (if tol-ok "" " (tol mismatch)")
                                           tol-str))
                           layer DQC:FAIL-LAYER))
                   (DQC:place-balloon txtpt txth dimang label layer)
                   (list (if ok 'PASS 'FAIL) label primary alt expected raw)))))))))))

;;; Inline N [mm] check using an ent-info rec (ename stripped-txt pt th)
(defun DQC:count-newlines-before (s pos / n i)
  (setq n 0 i 1)
  (while (< i pos)
    (if (<= i (strlen s))
      (if (= (substr s i 1) "\n") (setq n (1+ n))))
    (setq i (1+ i)))
  n)

(defun DQC:check-inline-mm-info (rec total pass fail / ename txt pt th
                                       hits hit pv av expected ok label layer
                                       hit-pos hit-pt df
                                       pv1 pv2 av1 av2 ok1 ok2 sla tol1 tol2 ov
                                       inch-full in-tol-i mm-tol-i tol-ok-i
                                       i-hi i-lo m-hi-ex m-lo-ex m-hi-ac m-lo-ac t-hi t-lo)
  (setq ename (nth 0 rec) txt (nth 1 rec) pt (nth 2 rec) th (nth 3 rec))
  (setq hits (DQC:scan-inline-dims txt))
  (foreach hit hits
    (setq hit-pos (nth 2 hit))
    (cond
      ;; Range hit: N1/N2 [M1/M2]
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
      ;; Simple hit: N [M]
      (T
       (setq pv (DQC:atof-safe (car hit)) av (DQC:atof-safe (cadr hit)))
       (if (and (> pv 0.0) (> av 0.0))
         (progn
           (setq expected (* (abs pv) DQC:MM/IN)
                 df (abs (- expected (abs av)))
                 ok (<= df 0.5)
                 total (1+ total))
           ;; Tolerance validation using full inch text captured by scan-inline-dims
           (setq in-tol-i nil mm-tol-i nil tol-ok-i T
                 inch-full (nth 4 hit))
           (if inch-full
             (progn
               (setq in-tol-i (DQC:extract-tol inch-full)
                     mm-tol-i (DQC:extract-tol (cadr hit)))))
           (if (and ok in-tol-i mm-tol-i)
             (progn
               (setq i-hi    (DQC:tol-val (car in-tol-i))
                     i-lo    (DQC:tol-val (cadr in-tol-i))
                     m-hi-ex (* i-hi DQC:MM/IN)
                     m-lo-ex (* i-lo DQC:MM/IN)
                     m-hi-ac (DQC:tol-val (car mm-tol-i))
                     m-lo-ac (DQC:tol-val (cadr mm-tol-i))
                     t-hi    (max (DQC:rounding-tol (DQC:trim (car mm-tol-i))) 0.01)
                     t-lo    (max (DQC:rounding-tol (DQC:trim (cadr mm-tol-i))) 0.01))
               (if (not (and (<= (abs (- m-hi-ex m-hi-ac)) t-hi)
                             (<= (abs (- m-lo-ex m-lo-ac)) t-lo)))
                 (setq tol-ok-i nil ok nil))))
           (if ok
             (setq pass (1+ pass) label (DQC:pass-label) layer DQC:PASS-LAYER)
             (setq fail (1+ fail)
                   label (DQC:fail-label
                           (strcat "\\U+2717 "
                                   (if inch-full inch-full (car hit))
                                   " [" (cadr hit) "] exp "
                                   (rtos expected 2 2) " mm"
                                   (if tol-ok-i "" " (tol mismatch)")))
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

;;; Collect all MTEXT/TEXT entity records (excluding QC layers).
;;; Returns list of (ename txt pt th) records.
;;; FAST: pure DXF entget only — zero vlax COM calls.
;;; List built with cons+reverse (O(n)) not append (O(n^2)).
(defun DQC:collect-text-entities (/ ss len i ename ed etype lay txt pt th result)
  (setq result nil)
  (setq ss (ssget "X" (list (cons 0 "MTEXT,TEXT"))))
  (if ss
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (setq ename (ssname ss i))
        (setq ed (entget ename))
        (if ed
          (progn
            (setq lay (cdr (assoc 8 ed)))
            ;; Skip QC mark layers without a separate function call
            (if (not (or (= (strcase lay) (strcase DQC:PASS-LAYER))
                         (= (strcase lay) (strcase DQC:FAIL-LAYER))))
              (progn
                ;; DXF group 1 = raw text (MTEXT has format codes; DQC:strip handles them)
                (setq txt (DQC:strip (cdr (assoc 1 ed)) 0.0 0.0))
                (if (null txt) (setq txt ""))
                ;; DXF group 10 = insertion/start point
                (setq pt (cdr (assoc 10 ed)))
                ;; DXF group 40 = text height
                (setq th (cdr (assoc 40 ed)))
                (if (or (null th) (< th 0.001)) (setq th 2.5))
                (setq result (cons (list ename txt pt th) result))))))
        (setq i (1+ i)))))
  ;; cons builds in reverse; reverse once at end (O(n) total)
  (reverse result))

;;; Pre-filter an entity list to only those containing kwl or altl keywords.
;;; Reduces O(n^2) scan from n=all-entities to n=relevant-only (typically <20).
;;; Built with cons+reverse to avoid O(n^2) append.
(defun DQC:filter-ent-for-kw (ent-list kwl altl / result rec txt)
  (setq result nil)
  (foreach rec ent-list
    (setq txt (nth 1 rec))
    (if (and txt
             (or (DQC:find-prim-hits txt kwl)
                 (DQC:find-alt-in-string txt altl 1)))
      (setq result (cons rec result))))
  (reverse result))

;;; Mode 3 FT-LB [N-m] torque check.
;;; Takes pre-collected entity list; pre-filters before O(n^2) scan.
;;; no-miss-mark=T: only marks when a [N-m] bracket is found near FT-LB value.
(defun DQC:run-mode3-torque (ent-info-list / filtered res total pass fail)
  (setq filtered
    (DQC:filter-ent-for-kw ent-info-list
      (list "FT-LB" "FT.LB" "FTLB" "FT LB")
      (list "N-M" "N.M" "NM" "N M")))
  (setq res
    (DQC:match-cross-entity
      filtered
      (list
        (list (list "FT-LB" "FT.LB" "FTLB" "FT LB")
              (list "N-M" "N.M" "NM" "N M")
              1.355818 "FT-LB" "N-M" 1 2 0.5 T))))
  (setq total (car res) pass (cadr res) fail (caddr res))
  (list total pass fail))

;;; Mode 3 LB [KG] weight check.
;;; Takes pre-collected entity list; pre-filters before O(n^2) scan.
;;; no-miss-mark=T: silently skip if no [KG] bracket present.
(defun DQC:run-mode3-weight (ent-info-list / filtered res)
  (setq filtered
    (DQC:filter-ent-for-kw ent-info-list
      (list "LBS" "LB")
      (list "KG")))
  (setq res
    (DQC:match-cross-entity
      filtered
      (list
        (list (list "LBS" "LB") (list "KG") 0.453592 "LB" "KG" 1 2 0.5 T))))
  res)

;;; Filter an already-collected ent-info list to only those containing "[".
;;; No ssget — reuses the list built in Pass 2. Pure list traversal, O(n).
(defun DQC:filter-bracket-text (ent-info-list / result rec txt)
  (setq result nil)
  (foreach rec ent-info-list
    (setq txt (nth 1 rec))
    (if (and txt (vl-string-search "[" txt 0))
      (setq result (cons rec result))))
  (reverse result))

;;; Scan ATTRIB entities DIRECTLY — no INSERT walking, no entnext chains.
;;; ssget "X" ATTRIB is a direct type-indexed lookup — fast on any drawing size.
;;; ent-info-list = bracket-text list already filtered by DQC:filter-bracket-text.
;;; Early-exit while replaces foreach; stops once a tight spatial match is found.
(defun DQC:run-block-attrs (doc ent-info-list / ss len i ename ed
                                  attsub attpt attval lay
                                  total pass fail pv-test alt-val expected ok
                                  label layer nb-rec nb-txt nb-pt nb-dy nb-dx
                                  nb-best-dx nb-list th)
  (setq total 0 pass 0 fail 0)
  ;; ATTRIB is a distinct DXF entity type — direct fast lookup, no INSERT needed
  (setq ss (ssget "X" (list (cons 0 "ATTRIB"))))
  (if ss
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (if (= (rem i 20) 0)
          (DQC:show-progress "Pass 3/3: Block attributes" i len))
        (setq ename (ssname ss i))
        (setq ed (vl-catch-all-apply 'entget (list ename)))
        (if (not (vl-catch-all-error-p ed))
          (progn
            ;; Skip QC mark layers
            (setq lay (cdr (assoc 8 ed)))
            (if (not (or (and lay (= (strcase lay) (strcase DQC:PASS-LAYER)))
                         (and lay (= (strcase lay) (strcase DQC:FAIL-LAYER)))))
              (progn
                (setq attsub (DQC:trim (cdr (assoc 1 ed))))
                (setq attpt  (cdr (assoc 10 ed)))
                ;; Text height: DXF group 40, NO vlax
                (setq th (cdr (assoc 40 ed)))
                (if (or (null th) (< th 0.001)) (setq th 2.5))
                (setq attval (DQC:number-only-text attsub))
                ;; Only numeric attributes are candidate inch values
                (if (and attval attpt (> attval 0.0))
                  (progn
                    (setq nb-best-dx 1e99 alt-val nil)
                    ;; while with early-exit: stop once a close match is found
                    (setq nb-list ent-info-list)
                    (while nb-list
                      (setq nb-rec  (car nb-list)
                            nb-list (cdr nb-list))
                      (setq nb-txt (nth 1 nb-rec)
                            nb-pt  (nth 2 nb-rec))
                      (if nb-pt
                        (progn
                          (setq nb-dy (abs (- (cadr nb-pt) (cadr attpt)))
                                nb-dx (abs (- (car nb-pt) (car attpt))))
                          (if (and (<= nb-dy (* th 4.0))
                                   (<= nb-dx (* th 20.0))
                                   (< (+ nb-dy nb-dx) nb-best-dx)
                                   (> (DQC:find-char nb-txt "[" 1) 0))
                            (progn
                              (setq pv-test (DQC:extract-bracket-number nb-txt))
                              (if pv-test
                                (progn
                                  (setq nb-best-dx (+ nb-dy nb-dx)
                                        alt-val pv-test)
                                  ;; Tight match — stop early
                                  (if (and (<= nb-dy th) (<= nb-dx (* th 3.0)))
                                    (setq nb-list nil)))))))))
                    (if alt-val
                      (progn
                        (setq expected (* attval DQC:MM/IN)
                              ok       (<= (abs (- expected (abs alt-val))) 0.5)
                              total    (1+ total))
                        (if ok
                          (setq pass  (1+ pass)
                                label (DQC:pass-label)
                                layer DQC:PASS-LAYER)
                          (setq fail  (1+ fail)
                                label (DQC:fail-label
                                        (strcat "\\U+2717 BLK:" (rtos attval 2 3)
                                                " IN [" (rtos alt-val 2 1)
                                                "] exp " (rtos expected 2 1) " mm"))
                                layer DQC:FAIL-LAYER))
                        (DQC:place-balloon attpt th 0.0 label layer))))))))
        (setq i (1+ i)))))
  (list total pass fail)))

;;; Write a progress line to the AutoCAD status bar AND command line.
;;; cur/tot drive the bar; msg is the stage label.
(defun DQC:show-progress (msg cur tot / pct filled bar)
  (setq pct   (if (> tot 0) (fix (* 100.0 (/ (float cur) tot))) 100)
        filled (fix (/ pct 5))            ; 20-char bar
        bar    "")
  (repeat filled        (setq bar (strcat bar "|")))
  (repeat (- 20 filled) (setq bar (strcat bar ".")))
  (setq bar (strcat "DIMQC [" bar "] " (itoa pct) "% - " msg))
  (grtext -1 bar)
  (princ (strcat "\r  " bar "   ")))

;;; Inline same-entity unit check: finds prim keyword + [alt] in one text string.
;;; No cross-entity scan — FT-LB and [N-m] must be in the same entity text
;;; (works for multi-line MTEXT since \P is stripped to \n in one string).
;;; no-miss mode: if [alt] bracket not found, silently skip (no false positive).
;;; Returns (pass fail).
(defun DQC:check-inline-unit (rec kwl altl factor dp-p dp-a tol /
                               ename txt pt th hits hit pv av
                               expected ok label layer pass fail)
  (setq pass 0 fail 0
        ename (nth 0 rec) txt (nth 1 rec)
        pt    (nth 2 rec) th  (nth 3 rec))
  (setq hits (DQC:find-prim-hits txt kwl))
  (foreach hit hits
    (setq pv (car hit)
          av (DQC:find-alt-in-string txt altl 1))
    (if av
      (progn
        (setq expected (* pv factor)
              ok       (DQC:ok-abs? pv av factor tol))
        (if ok
          (setq pass (1+ pass) label (DQC:pass-label) layer DQC:PASS-LAYER)
          (setq fail (1+ fail)
                label (DQC:fail-label
                        (strcat "\\U+2717 "
                                (rtos pv 2 dp-p) " ["
                                (rtos av 2 dp-a) "] exp "
                                (rtos expected 2 dp-a)))
                layer DQC:FAIL-LAYER))
        (DQC:place-balloon pt th 0.0 label layer))))
  (list pass fail))

;;; Mode 3: inch [mm] + FT-LB [N-m] + LB [KG] + block attrs.
;;; Pass 1: single loop over all entities (same as v17) for inch [mm].
;;; Pass 2: FT-LB and LB cross-entity on pre-filtered text list (tiny n).
;;; Pass 3: block attribute scan (ssget group 66=1, only attrib blocks).
(defun DQC:run-dims (doc / ss len i ename res res-car total pass fail skip
                          txt pt th rec r2
                          text-ents torque-res wt-res blk-res)
  (setq total 0 pass 0 fail 0 skip 0)

  ;; ── Pass 1: inch [mm] — identical to v17, one loop ───────────────────────
  (DQC:show-progress "Pass 1/3: inch [mm]" 0 1)
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss)
    (princ "\n No entities found.\n")
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (if (= (rem i 20) 0)
          (DQC:show-progress "Pass 1/3: inch [mm]" i len))
        (setq ename (ssname ss i))
        (if (not (DQC:on-qc-layer? ename))
          (progn
            (setq res     (DQC:process-dim ename doc)
                  res-car (car res))
            (setq total (1+ total))
            (cond
              ((= res-car 'PASS) (setq pass (1+ pass)))
              ((= res-car 'FAIL) (setq fail (1+ fail)))
              (T                 (setq skip (1+ skip))))
            (setq txt (DQC:strip (DQC:get-text ename) 0.0 0.0)
                  pt  (DQC:dim-textpt ename)
                  th  (DQC:dim-txth ename)
                  rec (list ename txt pt th))
            (if (= res-car 'SKIP)
              (progn
                (setq r2 (DQC:check-inline-mm-info rec 0 0 0))
                (setq pass (+ pass (cadr r2))
                      fail (+ fail (caddr r2)))))))
        (setq i (1+ i)))
      (DQC:show-progress "Pass 1/3: inch [mm]" len len)))

  ;; ── Pass 2: FT-LB [N-m] and LB [KG] cross-entity ────────────────────────
  ;; DQC:collect-text-entities uses pure DXF (no vlax), cons+reverse O(n).
  ;; DQC:filter-ent-for-kw keeps only entities with actual keywords — tiny list.
  ;; DQC:match-cross-entity runs on that tiny list: fast.
  (DQC:show-progress "Pass 2/3: FT-LB / LB-KG" 0 1)
  (setq text-ents (DQC:collect-text-entities))
  (setq torque-res (DQC:run-mode3-torque text-ents))
  (setq pass (+ pass (cadr torque-res)) fail (+ fail (caddr torque-res)))
  (setq wt-res (DQC:run-mode3-weight text-ents))
  (setq pass (+ pass (cadr wt-res)) fail (+ fail (caddr wt-res)))
  (DQC:show-progress "Pass 2/3: FT-LB / LB-KG" 1 1)

  ;; ── Pass 3: block attribute inch→mm ──────────────────────────────────────
  ;; Reuses text-ents from Pass 2 — no second ssget.
  ;; DQC:filter-bracket-text keeps only "["-containing records (tiny list).
  ;; DQC:run-block-attrs gets group 66=1 INSERT only, no vlax in loop.
  (DQC:show-progress "Pass 3/3: Block attributes" 0 1)
  (setq blk-res (DQC:run-block-attrs doc (DQC:filter-bracket-text text-ents)))
  (setq pass (+ pass (cadr blk-res)) fail (+ fail (caddr blk-res)))
  (DQC:show-progress "Pass 3/3: Block attributes" 1 1)

  (grtext -1 "")
  (princ "\n")
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
          (list (list "LBS" "LB") (list "KG") 0.453592 "LB" "KG" 1 2)))
      (setq res (DQC:match-cross-entity ent-info-list rules))
      (setq total (car res) pass (cadr res) fail (caddr res))
      (foreach rec ent-info-list
        (setq r2 (DQC:check-inline-mm-info rec total pass fail))
        (setq total (car r2) pass (cadr r2) fail (caddr r2)))
      (list total pass fail 0))))


;;; ============================================================================
;;;  PART 14 - RESET
;;; ============================================================================

(defun DQC:erase-balloons (doc / ss len i)
  (foreach lname (list DQC:PASS-LAYER DQC:FAIL-LAYER)
    (setq ss (ssget "X" (list (cons 8 lname))))
    (if ss
      (progn
        (setq len (sslength ss) i 0)
        (while (< i len)
          (vl-catch-all-apply 'entdel (list (ssname ss i)))
          (setq i (1+ i))))))
  (vl-catch-all-apply 'vla-Regen (list doc acActiveViewport)))

(defun C:DIMQC-RESET ( / doc n ss len i)
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
  (vla-Regen doc acActiveViewport)
  (princ (strcat "\n Removed " (itoa n) " QC mark entity/entities and deleted QC layers.\n"))
  (princ))


;;; ============================================================================
;;;  PART 15 - MAIN COMMAND C:DIMQC
;;; ============================================================================

(defun C:DIMQC ( / doc mode result total pass fail skip sumstr)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  ;; Reset style caches each run so stale data never carries over
  (setq DQC:STYLE-LFAC-CACHE nil
        DQC:STYLE-DEC-CACHE  nil)
  (DQC:ensure-layer DQC:PASS-LAYER DQC:PASS-COLOR doc)
  (DQC:ensure-layer DQC:FAIL-LAYER DQC:FAIL-COLOR doc)
  (DQC:erase-balloons doc)

  (princ "\n============================================")
  (princ "\n  DIM QC v18 - Select Check Mode")
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
       (princ "\n Running Dimensions Check (inch [mm], FT-LB [N-M], blocks)...\n")
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

  (vla-Regen doc acActiveViewport)
  (setq sumstr (strcat "Checked: " (itoa (+ pass fail))
                       "   PASS: " (itoa pass)
                       "   FAIL: " (itoa fail)
                       "   Skipped: " (itoa skip)))
  (princ (strcat "\n " sumstr "\n"))
  (princ))


;;; ============================================================================
;;;  PART 16 - DIMQC-DIAG  (Mode 3 debug output)
;;; ============================================================================

(defun C:DIMQC-DIAG ( / doc ss len i ename obj ed etype sname lfac is-dim
                         meas raw stripped pair primary alt expected
                         ts to g alt-on dimaltf flags70)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n========== DIMQC DIAGNOSTIC v17.2 ==========\n")
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
        (if pair
          (progn
            (princ (strcat "  Primary: " (rtos (car pair) 2 6) "\n"))
            (if (eq (cadr pair) 'EMPTY)
              (princ "  Alt    : EMPTY\n")
              (princ (strcat "  Alt    : " (rtos (cadr pair) 2 6) "\n")))
            (if (nth 2 pair) (princ (strcat "  InTol  : " (DQC:fmt-tol (nth 2 pair)) "\n")))
            (if (nth 3 pair) (princ (strcat "  MmTol  : " (DQC:fmt-tol (nth 3 pair)) "\n"))))
          (princ "  Pair   : nil (no [mm] found)\n"))
        (princ "\n")
        (setq i (1+ i))
        (if (= (rem i 20) 0) (getstring " --- ENTER for next batch --- ")))
      (princ "========== END ==========\n")))
  (princ))


;;; ============================================================================
;;;  LOAD MESSAGE
;;; ============================================================================
(princ "\n================================================\n")
(princ " DIM QC v18 Loaded.\n")
(princ "   DIMQC        Mode menu:\n")
(princ "     1 = Operating Conditions (HP/kW integer, IN-LB/N-M integer)\n")
(princ "     2 = MED Check (LB/KG, torque, stiffness, PSI)\n")
(princ "     3 = Dimensions Check:\n")
(princ "           inch [mm] all entities + tolerance validation\n")
(princ "           FT-LB [N-M] torque  (no tick if no bracket)\n")
(princ "           LB [KG] weight       (no tick if no bracket)\n")
(princ "           Block attribute inch->mm scan\n")
(princ "           Orphan metric unit detection\n")
(princ "           LB-IN compound (lb-in2) skipped in Modes 2 & 3\n")
(princ "     4 = Notes Check (LB/KG + inch/mm)\n")
(princ "   DIMQC-RESET  Remove marks + delete QC layers\n")
(princ "   DIMQC-DIAG   Command-line diagnostic with pair info\n")
(princ "   Block skip: edit DQC:SKIP-BLOCK-NAMES at top of file\n")
(princ "================================================\n")
