;;; ============================================================================
;;;  dim_qc_v6.lsp  -  Engineering Dual-Unit Dimension QC
;;;  Version 6.0
;;;
;;;  COMMANDS
;;;    DIMQC        Mode selection menu -> run selected check -> place marks
;;;    DIMQC-RESET  Erase all marks AND delete QC layers
;;;    DIMQC-DIAG   Command-line diagnostic
;;;
;;;  MODES
;;;    1. Operating Conditions Check  (HP/kW, IN-LB/N-M pairs from text)
;;;    2. MED Check                   (placeholder - reserved)
;;;    3. Dimensions Check            (inch [mm] only)
;;;    4. Notes Check                 (placeholder - reserved)
;;;
;;;  LAYERS CREATED (all deleted on DIMQC-RESET)
;;;    DIM_QC_PASS  colour 3  (green)
;;;    DIM_QC_FAIL  colour 7  (white, bold text)
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


;;; ============================================================================
;;;  PART 1 - UTILITIES
;;; ============================================================================

(defun DQC:trim (s)
  (if (or (null s) (/= (type s) 'STR)) "" (vl-string-trim " " s)))

(defun DQC:find-char (s c pos / p)
  (if (setq p (vl-string-search c s (1- pos))) (1+ p) 0))

(defun DQC:ok? (primary alt factor / exp diff)
  (if (< (abs primary) 1e-9)
    (< (abs alt) 0.1)
    (progn
      (setq exp  (* (abs primary) factor)
            diff (abs (- exp (abs alt))))
      (if (equal factor 25.4 1e-5)
        (and (<= (/ diff exp) DQC:REL-TOL)
             (<= diff DQC:ABS-TOL))
        (<= (/ diff exp) DQC:REL-TOL)))))

(defun DQC:ensure-layer (name aci doc / layers lay)
  (setq layers (vla-get-Layers doc))
  (setq lay
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers name)))
      (vla-add layers name)
      (vla-item layers name)))
  (vla-put-Color lay aci)
  lay)

(defun DQC:dim-prefix (s / su)
  (setq s (DQC:trim s) su (strcase s))
  (cond
    ((= (strlen s) 0) "")
    ((and (>= (strlen su) 3)
          (= (substr su 1 2) "%%")
          (wcmatch (substr su 3 1) "C,D")) "%%c")
    ((= (substr su 1 1) "R") "R")
    (T "")))

(defun DQC:count-dp-in-token (tok / s i c dot count prev-digit)
  (setq s (DQC:trim tok) i 1)
  (while (and (<= (+ i 1) (strlen s))
              (= (substr s i 1) "%")
              (= (substr s (1+ i) 1) "%"))
    (setq i (+ i 3)))
  (while (and (<= i (strlen s))
              (setq c (substr s i 1))
              (not (wcmatch c "#"))
              (/= c "-")
              (/= c "."))
    (setq i (1+ i)))
  (setq dot 0 count 0 prev-digit nil)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (cond
      ((= c ".")
       (setq dot i i (1+ i)))
      ((wcmatch c "#")
       (if (> dot 0) (setq count (1+ count)))
       (setq prev-digit T i (1+ i)))
      ((and prev-digit (or (= c "+") (= c "-") (= c "/") (= c " ") (= c "~")))
       (setq i (1+ (strlen s))))
      (T (setq i (1+ i)))))
  count)

(defun DQC:fmt (val dp)
  (rtos val 2 dp))

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
             ((= nx "S")
              (setq sc (DQC:find-char s ";" (+ i 2)))
              (if (= sc 0)
                (setq i (+ i 2))
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
             ((wcmatch nx "L,O,K")
              (setq i (+ i 2)))
             (T
              (setq out (strcat out (substr s (1+ i) 1)) i (+ i 2)))))
         (setq i (1+ i))))

      ((and (= ch "<") (<= (1+ i) (strlen s)) (= (substr s (1+ i) 1) ">"))
       (if (and meas (numberp meas))
         (setq out (strcat out (rtos (abs meas) 2 6)))
         (setq out (strcat out "<>")))
       (setq i (+ i 2)))

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

      ((or (= ch "{") (= ch "}"))
       (setq i (1+ i)))

      (T
       (setq out (strcat out ch) i (1+ i)))))
  out)


;;; ============================================================================
;;;  PART 3 - TOLERANCE PARSER
;;; ============================================================================

(defun DQC:parse-tol-block (content / caret-pos hi lo)
  (setq caret-pos (DQC:find-char content "^" 1))
  (if (> caret-pos 0)
    (list (DQC:trim (substr content 1 (1- caret-pos)))
          (DQC:trim (substr content (1+ caret-pos))))
    (progn
      (setq hi (DQC:trim content))
      (if (= (strlen hi) 0) nil
        (if (= (substr hi 1 1) "+")
          (list hi (strcat "-" (substr hi 2)))
          (list (strcat "+" hi) (strcat "-" hi)))))))

(defun DQC:extract-tol (s / t1 t2 content result)
  (setq t1 (DQC:find-char s "~" 1))
  (if (= t1 0)
    (DQC:extract-plain-tol s)
    (progn
      (setq t2 (DQC:find-char s "~" (1+ t1)))
      (if (= t2 0)
        (DQC:extract-plain-tol s)
        (progn
          (setq content (substr s (1+ t1) (- t2 t1 1)))
          (setq result (DQC:parse-tol-block content))
          (if result result
            (DQC:extract-plain-tol s)))))))

(defun DQC:extract-plain-tol (s / su i j c hi lo num-s found)
  (setq su (strcase s) i 1 found nil j 1)
  (while (and (<= j (- (strlen su) 2)) (null found))
    (if (and (= (substr su j 1) "%")
             (= (substr su (+ j 1) 1) "%")
             (= (substr su (+ j 2) 1) "P"))
      (progn
        (setq i (+ j 3) num-s "")
        (while (and (<= i (strlen s)) (= (substr s i 1) " "))
          (setq i (1+ i)))
        (if (and (<= i (strlen s)) (wcmatch (substr s i 1) "-,+"))
          (setq num-s (strcat num-s (substr s i 1)) i (1+ i)))
        (while (and (<= i (strlen s))
                    (or (wcmatch (substr s i 1) "#") (= (substr s i 1) ".")))
          (setq num-s (strcat num-s (substr s i 1)) i (1+ i)))
        (if (> (strlen num-s) 0)
          (progn
            (setq num-s
              (if (= (substr num-s 1 1) "-") (substr num-s 2) num-s))
            (setq found (list (strcat "+" num-s) (strcat "-" num-s))))
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
                           (or (= (substr s j 1) "/")
                               (= (substr s j 1) "^")
                               (= (substr s j 1) " ")
                               (= (substr s j 1) "|")))
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

(defun DQC:drop-tol (s / out i ch in-tol)
  (setq out "" i 1 in-tol nil)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (cond
      ((= ch "~") (setq in-tol (not in-tol) i (1+ i)))
      (in-tol      (setq i (1+ i)))
      (T           (setq out (strcat out ch) i (1+ i)))))
  out)

(defun DQC:strip-pfx (tok / su orig cur iter)
  (setq cur (vl-string-trim " \t" tok) iter 0)
  (while (< iter 5)
    (setq su (strcase cur) orig cur)
    (cond
      ((wcmatch su "R#*")    (setq cur (substr cur 2)))
      ((wcmatch su "R.*")    (setq cur (substr cur 2)))
      ((wcmatch su "R *")    (setq cur (substr cur 2)))
      ((wcmatch su "SR#*")   (setq cur (substr cur 3)))
      ((wcmatch su "SR.*")   (setq cur (substr cur 3)))
      ((wcmatch su "SR *")   (setq cur (substr cur 3)))
      ((wcmatch su "S#*")    (setq cur (substr cur 2)))
      ((wcmatch su "S.*")    (setq cur (substr cur 2)))
      ((wcmatch su "S *")    (setq cur (substr cur 2)))
      ((wcmatch su "M#*")    (setq cur (substr cur 2)))
      ((wcmatch su "M.*")    (setq cur (substr cur 2)))
      ((wcmatch su "M *")    (setq cur (substr cur 2)))
      ((wcmatch su "DIA*")   (setq cur (substr cur 4)))
      ((wcmatch su "%%C*")   (setq cur (substr cur 4)))
      ((wcmatch su "%%?*")   (setq cur (substr cur 4))))
    (setq cur (vl-string-trim " \t" cur))
    (if (= orig cur) (setq iter 5) (setq iter (1+ iter))))
  cur)

(defun DQC:first-num (tok / v)
  (setq tok (DQC:drop-tol (DQC:trim tok)))
  (setq tok (DQC:strip-pfx tok))
  (if (= (strlen tok) 0) nil
    (progn
      (setq v (atof tok))
      (if (= v 0.0)
        (if (wcmatch (substr tok 1 1) "#") v nil)
        v))))

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
                        (DQC:parse-metric alt-s)
                        nil))
              (list p (if m m 'EMPTY) in-tol mm-tol))))))))


;;; ============================================================================
;;;  PART 4B - INLINE DIMENSION SCANNER
;;;  Scans plain text for embedded  N [M]  patterns inside sentences.
;;; ============================================================================

(defun DQC:scan-inline-dims (txt / results i len ch num-s in-num dot-seen j alt-s k closed)
  (setq results nil i 1 len (strlen txt))
  (while (<= i len)
    (setq ch (substr txt i 1))
    (if (or (wcmatch ch "#")
            (and (= ch ".") (<= (1+ i) len) (wcmatch (substr txt (1+ i) 1) "#")))
      (progn
        (setq num-s "" j i dot-seen nil in-num T)
        (while (and (<= j len) in-num)
          (setq ch (substr txt j 1))
          (cond
            ((wcmatch ch "#") (setq num-s (strcat num-s ch) j (1+ j)))
            ((and (= ch ".") (not dot-seen))
             (setq num-s (strcat num-s ch) dot-seen T j (1+ j)))
            (T (setq in-num nil))))
        (setq k j)
        (while (and (<= k len) (= (substr txt k 1) " "))
          (setq k (1+ k)))
        (if (and (<= k len) (= (substr txt k 1) "["))
          (progn
            (setq k (1+ k) alt-s "" closed nil)
            (while (and (<= k len) (not closed))
              (setq ch (substr txt k 1))
              (if (= ch "]")
                (setq closed T)
                (setq alt-s (strcat alt-s ch) k (1+ k))))
            (if closed
              (progn
                (setq results (append results (list (list num-s alt-s i))))
                (setq i k))
              (setq i j)))
          (setq i j)))
      (setq i (1+ i))))
  results)


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
        ((wcmatch etype "MTEXT,TEXT,ATTDEF,ATTRIB")
         (setq pt (cdr (assoc 10 ed))))
        ((wcmatch etype "*LEADER*")
         (setq txpt (vl-catch-all-apply 'vlax-get (list obj 'TextLocation)))
         (if (and (not (vl-catch-all-error-p txpt))
                  txpt
                  (not (equal txpt '(0.0 0.0 0.0))))
           (setq pt txpt)
           (setq pt (cdr (assoc 10 ed)))))
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
;;;  PART 6 - MARK PLACEMENT
;;; ============================================================================

(defun DQC:rot2 (dx dy ang / ca sa)
  (setq ca (cos ang) sa (sin ang))
  (list (- (* dx ca) (* dy sa))
        (+ (* dx sa) (* dy ca))))

(defun DQC:place-balloon (txtpt txth dimang label layer / bh bw offx offy ovec ins ent-data)
  (setq bh (if (and DQC:TXT-H (> DQC:TXT-H 0))
             DQC:TXT-H
             (* txth 0.85)))
  (if (< bh 0.5) (setq bh 0.5))
  (setq offx (if (and DQC:OFFSET (> DQC:OFFSET 0)) DQC:OFFSET 0.0)
        offy (* bh 0.7))
  (setq ovec (DQC:rot2 offx offy (if dimang dimang 0.0)))
  (setq ins (list (+ (car  txtpt) (car  ovec))
                  (+ (cadr txtpt) (cadr ovec))
                  (if (caddr txtpt) (caddr txtpt) 0.0)))
  (setq bw (* (strlen label) bh 0.8))
  (if (< bw (* bh 3.0)) (setq bw (* bh 3.0)))
  (setq ent-data
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
  (if (vl-catch-all-error-p (vl-catch-all-apply 'entmake (list ent-data)))
    nil (entlast)))

(defun DQC:fail-label (body)
  (strcat "{\\fArial|b1|i0;" body "}"))

(defun DQC:pass-label ()
  "{\\fArial|b1|i0;\\U+2713}")


;;; ============================================================================
;;;  PART 7 - PROCESS ONE ENTITY  (Mode 3: Dimensions inch [mm] only)
;;; ============================================================================

(defun DQC:process-dim (ename doc / obj ed etype entlay
                              meas sname lfac primary-auto is-dim
                              ts to g1 flags70 alt-on dimaltf
                              pair stripped raw from-text from-meas-sub
                              pfx in-dp mm-dp style-dp
                              bracket-pos inch-seg
                              primary alt expected ok label layer
                              in-tol mm-tol tol-str
                              txtpt txth dimang factor unit-str)

  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (vl-catch-all-error-p obj)
    (list 'SKIP "" nil nil nil "")
    (progn
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))
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

          (if (or (wcmatch (strcase ts) "*TAPER*")
                  (wcmatch (strcase to) "*TAPER*")
                  (wcmatch (strcase g1) "*TAPER*"))
            (list 'SKIP "" nil nil nil "TAPER callout - skipped")

            (progn
              (setq pair nil from-text nil from-meas-sub nil stripped "" raw "")

              (if (and (null pair) (> (strlen (DQC:trim ts)) 0))
                (progn
                  (setq stripped (DQC:strip ts primary-auto (* primary-auto DQC:MM/IN)))
                  (setq raw stripped)
                  (setq pair (DQC:parse stripped))
                  (if pair (setq from-text T from-meas-sub nil))))

              (if (and (null pair) (> (strlen (DQC:trim to)) 0))
                (progn
                  (setq stripped (DQC:strip to primary-auto (* primary-auto DQC:MM/IN)))
                  (setq raw stripped)
                  (setq pair (DQC:parse stripped))
                  (if pair (setq from-text T from-meas-sub (DQC:has-meas-token to)))))

              (if (and (null pair) (= (type g1) 'STR) (> (strlen (DQC:trim g1)) 0))
                (progn
                  (setq stripped (DQC:strip g1 primary-auto (* primary-auto DQC:MM/IN)))
                  (setq raw stripped)
                  (setq pair (DQC:parse stripped))
                  (if pair (setq from-text T from-meas-sub (DQC:has-meas-token g1)))))

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

              (setq pfx (DQC:dim-prefix stripped))
              (if (= pfx "") (setq pfx (DQC:dim-prefix ts)))
              (if (= pfx "") (setq pfx (DQC:dim-prefix to)))
              (if (= pfx "") (setq pfx (DQC:dim-prefix g1)))

              (setq style-dp (DQC:dimdec sname doc))
              (if (or (null style-dp) (< style-dp 0))
                (setq style-dp (fix (getvar "DIMDEC"))))

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
                  (strcat " [" (DQC:fmt-tol in-tol) " | " (DQC:fmt-tol mm-tol) "]")
                  ""))

              (setq txtpt (DQC:dim-textpt ename))
              (setq txth  (DQC:dim-txth  ename))
              (setq dimang (vl-catch-all-apply 'vlax-get (list obj 'TextRotation)))
              (if (vl-catch-all-error-p dimang)
                (setq dimang (if ed (cdr (assoc 53 ed)) 0.0)))
              (if (null dimang) (setq dimang 0.0))

              (setq factor  DQC:MM/IN
                    unit-str "mm")

              (cond
                ((null pair)
                 (list 'SKIP "" nil nil nil raw))

                ((eq (cadr pair) 'EMPTY)
                 (setq primary  (car pair)
                       expected (* (abs primary) factor)
                       label    (DQC:fail-label
                                  (strcat "\\U+2717 "
                                          pfx (DQC:fmt primary in-dp)
                                          " [?] exp " (DQC:fmt expected mm-dp) " " unit-str
                                          tol-str))
                       layer    DQC:FAIL-LAYER)
                 (if txtpt (DQC:place-balloon txtpt txth dimang label layer))
                 (list 'FAIL label primary nil expected raw))

                (T
                 (setq primary  (car  pair)
                       alt      (cadr pair)
                       expected (* (abs primary) factor)
                       ok       (DQC:ok? primary alt factor))
                 (if ok
                   (setq label (DQC:pass-label)
                         layer DQC:PASS-LAYER)
                   (setq label (DQC:fail-label
                                 (strcat "\\U+2717 "
                                         pfx (DQC:fmt primary in-dp)
                                         " [" (DQC:fmt alt mm-dp)
                                         "] exp " (DQC:fmt expected mm-dp) " " unit-str tol-str))
                         layer DQC:FAIL-LAYER))
                 (if txtpt (DQC:place-balloon txtpt txth dimang label layer))
                 (list (if ok 'PASS 'FAIL) label primary alt expected raw)))))))))


;;; ============================================================================
;;;  PART 7B - PROCESS TEXT ENTITY FOR INLINE DIMENSIONS
;;; ============================================================================

(defun DQC:process-text-inline (ename doc / obj ed etype entlay ts g1 stripped
                                        hits txt txth txtpt dimang factor unit-str
                                        ps as pv av expected ok label layer)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (not (vl-catch-all-error-p obj))
    (progn
      (setq ed (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))
      (setq entlay (if ed (cdr (assoc 8 ed)) nil))
      (if (not (and entlay
                    (or (= (strcase entlay) (strcase DQC:PASS-LAYER))
                        (= (strcase entlay) (strcase DQC:FAIL-LAYER)))))
        (progn
          (setq etype (if ed (strcase (cdr (assoc 0 ed))) "?"))
          (if (wcmatch etype "MTEXT,TEXT")
            (progn
              (setq ts (vl-catch-all-apply 'vlax-get (list obj 'TextString)))
              (if (or (vl-catch-all-error-p ts) (null ts)) (setq ts ""))
              (setq g1 (if ed (cdr (assoc 1 ed)) nil))
              (if (null g1) (setq g1 ""))
              (setq txt (if (> (strlen (DQC:trim ts)) 0) ts g1))
              (setq stripped (DQC:strip txt 0.0 0.0))
              (setq hits (DQC:scan-inline-dims stripped))
              (setq txtpt (DQC:dim-textpt ename))
              (setq txth  (DQC:dim-txth  ename))
              (setq dimang 0.0 factor DQC:MM/IN unit-str "mm")
              (foreach hit hits
                (setq ps (car hit) as (cadr hit))
                (setq pv (atof ps) av (atof as))
                (if (and (> pv 0.0) (> av 0.0))
                  (progn
                    (setq expected (* (abs pv) factor)
                          ok (DQC:ok? pv av factor))
                    (if ok
                      (setq label (DQC:pass-label) layer DQC:PASS-LAYER)
                      (setq label (DQC:fail-label
                                    (strcat "\\U+2717 "
                                            ps " [" as "] exp "
                                            (rtos expected 2 2) " " unit-str))
                            layer DQC:FAIL-LAYER))
                    (if txtpt
                      (DQC:place-balloon txtpt txth dimang label layer))))))))))))


;;; ============================================================================
;;;  PART 8A - MODE 1: OPERATING CONDITIONS CHECK
;;; ============================================================================

;;; Read number backward from kw-pos (1-based). Returns (value) or nil.
(defun DQC:num-before-kw (s kw-pos / i num-s dot-seen)
  (setq i (1- kw-pos))
  (while (and (> i 0) (= (substr s i 1) " "))
    (setq i (1- i)))
  (if (<= i 0) nil
    (progn
      (setq num-s "" dot-seen nil)
      (while (and (> i 0)
                  (or (wcmatch (substr s i 1) "#")
                      (and (= (substr s i 1) ".") (not dot-seen))))
        (if (= (substr s i 1) ".") (setq dot-seen T))
        (setq num-s (strcat (substr s i 1) num-s)
              i (1- i)))
      (if (= (strlen num-s) 0) nil
        (list (atof num-s))))))

;;; Read number forward from kw-end (1-based). Returns real or nil.
(defun DQC:num-after-pos (s kw-end / i num-s dot-seen)
  (setq i kw-end)
  (while (and (<= i (strlen s)) (= (substr s i 1) " "))
    (setq i (1+ i)))
  (setq num-s "" dot-seen nil)
  (while (and (<= i (strlen s))
              (or (wcmatch (substr s i 1) "#")
                  (and (= (substr s i 1) ".") (not dot-seen))))
    (if (= (substr s i 1) ".") (setq dot-seen T))
    (setq num-s (strcat num-s (substr s i 1)) i (1+ i)))
  (if (= (strlen num-s) 0) nil
    (atof num-s)))

;;; Find keyword kw in uppercase string su, 1-based start from.
;;; Returns 1-based start pos or 0.
(defun DQC:kw-find (su kw from / p)
  (setq p (vl-string-search kw su (1- from)))
  (if p (1+ p) 0))

;;; Parse string s for HP [kW] pairs.
;;; Returns list of (hp-val kw-val ok expected).
(defun DQC:find-hp-kw (s / su i hp-pos hp-end hp-val
                            srch-s brk-p kw-p kw-val
                            expected ok result srch-len)
  (setq su (strcase s) result nil i 1)
  (while (<= i (strlen su))
    (setq hp-pos (DQC:kw-find su "HP" i))
    (if (= hp-pos 0)
      (setq i (1+ (strlen su)))
      (progn
        (if (or (= hp-pos 1) (not (wcmatch (substr su (1- hp-pos) 1) "A-Z,#")))
          (progn
            (setq hp-val (DQC:num-before-kw s hp-pos))
            (if hp-val
              (progn
                (setq hp-end (+ hp-pos 2))
                (setq srch-len (min 200 (max 1 (- (strlen su) hp-end -1))))
                (setq srch-s (substr su hp-end srch-len))
                (setq brk-p (vl-string-search "[" srch-s 0))
                (setq kw-val nil)
                (if brk-p
                  (progn
                    (setq kw-p (vl-string-search "KW" srch-s (1+ brk-p)))
                    (if kw-p
                      (setq kw-val (DQC:num-after-pos srch-s (1+ brk-p))))))
                (if (and kw-val (car hp-val))
                  (progn
                    (setq expected (* (car hp-val) 0.7457)
                          ok (DQC:ok? (car hp-val) kw-val 0.7457))
                    (setq result (append result
                                   (list (list (car hp-val) kw-val ok expected))))))))))
        (setq i (+ hp-pos 2)))))
  result)

;;; Parse string s for IN-LB [N-M] pairs.
;;; Returns list of (inlb-val nm-val ok expected).
(defun DQC:find-inlb-nm (s / su i kw-start kw-end inlb-val
                              srch-s brk-p nm-p nm-val
                              expected ok result kw-list nm-list
                              kw j srch-len)
  (setq su (strcase s) result nil i 1)
  (setq kw-list (list "IN-LB" "IN.LB" "IN LB" "INLB"))
  (setq nm-list (list "N-M" "N.M" "N M" "NM"))
  (while (<= i (strlen su))
    (setq kw-start 0 kw-end 0)
    (foreach kw kw-list
      (setq j (DQC:kw-find su kw i))
      (if (and (> j 0) (or (= kw-start 0) (< j kw-start)))
        (progn
          (setq kw-start j
                kw-end   (+ j (strlen kw))))))
    (if (= kw-start 0)
      (setq i (1+ (strlen su)))
      (progn
        (setq inlb-val (DQC:num-before-kw s kw-start))
        (if inlb-val
          (progn
            (setq srch-len (min 200 (max 1 (- (strlen su) kw-end -1))))
            (setq srch-s (substr su kw-end srch-len))
            (setq brk-p (vl-string-search "[" srch-s 0))
            (setq nm-val nil nm-p nil)
            (if brk-p
              (progn
                (foreach nm-kw nm-list
                  (if (null nm-p)
                    (setq nm-p (vl-string-search nm-kw srch-s (1+ brk-p)))))
                (if nm-p
                  (setq nm-val (DQC:num-after-pos srch-s (1+ brk-p))))))
            (if (and nm-val (car inlb-val))
              (progn
                (setq expected (* (car inlb-val) 0.112985)
                      ok (DQC:ok? (car inlb-val) nm-val 0.112985))
                (setq result (append result
                               (list (list (car inlb-val) nm-val ok expected))))))))
        (setq i (1+ kw-end)))))
  result)

;;; Run Mode 1: Operating Conditions Check.
(defun DQC:run-opcond (doc / ss len i ename obj ed entlay ts g1
                              txt stripped hplist nmlist txtpt txth dimang
                              total pass fail skip label layer ok expected)
  (setq total 0 pass 0 fail 0 skip 0)
  (setq ss (ssget "X" (list (cons 0 "MTEXT,TEXT"))))
  (if (null ss)
    (progn (alert "No text entities found.") (princ) (exit)))
  (setq len (sslength ss) i 0)
  (while (< i len)
    (setq ename (ssname ss i))
    (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
    (if (not (vl-catch-all-error-p obj))
      (progn
        (setq ed (vl-catch-all-apply 'entget (list ename)))
        (if (vl-catch-all-error-p ed) (setq ed nil))
        (setq entlay (if ed (cdr (assoc 8 ed)) nil))
        (if (not (and entlay
                      (or (= (strcase entlay) (strcase DQC:PASS-LAYER))
                          (= (strcase entlay) (strcase DQC:FAIL-LAYER)))))
          (progn
            (setq ts (vl-catch-all-apply 'vlax-get (list obj 'TextString)))
            (if (or (vl-catch-all-error-p ts) (null ts)) (setq ts ""))
            (setq g1 (if ed (cdr (assoc 1 ed)) nil))
            (if (null g1) (setq g1 ""))
            (setq txt (if (> (strlen (DQC:trim ts)) 0) ts g1))
            (setq stripped (DQC:strip txt 0.0 0.0))
            (setq txtpt (DQC:dim-textpt ename))
            (setq txth  (DQC:dim-txth  ename))
            (setq dimang 0.0)

            (setq hplist (DQC:find-hp-kw stripped))
            (foreach hp hplist
              (setq ok (caddr hp) expected (cadddr hp))
              (setq total (1+ total))
              (if ok
                (progn
                  (setq pass (1+ pass))
                  (setq label (DQC:pass-label) layer DQC:PASS-LAYER))
                (progn
                  (setq fail (1+ fail))
                  (setq label (DQC:fail-label
                                (strcat "\\U+2717 HP: "
                                        (rtos (car hp) 2 1) " HP ["
                                        (rtos (cadr hp) 2 2) " kW] exp "
                                        (rtos expected 2 2) " kW"))
                        layer DQC:FAIL-LAYER)))
              (if txtpt (DQC:place-balloon txtpt txth dimang label layer)))

            (setq nmlist (DQC:find-inlb-nm stripped))
            (foreach nm nmlist
              (setq ok (caddr nm) expected (cadddr nm))
              (setq total (1+ total))
              (if ok
                (progn
                  (setq pass (1+ pass))
                  (setq label (DQC:pass-label) layer DQC:PASS-LAYER))
                (progn
                  (setq fail (1+ fail))
                  (setq label (DQC:fail-label
                                (strcat "\\U+2717 IN-LB: "
                                        (rtos (car nm) 2 0) " IN-LB ["
                                        (rtos (cadr nm) 2 1) " N-M] exp "
                                        (rtos expected 2 2) " N-M"))
                        layer DQC:FAIL-LAYER)))
              (if txtpt (DQC:place-balloon txtpt txth dimang label layer)))))))
    (setq i (1+ i)))
  (list total pass fail skip)))


;;; ============================================================================
;;;  PART 8B - MODE 3: DIMENSIONS CHECK
;;; ============================================================================

(defun DQC:run-dims (doc / ss len i ename res total pass fail skip)
  (setq total 0 pass 0 fail 0 skip 0)
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER,LEADER,MTEXT,TEXT"))))
  (if (null ss)
    (progn (alert "No dimension entities found.") (princ) (exit)))
  (setq len (sslength ss) i 0)
  (while (< i len)
    (setq ename (ssname ss i)
          res   (DQC:process-dim ename doc)
          total (1+ total))
    (cond
      ((= (car res) 'PASS) (setq pass (1+ pass)))
      ((= (car res) 'FAIL) (setq fail (1+ fail)))
      (T (setq skip (1+ skip))))
    (DQC:process-text-inline ename doc)
    (setq i (1+ i)))
  (list total pass fail skip))


;;; ============================================================================
;;;  PART 9 - MAIN COMMAND  C:DIMQC
;;; ============================================================================

(defun C:DIMQC ( / doc mode result total pass fail skip sumstr)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (DQC:ensure-layer DQC:PASS-LAYER DQC:PASS-COLOR doc)
  (DQC:ensure-layer DQC:FAIL-LAYER DQC:FAIL-COLOR doc)
  (DQC:erase-balloons doc)

  (princ "\n============================================")
  (princ "\n  DIM QC v6.0 - Select Check Mode")
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
     (setq result (DQC:run-opcond doc))
     (setq total (car result) pass (cadr result)
           fail (caddr result) skip (cadddr result)))

    ((= mode 2)
     (alert "MED Check mode is reserved for future implementation.")
     (princ) (exit))

    ((= mode 3)
     (princ "\n Running Dimensions Check (inch [mm])...\n")
     (setq result (DQC:run-dims doc))
     (setq total (car result) pass (cadr result)
           fail (caddr result) skip (cadddr result)))

    ((= mode 4)
     (alert "Notes Check mode is reserved for future implementation.")
     (princ) (exit)))

  (vla-Regen doc acAllViewports)
  (setq sumstr (strcat "Checked: " (itoa (+ pass fail))
                       "   PASS: " (itoa pass)
                       "   FAIL: " (itoa fail)
                       "   Skipped: " (itoa skip)))
  (princ (strcat "\n " sumstr "\n"))
  (princ))


;;; ============================================================================
;;;  PART 10 - DIMQC-RESET
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

(defun DQC:purge-layer (lname doc / ss len i layers lay)
  (setq ss (ssget "X" (list (cons 8 lname))))
  (if ss
    (progn
      (setq len (sslength ss) i 0)
      (while (< i len)
        (vl-catch-all-apply 'entdel (list (ssname ss i)))
        (setq i (1+ i)))))
  (setq layers (vla-get-Layers doc))
  (vl-catch-all-apply
    (function (lambda ()
      (setq lay (vla-item layers lname))
      (if lay (vla-delete lay))))
    nil))

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
  (princ (strcat "\n Removed " (itoa n) " mark(s) and deleted QC layers.\n\n"))
  (princ))


;;; ============================================================================
;;;  PART 11 - DIMQC-DIAG
;;; ============================================================================

(defun C:DIMQC-DIAG ( / doc ss len i ename obj ed etype sname lfac is-dim
                         meas raw stripped pair primary alt expected
                         ts to g alt-on dimaltf flags70)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n========== DIMQC DIAGNOSTIC v6.0 ==========\n")
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
                   "  mm=EMPTY"
                   "  in-tol=" (vl-princ-to-string (nth 2 pair))
                   "  exp=" (rtos (* (abs (car pair)) DQC:MM/IN) 2 4) " mm"))
          (T
           (setq primary (car pair) alt (cadr pair)
                 expected (* (abs primary) DQC:MM/IN))
           (strcat "primary=" (rtos primary 2 6)
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
(princ " DIM QC v6.0 Loaded.\n")
(princ "\n")
(princ "   DIMQC        Mode selection menu + run QC\n")
(princ "     Mode 1 = Operating Conditions (HP/kW, IN-LB/N-M)\n")
(princ "     Mode 2 = MED Check (reserved)\n")
(princ "     Mode 3 = Dimensions Check (inch [mm])\n")
(princ "     Mode 4 = Notes Check (reserved)\n")
(princ "\n")
(princ "   DIMQC-RESET  Remove all marks + delete QC layers\n")
(princ "   DIMQC-DIAG   Command-line diagnostic\n")
(princ "================================================\n\n")
(princ)
