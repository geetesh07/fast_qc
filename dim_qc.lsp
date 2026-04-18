;;; ============================================================================
;;;  dim_qc_v4.lsp  -  Engineering Dual-Unit Dimension QC  (Balloon Edition)
;;;  Version 4.0
;;;
;;;  WHAT THIS DOES
;;;    Reads every DIMENSION entity in the drawing, checks whether the
;;;    inch value × 25.4 matches the mm value in the [ ] bracket, then
;;;    places a small MText callout balloon NEXT TO each dimension text.
;;;
;;;    Balloon content:
;;;      PASS  →  "✓ 25.40"        (green layer, prints green or black)
;;;      FAIL  →  "✗ exp 25.40"    (red layer, prints red or black)
;;;      EMPTY →  "✗ mm?"          (red layer – mm value not entered yet)
;;;
;;;    Because the balloons are real MText objects in the drawing they
;;;    plot perfectly in any PDF at any scale with any CTB.  No colour
;;;    mapping, no SOLID-entity tricks, no post-processing.
;;;
;;;  COMMANDS
;;;    DIMQC        Open settings GUI → run check → place balloons
;;;    DIMQC-RESET  Erase all balloons (leaves dims untouched)
;;;    DIMQC-DIAG   Command-line diagnostic – shows parsed values for every dim
;;;
;;;  LAYERS CREATED
;;;    DIM_QC_PASS   colour 3  (green)
;;;    DIM_QC_FAIL   colour 1  (red)
;;;
;;;  HOW TO PRINT
;;;    Just plot normally.  The balloons are geometry.
;;;    To hide balloons while working: freeze layers DIM_QC_PASS and DIM_QC_FAIL
;;;    To show again: thaw those layers
;;;    To permanently remove: run DIMQC-RESET
;;;
;;;  TOLERANCES  (editable in GUI)
;;;    Relative : 3 %   (generous for nominal dims)
;;;    Absolute : 0.08 mm  (fallback for tiny values)
;;; ============================================================================

(vl-load-com)

;;; ── Global defaults (all overrideable from the GUI) ──────────────────────
(setq DQC:MM/IN    25.4)
(setq DQC:REL-TOL  0.03)   ; 3 %
(setq DQC:ABS-TOL  0.08)   ; mm
(setq DQC:TXT-H    nil)    ; balloon text height – nil = auto from dim style
(setq DQC:OFFSET   nil)    ; balloon offset from dim text – nil = auto

(setq DQC:PASS-LAYER "DIM_QC_PASS")
(setq DQC:FAIL-LAYER "DIM_QC_FAIL")
(setq DQC:PASS-COLOR 3)    ; green
(setq DQC:FAIL-COLOR 1)    ; red


;;; ============================================================================
;;;  PART 1 – LOW-LEVEL UTILITIES
;;; ============================================================================

;;; Trim leading/trailing spaces
(defun DQC:trim (s)
  (if (or (null s) (/= (type s) 'STR)) (setq s ""))
  (while (and (> (strlen s) 0) (= (substr s 1 1) " "))
    (setq s (substr s 2)))
  (while (and (> (strlen s) 0) (= (substr s (strlen s) 1) " "))
    (setq s (substr s 1 (1- (strlen s)))))
  s)

;;; Index of first char c in s starting at pos; returns 0 if not found
(defun DQC:find-char (s c pos / i)
  (setq i pos)
  (while (and (<= i (strlen s)) (/= (substr s i 1) c))
    (setq i (1+ i)))
  (if (<= i (strlen s)) i 0))

;;; Parse first number in tok, skipping prefix junk (Ø %%c R ( ± spaces)
(defun DQC:clean-num (tok / i c)
  (setq tok (DQC:trim tok))
  (if (= (strlen tok) 0) nil
    (progn
      (setq i 1)
      ;; skip %%c / %%d / %%p etc.
      (while (and (<= (+ i 2) (strlen tok))
                  (= (substr tok i 1) "%")
                  (= (substr tok (1+ i) 1) "%"))
        (setq i (+ i 3)))
      ;; skip non-numeric prefix chars
      (while (and (<= i (strlen tok))
                  (setq c (substr tok i 1))
                  (not (wcmatch c "#"))
                  (/= c "-")
                  (/= c "."))
        (setq i (1+ i)))
      (if (> i (strlen tok)) nil
        (atof (substr tok i))))))

;;; Verify: |alt - primary×factor| within relative OR absolute tolerance
(defun DQC:ok? (primary alt factor / exp diff)
  (if (< (abs primary) 1e-9)
    (< (abs alt) 0.1)
    (progn
      (setq exp  (* (abs primary) factor)
            diff (abs (- exp (abs alt))))
      (or (<= (/ diff exp) DQC:REL-TOL)
          (<= diff DQC:ABS-TOL)))))

;;; Ensure a layer exists with the given ACI colour; return the layer object
(defun DQC:ensure-layer (name aci doc / layers lay)
  (setq layers (vla-get-Layers doc))
  (setq lay
    (if (vl-catch-all-error-p
          (vl-catch-all-apply 'vla-item (list layers name)))
      (vla-add layers name)
      (vla-item layers name)))
  (vla-put-Color lay aci)
  lay)

;;; Safe string: replace nil with ""
(defun DQC:s (x) (if (and x (= (type x) 'STR)) x ""))


;;; ============================================================================
;;;  PART 2 – MTEXT FORMAT-CODE STRIPPER
;;;
;;;  \P  (paragraph break)  → "|"  (line separator for the parser)
;;;  \S  (stacked fraction) → "~tol~" wrapper so parser can isolate it
;;;      e.g.  \S+.004^-.000;  →  ~+.004^-.000~
;;;      This lets DQC:first-num skip the tolerance entirely,
;;;      while DQC:parse-metric can still read the nominal that precedes it.
;;;
;;;  All other format codes (\H \A \C \T \Q \W \F \L \O \K \U) → discarded.
;;;  {} grouping braces → discarded.
;;;  <> measurement token → substituted with the numeric measurement.
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
             ;; \P or \X paragraph/line break → pipe separator for parser
             ((or (= nx "P") (= nx "X"))
              (setq out (strcat out "|") i (+ i 2)))
             ;; \~ non-breaking space
             ((= nx "~")
              (setq out (strcat out " ") i (+ i 2)))
             ;; \\ literal backslash
             ((= nx "\\")
              (setq out (strcat out "\\") i (+ i 2)))
             ;; \S stacked fraction → wrap content in ~...~ so it can be
             ;;    stripped from the nominal number but kept for metric parsing
             ((= nx "S")
              (setq sc (DQC:find-char s ";" (+ i 2)))
              (if (= sc 0)
                (setq i (+ i 2))
                (progn
                  (setq frac (substr s (+ i 2) (- sc i 2)))
                  (setq out (strcat out "~" frac "~"))
                  (setq i (1+ sc)))))
             ;; \H \A \C \T \Q \W \F – codes with semicolons, no content needed
             ((wcmatch nx "H,A,C,T,Q,W,F")
              (setq sc (DQC:find-char s ";" (+ i 2)))
              (if (= sc 0) (setq i (+ i 2)) (setq i (1+ sc))))
             ;; \L \O \K \U – toggle codes, no argument
             ((wcmatch nx "L,O,K,U")
              (setq i (+ i 2)))
             ;; Unknown escape – skip the backslash, keep the char
             (T
              (setq out (strcat out (substr s (1+ i) 1)) i (+ i 2)))))
         ;; Lone trailing backslash – skip it
         (setq i (1+ i))))

      ;; <> measurement placeholder
      ((and (= ch "<") (<= (1+ i) (strlen s)) (= (substr s (1+ i) 1) ">"))
       (if (and meas (numberp meas))
         (setq out (strcat out (rtos (abs meas) 2 6)))
         (setq out (strcat out "<>")))
       (setq i (+ i 2)))

      ;; [] alternate measurement placeholder (handles spaces inside like [ ])
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

      ;; Grouping braces – discard
      ((or (= ch "{") (= ch "}"))
       (setq i (1+ i)))

      ;; Normal character
      (T
       (setq out (strcat out ch) i (1+ i)))))
  out)


;;; ============================================================================
;;;  PART 3 – TEXT PARSER
;;;
;;;  After DQC:strip, the string uses two special separators:
;;;    "|"  = line break (was \P)  e.g. "%%c4.623~+.004^-.000~|[117.42~+0.10^-0.00~]"
;;;    "~"  = tolerance wrapper boundary (was \S...;)
;;;
;;;  Formats handled:
;;;    A  same-line no tol  :  ".595 [15.1]"
;;;    B  two-line no tol   :  "5.726 (COLD)|[145.4]"
;;;    C  prefix            :  "%%c.465 [11.8]"   "R.005 [0.13]"
;;;    D  same-line tol     :  ".500~+.005^-.000~ [12.70~+0.13^-0.00~]"
;;;                            or  ".500~+.005^-.000~\P[12.70~+0.13^-0.00~]"
;;;    E  two-line tol      :  "%%c4.623~+.004^-.000~|[117.42~+0.10^-0.00~]"
;;;    F  empty bracket     :  "2.500|[]"  "2.500 [ ]"
;;;
;;;  Returns:
;;;    (primary  alt-real)  – both parsed
;;;    (primary  'EMPTY)    – bracket exists but nothing useful inside
;;;    nil                  – no bracket found → skip (single-unit dim)
;;; ============================================================================

;;; ── String utilities ────────────────────────────────────────────────────────

;;; Split string s at the FIRST occurrence of single char c.
;;; Returns (before after) or nil.
(defun DQC:split-at (s c / pos)
  (setq pos (DQC:find-char s c 1))
  (if (= pos 0) nil
    (list (substr s 1 (1- pos))
          (substr s (1+ pos)))))

;;; Remove all ~...~ tolerance wrappers from string s.
;;; e.g. "4.623~+.004^-.000~" → "4.623"
(defun DQC:drop-tol (s / out i ch in-tol)
  (setq out "" i 1 in-tol nil)
  (while (<= i (strlen s))
    (setq ch (substr s i 1))
    (cond
      ((= ch "~") (setq in-tol (not in-tol) i (1+ i)))
      (in-tol      (setq i (1+ i)))           ; inside tolerance – skip
      (T           (setq out (strcat out ch) i (1+ i)))))
  out)

;;; Strip leading non-numeric prefix chars from tok.
;;; Handles: %%c %%C %%d  Ø ø R r ( spaces
;;; Stops at first digit, "-", or "."
(defun DQC:strip-pfx (tok / i c)
  (setq tok (DQC:trim tok) i 1)
  ;; skip %%x sequences (%%c, %%d, etc.)
  (while (and (<= (+ i 1) (strlen tok))
              (= (substr tok i 1) "%")
              (= (substr tok (1+ i) 1) "%"))
    (setq i (+ i 3)))
  ;; skip single non-numeric chars that are not -, .
  (while (and (<= i (strlen tok))
              (setq c (substr tok i 1))
              (not (wcmatch c "#"))
              (/= c "-")
              (/= c "."))
    (setq i (1+ i)))
  (if (> i (strlen tok)) "" (substr tok i)))

;;; Extract the nominal number from a raw token.
;;; 1. Drop tolerance wrappers  2. Strip prefix  3. atof
;;; Returns real or nil.
(defun DQC:first-num (tok / v)
  (setq tok (DQC:drop-tol (DQC:trim tok)))
  (setq tok (DQC:strip-pfx tok))
  (if (= (strlen tok) 0) nil
    (progn
      (setq v (atof tok))
      (if (= v 0.0)
        (if (wcmatch (substr tok 1 1) "#") v nil)
        v))))

;;; Same but handles metric bracket content which may have no tol wrapper:
;;; "15.1"  "117.42~+0.10^-0.00~"  "7.90+0.13/-0.00"  "7.90 +0.25"
;;; Strategy:
;;;   1. Drop ~tol~ wrappers (\S stacked tolerances become these)
;;;   2. Strip non-numeric prefix (R, %%c, spaces, etc.)
;;;   3. Truncate at first plain-text tolerance marker (+, -, /) that follows
;;;      a digit/dot, so "7.90+0.25-0.00" → "7.90"
;;;   4. atof the result
(defun DQC:parse-metric (s / v i ch prev)
  (setq s (DQC:drop-tol (DQC:trim s)))
  (setq s (DQC:strip-pfx s))
  (if (= (strlen s) 0) nil
    (progn
      ;; Truncate at a +, -, / or space that appears AFTER we have seen at
      ;; least one digit or dot (i.e. after the nominal number has started).
      ;; This handles un-stacked tolerance suffixes like "+0.25-0.00".
      (setq i 1 prev nil)
      (while (<= i (strlen s))
        (setq ch (substr s i 1))
        (cond
          ;; Digit or dot: mark that the nominal has started, keep going
          ((or (wcmatch ch "#") (= ch "."))
           (setq prev 'digit i (1+ i)))
          ;; +, -, /, space after a digit: tolerance suffix starts here
          ((and prev
                (or (= ch "+") (= ch "-") (= ch "/") (= ch " ")))
           ;; truncate the string at this position
           (setq s (substr s 1 (1- i)) i (1+ (strlen s))))
          ;; minus or plus sign BEFORE a digit
          ((and (null prev) (or (= ch "-") (= ch "+")))
           (setq i (1+ i)))
          ;; anything else (unexpected): stop
          (T (setq s (substr s 1 (1- i)) i (1+ (strlen s))))))
      (if (= (strlen s) 0) nil
        (progn
          (setq v (atof s))
          (if (and (= v 0.0) (not (wcmatch (substr s 1 1) "#"))) nil v))))))

;;; ── Main parser ─────────────────────────────────────────────────────────────

(defun DQC:parse (txt / open close alt-s prim-s p after-s m)
  (setq txt (DQC:trim txt))
  (setq open (DQC:find-char txt "[" 1))
  (if (= open 0) nil
    (progn
      (setq close (DQC:find-char txt "]" (1+ open)))
      (if (= close 0) nil
        (progn
          (setq alt-s  (DQC:trim (substr txt (1+ open) (- close open 1)))
                prim-s (DQC:trim (substr txt 1 (1- open))))
          (setq p (DQC:first-num prim-s))
          ;; If no valid number before bracket, try text after bracket
          (if (null p)
            (progn
              (setq after-s (DQC:trim (substr txt (1+ close))))
              (setq p (DQC:first-num after-s))))
          
          (if (null p)
            nil
            (progn
              (setq m (if (> (strlen (DQC:drop-tol alt-s)) 0)
                        (DQC:parse-metric alt-s)
                        nil))
              (list p (if m m 'EMPTY))))))))))


;;; ============================================================================
;;;  PART 4 – DIMENSION GEOMETRY HELPERS
;;;  We need the position of the dimension text in WCS so we know WHERE to
;;;  place the balloon.  AutoCAD stores this in DXF group 11 for most dim types.
;;; ============================================================================

;;; Return the text mid-point of a dimension/leader in WCS.
(defun DQC:dim-textpt (ename / ed pt obj txpt)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (if (null ed) nil
    (progn
      (setq obj (vlax-ename->vla-object ename))
      (if (wcmatch (cdr (assoc 0 ed)) "*LEADER")
        (progn
          (setq txpt (vl-catch-all-apply 'vlax-get (list obj 'TextLocation)))
          (if (not (vl-catch-all-error-p txpt)) (setq pt txpt))))
      (if (null pt) (setq pt (cdr (assoc 11 ed))))
      (if (null pt) (setq pt (cdr (assoc 10 ed))))
      pt)))

;;; Return dimension text height.
(defun DQC:dim-txth (ename / ed h obj)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (setq h (if ed (cdr (assoc 140 ed)) nil))
  (if (or (null h) (< h 0.001))
    (progn
      (setq obj (vlax-ename->vla-object ename))
      (setq h (vl-catch-all-apply 'vlax-get (list obj 'TextHeight)))
      (if (vl-catch-all-error-p h) (setq h nil))))
  (if (or (null h) (< h 0.001))
    (setq h (getvar "DIMTXT")))
  (if (or (null h) (< h 0.001))
    (setq h 2.5))
  h)

;;; Return DXF-42 measurement from a dimension entity.
(defun DQC:dim-meas (ename / ed)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (if ed (cdr (assoc 42 ed)) nil))

;;; Return the dim style name (DXF 3).
(defun DQC:dim-style (ename / ed)
  (setq ed (vl-catch-all-apply 'entget (list ename)))
  (if (vl-catch-all-error-p ed) (setq ed nil))
  (if ed (cdr (assoc 3 ed)) nil))

;;; Get DIMLFAC from the named dim style.  Returns 1.0 on any failure.
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
              (if (or (vl-catch-all-error-p lf) (null lf) (zerop lf))
                1.0 (abs lf)))))))))


;;; ============================================================================
;;;  PART 5 – BALLOON PLACEMENT
;;;
;;;  A balloon is a single MText entity placed at an offset from the dim text.
;;;  Offset direction: we push slightly to the right and upward so it does not
;;;  sit directly on the dimension line.  For dimensions whose text rotation
;;;  (DXF 53) is non-zero we rotate the offset vector accordingly.
;;;
;;;  MText format codes used:
;;;    \C3;  = green (ACI 3)   for PASS
;;;    \C1;  = red   (ACI 1)   for FAIL
;;;    \H<n>; = text height
;;;    Plain unicode tick/cross: use ASCII stand-ins (+/-) because
;;;    unicode support varies by font.  We use:
;;;      PASS :  "++ "  followed by the expected mm value
;;;      FAIL :  "XX "  followed by "exp " and the expected mm value
;;;    These are readable in any font without unicode dependency.
;;; ============================================================================

;;; Rotate a 2-D vector (dx dy) by angle ang (radians).
(defun DQC:rot2 (dx dy ang / ca sa)
  (setq ca (cos ang) sa (sin ang))
  (list (- (* dx ca) (* dy sa))
        (+ (* dx sa) (* dy ca))))

;;; Place one MText balloon.
;;;  txtpt  – 3D WCS point near which to place it
;;;  txth   – dimension text height (used to scale offset and balloon font)
;;;  dimang – rotation of the dimension text (DXF 53), radians
;;;  label  – the string to display  e.g. "++ 25.40" or "XX exp 25.40"
;;;  layer  – layer name to place balloon on
;;;  Returns the new entity name or nil.
(defun DQC:place-balloon (txtpt txth dimang label layer / offx offy ovec ins
                                  bh bw ent-data)

  ;; Balloon font height: use DQC:TXT-H if set, else 85% of dim text height
  (setq bh (if (and DQC:TXT-H (> DQC:TXT-H 0))
             DQC:TXT-H
             (* txth 0.85)))
  (if (< bh 0.5) (setq bh 0.5))

  ;; Offset magnitude: use DQC:OFFSET if set, else 2.5× balloon height
  (setq offx (* (if (and DQC:OFFSET (> DQC:OFFSET 0))
                  DQC:OFFSET
                  (* bh 2.5))
                1.0)
        offy (* bh 1.2))

  ;; Rotate offset vector by the dimension text angle
  (setq ovec (DQC:rot2 offx offy (if dimang dimang 0.0)))

  ;; Insertion point
  (setq ins (list (+ (car  txtpt) (car  ovec))
                  (+ (cadr txtpt) (cadr ovec))
                  (if (caddr txtpt) (caddr txtpt) 0.0)))

  ;; Build MText content string
  ;; \H sets height, \C sets colour, text follows
  (setq bw (* (strlen label) bh 0.7))   ; approximate width for MText width
  (if (< bw (* bh 3)) (setq bw (* bh 3)))

  ;; Use entmake for MText (entity type MTEXT, DXF 0)
  ;;  1  = raw text (with format codes)
  ;;  10 = insertion point
  ;;  40 = nominal char height
  ;;  41 = reference rectangle width
  ;;  71 = attachment point (7 = middle-left)
  ;;  72 = drawing direction (1 = left to right)
  ;;   8 = layer
  (setq ent-data
    (list
      (cons 0  "MTEXT")
      (cons 100 "AcDbEntity")
      (cons 8  layer)
      (cons 100 "AcDbMText")
      (cons 10 ins)
      (cons 40 bh)
      (cons 41 bw)
      (cons 71 7)
      (cons 72 1)
      (cons 1  label)))

  (if (vl-catch-all-error-p (vl-catch-all-apply 'entmake (list ent-data)))
    nil
    (entlast)))


;;; ============================================================================
;;;  PART 6 – PROCESS ONE DIMENSION
;;;  Returns a list:
;;;    (status  label-string  primary  alt  expected  raw-text)
;;;  where status = 'PASS | 'FAIL | 'SKIP
;;; ============================================================================

(defun DQC:process (ename doc / obj ed meas sname lfac
                              primary-auto raw stripped pair
                              primary alt expected ok
                              txtpt txth dimang label layer
                              ovr g1 alt-on)

  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (vl-catch-all-error-p obj)
    (list 'SKIP "" nil nil nil "")

    (progn
      ;; ── Read dim data ────────────────────────────────────────────────────
      (setq ed    (vl-catch-all-apply 'entget (list ename)))
      (if (vl-catch-all-error-p ed) (setq ed nil))

      (setq meas  (if ed (cdr (assoc 42 ed)) nil))
      (if (null meas) (setq meas 0.0))

      (setq sname (DQC:dim-style ename)
            lfac  (DQC:lfac sname doc))

      ;; The inch value AutoCAD would show when text uses <>
      (setq primary-auto (* (abs meas) lfac))

      ;; ── Read raw text override ────────────────────────────────────────────
      (setq ovr (vl-catch-all-apply 'vla-get-TextOverride (list obj)))
      (if (or (vl-catch-all-error-p ovr) (null ovr) (= ovr ""))
        (progn
          (setq ovr (vl-catch-all-apply 'vla-get-TextString (list obj)))
          (if (vl-catch-all-error-p ovr) (setq ovr ""))))
      
      (setq g1 (if ed (cdr (assoc 1 ed)) nil))
      (if (null g1) (setq g1 ""))

      (setq alt-on (vl-catch-all-apply 'vla-get-AlternateUnits (list obj)))
      (if (vl-catch-all-error-p alt-on) (setq alt-on nil))

      ;; Prefer TextOverride/TextString; fall back to DXF 1; synthesize if Alternate Units are natively ON
      (setq raw
        (cond
          ((> (strlen (DQC:trim ovr)) 0) ovr)
          ((and (= (type g1) 'STR) (> (strlen (DQC:trim g1)) 0)) g1)
          ((eq alt-on :vlax-true) "<> []")
          (T "")))

      ;; Strip mtext codes; substitute <> with the auto inch value and [] with alt auto value
      (setq stripped (DQC:strip raw primary-auto (* primary-auto DQC:MM/IN)))

      ;; Parse  "primary [alt]"
      (setq pair (DQC:parse stripped))

      ;; ── Geometry: where is the dim text? ────────────────────────────────
      (setq txtpt (DQC:dim-textpt ename))
      (setq txth  (DQC:dim-txth  ename))
      (setq dimang (vl-catch-all-apply 'vla-get-TextRotation (list obj)))
      (if (vl-catch-all-error-p dimang)
        (setq dimang (if ed (cdr (assoc 53 ed)) 0.0)))
      (if (null dimang) (setq dimang 0.0))

      ;; ── Classify and place balloon ───────────────────────────────────────
      (cond

        ;; No [ ] bracket → single-unit dim → skip silently
        ((null pair)
         (list 'SKIP "" nil nil nil raw))

        ;; Bracket present but mm slot is empty
        ((eq (cadr pair) 'EMPTY)
         (setq primary  (car pair)
               expected (* (abs primary) DQC:MM/IN)
               label    (strcat (rtos primary 2 3) "\" [?] XX (exp " (rtos expected 2 2) ")")
               layer    DQC:FAIL-LAYER)
         (if txtpt
           (DQC:place-balloon txtpt txth dimang label layer))
         (list 'FAIL label primary nil expected raw))

        ;; Both values present → verify
        (T
         (setq primary  (car  pair)
               alt      (cadr pair)
               expected (* (abs primary) DQC:MM/IN)
               ok       (DQC:ok? primary alt DQC:MM/IN))
         (if ok
           (setq label (strcat (rtos primary 2 3) "\" [" (rtos alt 2 2) "] OK")
                 layer DQC:PASS-LAYER)
           (setq label (strcat (rtos primary 2 3) "\" [" (rtos alt 2 2) "] XX (exp " (rtos expected 2 2) ")")
                 layer DQC:FAIL-LAYER))
         (if txtpt
           (DQC:place-balloon txtpt txth dimang label layer))
         (list (if ok 'PASS 'FAIL) label
               primary alt expected raw))
      )
    )
  )
)


;;; ============================================================================
;;;  PART 7 – DCL DIALOG
;;; ============================================================================

(defun DQC:write-dcl ( / path f)
  (setq path (strcat (getvar "TEMPPREFIX") "dim_qc_v4.dcl"))
  (setq f (open path "w"))

  (write-line "dqc_settings : dialog {" f)
  (write-line "  label = \"DIM QC v4.0  –  Balloon Edition\";" f)

  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Conversion factor\";" f)
  (write-line "    : row {" f)
  (write-line "      : text  { label = \"1 inch  =  ? mm  (normally 25.4) :\"; }" f)
  (write-line "      : edit_box { key = \"factor\"; width = 10; }" f)
  (write-line "    }" f)
  (write-line "  }" f)

  (write-line "  : boxed_column {" f)
  (write-line "    label = \"Tolerances  (what counts as a match)\";" f)
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
  (write-line "    : text { label = \"  n.nn\\\" [nn.n] OK = PASS (on layer DIM_QC_PASS, colour green)\"; }" f)
  (write-line "    : text { label = \"  n.nn\\\" [nn.n] XX  = FAIL wrong mm value (layer DIM_QC_FAIL, red)\"; }" f)
  (write-line "    : text { label = \"  n.nn\\\" [?] XX     = FAIL mm bracket empty (layer DIM_QC_FAIL, red)\"; }" f)
  (write-line "    : text { label = \" \"; }" f)
  (write-line "    : text { label = \"To hide balloons: freeze layers DIM_QC_PASS and DIM_QC_FAIL\"; }" f)
  (write-line "    : text { label = \"To remove balloons: run  DIMQC-RESET\"; }" f)
  (write-line "  }" f)

  (write-line "  : row {" f)
  (write-line "    : button { key = \"run\";    label = \"Run QC + Place Balloons\"; is_default = true; width = 24; }" f)
  (write-line "    : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; width = 12; }" f)
  (write-line "  }" f)
  (write-line "}" f)

  ;; ── Results dialog ───────────────────────────────────────────────────────
  (write-line "" f)
  (write-line "dqc_results : dialog {" f)
  (write-line "  label = \"DIM QC v4.0  –  Results\";" f)
  (write-line "  : text { key = \"sum_line\"; label = \" \"; }" f)
  (write-line "  : list_box {" f)
  (write-line "    key        = \"res_list\";" f)
  (write-line "    label      = \"All dual-unit dimensions checked:\";" f)
  (write-line "    height     = 22;" f)
  (write-line "    width      = 72;" f)
  (write-line "    multiple_select = false;" f)
  (write-line "  }" f)
  (write-line "  : text { label = \"Balloons have been placed in the drawing.  Plot as normal.\"; }" f)
  (write-line "  : text { label = \"Freeze DIM_QC_PASS / DIM_QC_FAIL layers to hide, or run DIMQC-RESET to remove.\"; }" f)
  (write-line "  ok_cancel;" f)
  (write-line "}" f)

  (close f)
  path)


;;; ============================================================================
;;;  PART 8 – MAIN COMMAND  C:DIMQC
;;; ============================================================================

(defun C:DIMQC ( / dcl-path dcl-id action
                   doc ss len i ename res
                   total pass fail skip lines sumstr
                   f-str r-str a-str h-str o-str)

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; ── 1. Write & load DCL ──────────────────────────────────────────────────
  (setq dcl-path (DQC:write-dcl))
  (setq dcl-id   (load_dialog dcl-path))
  (if (< dcl-id 0)
    (progn (alert "Cannot load DCL file.") (exit)))

  (if (not (new_dialog "dqc_settings" dcl-id))
    (progn (unload_dialog dcl-id) (alert "Cannot open settings dialog.") (exit)))

  ;; Pre-fill with current settings
  (set_tile "factor"  (rtos DQC:MM/IN   2 4))
  (set_tile "rel_tol" (rtos (* DQC:REL-TOL 100.0) 2 1))
  (set_tile "abs_tol" (rtos DQC:ABS-TOL 2 3))
  (set_tile "txth"    (if (and DQC:TXT-H (> DQC:TXT-H 0)) (rtos DQC:TXT-H 2 3) "0"))
  (set_tile "offset"  (if (and DQC:OFFSET (> DQC:OFFSET 0)) (rtos DQC:OFFSET 2 3) "0"))

  (setq action "cancel")
  (action_tile "run"    "(setq action \"run\")    (done_dialog 1)")
  (action_tile "cancel" "(setq action \"cancel\") (done_dialog 0)")

  (start_dialog)

  ;; Capture tile values BEFORE unloading
  (setq f-str (get_tile "factor")
        r-str (get_tile "rel_tol")
        a-str (get_tile "abs_tol")
        h-str (get_tile "txth")
        o-str (get_tile "offset"))

  (unload_dialog dcl-id)

  (if (= action "cancel") (progn (princ "\n Cancelled.\n") (princ) (exit)))

  ;; Apply settings
  (if (and f-str (> (strlen f-str) 0)) (setq DQC:MM/IN   (atof f-str)))
  (if (and r-str (> (strlen r-str) 0)) (setq DQC:REL-TOL (/ (atof r-str) 100.0)))
  (if (and a-str (> (strlen a-str) 0)) (setq DQC:ABS-TOL (atof a-str)))
  (setq DQC:TXT-H  (if (and h-str (> (atof h-str) 0)) (atof h-str) nil))
  (setq DQC:OFFSET (if (and o-str (> (atof o-str) 0)) (atof o-str) nil))

  ;; ── 2. Ensure layers exist ───────────────────────────────────────────────
  (DQC:ensure-layer DQC:PASS-LAYER DQC:PASS-COLOR doc)
  (DQC:ensure-layer DQC:FAIL-LAYER DQC:FAIL-COLOR doc)

  ;; ── 3. Remove any old balloons ───────────────────────────────────────────
  (DQC:erase-balloons doc)

  ;; ── 4. Find all DIMENSION entities (and MULTILEADERs) ───────────────────
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER"))))
  (if (null ss)
    (progn (alert "No DIMENSION entities found in this drawing.") (princ) (exit)))

  (setq len   (sslength ss)
        total 0  pass 0  fail 0  skip 0
        lines (list))

  ;; ── 5. Process each dim ─────────────────────────────────────────────────
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
                (cond
                  ((null (nth 3 res)) "[ ] EMPTY")
                  (T (strcat (rtos (nth 3 res) 2 3) " mm")))
                "  (exp " (if (nth 4 res) (rtos (nth 4 res) 2 3) "?") " mm)"
                (if (null (nth 3 res)) "  <- mm NOT ENTERED" "  <- MISMATCH"))))))
      (T (setq skip (1+ skip))))
    (setq i (1+ i)))

  ;; Regenerate so balloons appear
  (vla-Regen doc acAllViewports)

  ;; ── 6. Results dialog ────────────────────────────────────────────────────
  (setq sumstr
    (strcat "Checked: " (itoa (+ pass fail))
            "   PASS: " (itoa pass)
            "   FAIL: " (itoa fail)
            "   Skipped (single-unit): " (itoa skip)))

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
;;;  PART 9 – DIMQC-RESET  (erase all balloons)
;;; ============================================================================

;;; Erase all MText objects on DIM_QC_PASS and DIM_QC_FAIL layers.
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
  (vla-Regen doc acAllViewports)
  (princ (strcat "\n Removed " (itoa n) " balloon(s).\n\n"))
  (princ))


;;; ============================================================================
;;;  PART 10 – DIMQC-DIAG  (diagnostic dump, no GUI)
;;; ============================================================================

(defun C:DIMQC-DIAG ( / doc ss len i ename obj ed sname lfac
                         meas raw stripped pair primary alt expected
                         ovr2 g alt-on)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (princ "\n========== DIMQC DIAGNOSTIC ==========\n")
  (setq ss (ssget "X" (list (cons 0 "DIMENSION,MULTILEADER"))))
  (if (null ss) (progn (princ " No DIMENSION or MULTILEADER entities.\n\n") (princ) (exit)))
  (setq len (sslength ss))
  (princ (strcat " " (itoa len) " entities found.\n\n"))
  (setq i 0)
  (while (< i len)
    (setq ename (ssname ss i))
    (setq obj   (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
    (setq ed    (vl-catch-all-apply 'entget (list ename)))
    (if (vl-catch-all-error-p ed) (setq ed nil))
    (setq sname (DQC:dim-style ename)
          lfac  (DQC:lfac sname doc)
          meas  (DQC:dim-meas ename))
    (if (null meas) (setq meas 0.0))
    
    (setq ovr2 (vl-catch-all-apply 'vla-get-TextOverride (list obj)))
    (if (or (vl-catch-all-error-p ovr2) (null ovr2) (= ovr2 ""))
      (progn
        (setq ovr2 (vl-catch-all-apply 'vla-get-TextString (list obj)))
        (if (vl-catch-all-error-p ovr2) (setq ovr2 ""))))
        
    (setq g (if ed (cdr (assoc 1 ed)) nil))
    (if (null g) (setq g ""))
    
    (setq alt-on (vl-catch-all-apply 'vla-get-AlternateUnits (list obj)))
    (if (vl-catch-all-error-p alt-on) (setq alt-on nil))

    (setq raw
      (cond
        ((> (strlen (DQC:trim ovr2)) 0) ovr2)
        ((and (= (type g) 'STR) (> (strlen (DQC:trim g)) 0)) g)
        ((eq alt-on :vlax-true) "<> []")
        (T "")))
        
    (setq stripped (DQC:strip raw (* (abs meas) lfac) (* (* (abs meas) lfac) DQC:MM/IN)))
    (setq pair     (DQC:parse stripped))
    (princ (strcat "ITEM #" (itoa (1+ i)) " [" (cdr (assoc 0 ed)) "]\n"))
    (princ (strcat "  Style   : " (if sname sname "?") "\n"))
    (princ (strcat "  VLA txt : \"" ovr2 "\"\n"))
    (princ (strcat "  DXF 1   : \"" (if (= (type g) 'STR) g "") "\"\n"))
    (princ (strcat "  Raw used: \"" raw "\"\n"))
    (princ (strcat "  Stripped: \"" stripped "\"\n"))
    (princ (strcat "  Meas    : " (rtos meas 2 6) "\n"))
    (princ (strcat "  LFAC    : " (rtos lfac 2 4) "\n"))
    (princ (strcat "  inch val: " (rtos (* (abs meas) lfac) 2 6) "\"\n"))
    (princ
      (strcat "  Parse   : "
        (cond
          ((null pair) "no [ ] bracket  ->  SKIP")
          ((eq (cadr pair) 'EMPTY)
           (strcat "primary=" (rtos (car pair) 2 4)
                   "\"  mm=EMPTY  exp="
                   (rtos (* (abs (car pair)) DQC:MM/IN) 2 3) " mm"))
          (T
           (setq primary (car pair) alt (cadr pair)
                 expected (* (abs primary) DQC:MM/IN))
           (strcat "primary=" (rtos primary 2 4) "\""
                   "  mm=" (rtos alt 2 3)
                   "  exp=" (rtos expected 2 3)
                   "  diff=" (rtos (abs (- expected alt)) 2 4)
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
(princ " DIM QC v4.0  (Balloon Edition)  loaded.\n")
(princ "\n")
(princ "   DIMQC        Open GUI and run QC check\n")
(princ "   DIMQC-RESET  Remove all balloons from drawing\n")
(princ "   DIMQC-DIAG   Command-line diagnostic dump\n")
(princ "\n")
(princ " Balloons are MText objects on dedicated layers.\n")
(princ " They plot in any PDF with any CTB automatically.\n")
(princ " Freeze DIM_QC_PASS / DIM_QC_FAIL to hide them.\n")
(princ "================================================\n\n")
(princ)
