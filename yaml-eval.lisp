;;;; yaml-eval.lisp — CL/SBCL evaluator for YAML 1.2 s-expr grammar
;;;; PEG parser interpreter with packrat memoization.

(defpackage #:yaml-eval
  (:use #:cl)
  (:export #:load-yaml-grammar #:yaml-parse #:yaml-parse-file
           #:main #:run-tests #:*trace*))
(in-package #:yaml-eval)

;;; Muffle style warnings for forward references
(declaim (sb-ext:muffle-conditions style-warning))

;;; ═══════════════════════════════════════════════════════════════════
;;; 1. INPUT STREAM
;;; ═══════════════════════════════════════════════════════════════════

(defstruct (inp (:constructor %inp (str pos line col)))
  (str "" :type string :read-only t)
  (pos 0  :type fixnum)
  (line 1 :type fixnum)
  (col  0 :type fixnum))

(defun mkinp (s) (%inp s 0 1 0))

(declaim (inline ieof ipk icp isol))
(defun ieof (i) (>= (inp-pos i) (length (inp-str i))))
(defun ipk  (i) (unless (ieof i) (char (inp-str i) (inp-pos i))))
(defun icp  (i) (let ((c (ipk i))) (when c (char-code c))))
(defun isol (i) (zerop (inp-col i)))

(defun iadv (i)
  (if (ieof i) (values i nil)
      (let* ((c (ipk i)) (nl (char= c #\Newline)))
        (values (%inp (inp-str i) (1+ (inp-pos i))
                      (if nl (1+ (inp-line i)) (inp-line i))
                      (if nl 0 (1+ (inp-col i))))
                c))))

(defun iadv-n (i n)
  (if (> (+ (inp-pos i) n) (length (inp-str i))) (values i nil)
      (let ((s (subseq (inp-str i) (inp-pos i) (+ (inp-pos i) n)))
            (cur i))
        (dotimes (_ n) (setf cur (iadv cur)))
        (values cur s))))

;;; ═══════════════════════════════════════════════════════════════════
;;; 2. PARSE RESULT
;;; ═══════════════════════════════════════════════════════════════════

(defstruct (ok (:constructor ok (val rest &optional tag ast))) val rest tag ast)
(defstruct (fa (:constructor fa (msg pos))) msg (pos 0 :type fixnum))

;;; ═══════════════════════════════════════════════════════════════════
;;; 3. GRAMMAR STORAGE
;;; ═══════════════════════════════════════════════════════════════════

(defstruct gram name (rules (make-hash-table :test 'equal)))
(defstruct rdef num name params body)

;;; ═══════════════════════════════════════════════════════════════════
;;; 4. GRAMMAR LOADER
;;; ═══════════════════════════════════════════════════════════════════
;;; Preprocess Scheme 'x' and '\\x' char literals into integer codes,
;;; then use CL READ.

(defun preprocess-text (text)
  "Replace Scheme char literals 'x' and '\\\\x' with their char codes."
  (with-output-to-string (out)
    (let ((i 0) (len (length text)))
      (loop
        (when (>= i len) (return))
        (let ((c (char text i)))
          (cond
            ;; 4-char escaped: '\\x' where x is the escaped char
            ((and (char= c #\')
                  (< (+ i 3) len)
                  (char= (char text (+ i 1)) #\\)
                  (char= (char text (+ i 3)) #\'))
             (let ((esc (char text (+ i 2))))
               (format out "~D" (char-code
                                 (case esc
                                   (#\n #\Newline) (#\t #\Tab)
                                   (#\\ #\\) ;; '\\' → backslash
                                   (t esc)))))
             (incf i 4))
            ;; 3-char simple: 'x'
            ((and (char= c #\')
                  (< (+ i 2) len)
                  (char= (char text (+ i 2)) #\'))
             (format out "~D" (char-code (char text (+ i 1))))
             (incf i 3))
            (t (write-char c out) (incf i))))))))

(defun load-grammar-sexps (path)
  (let* ((raw (with-open-file (f path :external-format :utf-8)
                (let ((s (make-string (file-length f))))
                  (read-sequence s f) s)))
         (processed (preprocess-text raw))
         (*readtable* (copy-readtable))
         (*package* (find-package :yaml-eval)))
    (with-input-from-string (in processed)
      (loop for form = (read in nil :eof)
            until (eq form :eof) collect form))))

;;; ── Rule extraction ─────────────────────────────────────────────────

(defun grammar-kw-p (s)
  (member s '(ALT SEQ CHAR HEX RANGE REF STAR PLUS OPT NOT MINUS
              EMPTY REPEAT SWITCH STR LOOKAHEAD LOOKBEHIND
              STARTOFLINE ENDOFINPUT)))

(defun param-list-p (form)
  (and (listp form) (every #'symbolp form) (notany #'grammar-kw-p form)))

(defun extract-rule (sexp)
  (when (and (listp sexp) (eq (car sexp) 'RULE))
    (let ((num (cadr sexp)) (name (caddr sexp)) (rest (cdddr sexp)))
      (multiple-value-bind (params body-forms)
          (if (and rest (listp (car rest)) (param-list-p (car rest)))
              (values (car rest) (cdr rest))
              (values nil rest))
        (make-rdef :num num :name name :params params
                   :body (if (= 1 (length body-forms))
                             (car body-forms)
                             (cons 'SEQ body-forms)))))))

(defun load-yaml-grammar (path)
  (let* ((forms (load-grammar-sexps path))
         (top (car forms))
         (g (make-gram :name (cadr top)
                       :rules (make-hash-table :test 'equal))))
    (dolist (sexp (cddr top))
      (when (listp sexp)
        (let ((rd (extract-rule sexp)))
          (when rd (setf (gethash (rdef-name rd) (gram-rules g)) rd)))))
    (format t "~&; Loaded ~D rules~%" (hash-table-count (gram-rules g)))
    g))

;;; ═══════════════════════════════════════════════════════════════════
;;; 5. EVALUATOR
;;; ═══════════════════════════════════════════════════════════════════

(defvar *trace* nil)
(defvar *memo* nil)
(defvar *depth* 0)
(defparameter *max-depth* 300)

(defun resolve-char (x)
  (cond ((integerp x) x)
        ((characterp x) (char-code x))
        ((symbolp x)
         (let ((n (symbol-name x)))
           (cond ((string= n "SQUOTE") 39)
                 ((string= n "DQUOTE") 34)
                 ((= (length n) 1) (char-code (char n 0)))
                 (t (error "Can't resolve char: ~S" x)))))
        (t (error "Can't resolve char: ~S" x))))

(defun resolve-hex (x)
  "Resolve hex value. Note: CL reader turns (Hex 20) into integer 20,
   but it means 0x20=32. We re-parse the decimal repr as hex."
  (cond ((integerp x) (parse-integer (format nil "~D" x) :radix 16))
        ((symbolp x) (parse-integer (symbol-name x) :radix 16))
        (t (error "Can't resolve hex: ~S" x))))

(defun ctx= (a b) (string-equal (string a) (string b)))

;;; ── Arithmetic for rule params ──────────────────────────────────────

(defun eval-arith (expr env)
  (cond
    ((integerp expr) expr)
    ((symbolp expr)
     (let ((b (assoc expr env :test #'eq)))
       (if b (let ((v (cdr b)))
               (if (integerp v) v
                   ;; Context values aren't integers — propagate
                   (error "Non-int ~S=~S" expr v)))
           ;; 'm' is auto-detected indentation in block rules; default to 1
           (if (eq expr 'M) 1
               (error "Unbound ~S" expr)))))
    ((and (listp expr) (eq (car expr) '+))
     (reduce #'+ (mapcar (lambda (e) (eval-arith e env)) (cdr expr))))
    ((and (listp expr) (eq (car expr) '-))
     (let ((vs (mapcar (lambda (e) (eval-arith e env)) (cdr expr))))
       (if (= 1 (length vs)) (- (car vs))
           (- (car vs) (reduce #'+ (cdr vs))))))
    (t (error "Can't eval arith: ~S" expr))))

(defun eval-arg (expr env)
  "Evaluate a call argument (integer, context symbol, or expression)."
  (cond
    ((integerp expr) expr)
    ((and (symbolp expr)
          (member (symbol-name expr)
                  '("BLOCK-IN" "BLOCK-OUT" "FLOW-IN" "FLOW-OUT"
                    "BLOCK-KEY" "FLOW-KEY" "STRIP" "CLIP" "KEEP" "EMPTY")
                  :test #'string-equal))
     expr)
    ((symbolp expr)
     (let ((b (assoc expr env :test #'eq)))
       (if b (cdr b)
           ;; M is auto-detected indentation; default to 1
           (if (eq expr 'M) 1 expr))))
    ((listp expr)
     (cond
       ((eq (car expr) 'IN-FLOW)
        (let ((c (eval-arg (cadr expr) env)))
          (cond ((or (ctx= c 'FLOW-OUT) (ctx= c 'FLOW-IN)) 'FLOW-IN)
                ((or (ctx= c 'BLOCK-KEY) (ctx= c 'FLOW-KEY)) 'FLOW-KEY)
                (t 'FLOW-IN))))
       ((eq (car expr) 'SEQ-SPACES)
        (let ((n (eval-arith (cadr expr) env))
              (c (eval-arg (caddr expr) env)))
          (if (ctx= c 'BLOCK-OUT) (1- n) n)))
       (t (handler-case (eval-arith expr env) (error () expr)))))
    (t expr)))

;;; ── Core dispatch ───────────────────────────────────────────────────

(defun ev (expr inp env gram)
  (let ((*depth* (1+ *depth*)))
    (when (> *depth* *max-depth*)
      (return-from ev (fa "max depth" (inp-pos inp))))
    (cond
      ((null expr)       (ok "" inp))
      ((eq expr 'EMPTY)  (ok "" inp))
      ((integerp expr)   (ev-cc expr inp))
      ((atom expr)
       (if (gethash expr (gram-rules gram))
           (ev-ref expr nil inp env gram)
           (fa (format nil "Unknown: ~S" expr) (inp-pos inp))))
      (t (ev-form (car expr) (cdr expr) inp env gram)))))

(defun ev-form (op args inp env gram)
  (case op
    (CHAR      (ev-char args inp env))
    (HEX       (ev-hex args inp))
    (RANGE     (ev-range args inp))
    (STR       (ev-str args inp))
    (SEQ       (ev-seq args inp env gram))
    (ALT       (ev-alt args inp env gram))
    (STAR      (ev-star args inp env gram))
    (PLUS      (ev-plus args inp env gram))
    (OPT       (ev-opt args inp env gram))
    (REF       (ev-ref (car args) (cdr args) inp env gram))
    (NOT       (ev-not args inp env gram))
    (MINUS     (ev-minus args inp env gram))
    (EMPTY     (ok "" inp))
    (REPEAT    (ev-repeat args inp env gram))
    (SWITCH    (ev-switch args inp env gram))
    (LOOKAHEAD (let ((r (ev (car args) inp env gram)))
                 (if (ok-p r) (ok "" inp) r)))
    (LOOKBEHIND 
     ;; Check if the character before current position matches the argument
     (let ((p (inp-pos inp)))
       (if (zerop p) 
           (fa "lookbehind at start" p)
           ;; Create a temporary input at pos-1, try to match the argument
           (let ((tmp (%inp (inp-str inp) (1- p)
                            (inp-line inp) (max 0 (1- (inp-col inp))))))
             (let ((r (ev (car args) tmp env gram)))
               (if (ok-p r)
                   (ok "" inp)  ; match succeeded, don't consume
                   (fa "lookbehind fail" p)))))))
    (STARTOFLINE (if (isol inp) (ok "" inp)
                     (fa "not SOL" (inp-pos inp))))
    (ENDOFINPUT  (if (ieof inp) (ok "" inp)
                     (fa "not EOF" (inp-pos inp))))
    ;; ── Code-as-data extensions for YAML's non-PEG parts ──
    (BUILD       (ev-build args inp env gram))
    (COLLECT     (ev-collect args inp env gram))
    (SCALAR      (ev-scalar args inp env gram))
    (LET         (ev-let args inp env gram))
    (DETECT-INDENT (ev-detect-indent args inp env))
    (PARSE-INT   (ev-parse-int args inp env gram))
    (PARSE-SYM   (ev-parse-sym args inp env gram))
    (VAL         (ok "" inp (car args)))  ; produce a value without consuming
    (t (if (gethash op (gram-rules gram))
           (ev-ref op args inp env gram)
           (fa (format nil "Unknown op: ~S" op) (inp-pos inp))))))

;;; ── Primitives ──────────────────────────────────────────────────────

(defun ev-cc (code inp)
  (let ((cur (icp inp)))
    (if (and cur (= cur code))
        (multiple-value-bind (ni ch) (iadv inp) (ok (string ch) ni))
        (fa (format nil "want ~D got ~A" code cur) (inp-pos inp)))))

(defun ev-char (args inp env)
  (let* ((x (car args))
         (x (if (and (symbolp x) (assoc x env :test #'eq))
                (cdr (assoc x env :test #'eq)) x)))
    (ev-cc (resolve-char x) inp)))

(defun ev-hex (args inp) (ev-cc (resolve-hex (car args)) inp))

(defun ev-range (args inp)
  (flet ((val (a) (if (and (listp a) (eq (car a) 'HEX))
                      (resolve-hex (cadr a)) (resolve-char a))))
    (let ((lo (val (car args))) (hi (val (cadr args))) (cur (icp inp)))
      (if (and cur (<= lo cur hi))
          (multiple-value-bind (ni ch) (iadv inp) (ok (string ch) ni))
          (fa (format nil "range ~X-~X" lo hi) (inp-pos inp))))))

(defun ev-str (args inp)
  (let* ((target (let ((x (car args))) (if (stringp x) x (string x))))
         (len (length target)))
    (multiple-value-bind (ni matched) (iadv-n inp len)
      (if (and matched (string= matched target))
          (ok matched ni)
          (fa (format nil "want ~S" target) (inp-pos inp))))))

;;; ── Combinators ─────────────────────────────────────────────────────

(defun ev-seq (exprs inp env gram)
  (let ((cur inp) (parts nil) (asts nil))
    (dolist (e exprs)
      (let ((r (ev e cur env gram)))
        (if (ok-p r)
            (progn (push (ok-val r) parts)
                   (when (ok-ast r) (push (ok-ast r) asts))
                   (setf cur (ok-rest r)))
            (return-from ev-seq r))))
    (let ((al (nreverse asts)))
      (ok (apply #'concatenate 'string (nreverse parts)) cur nil
          (cond ((null al) nil) ((null (cdr al)) (car al)) (t al))))))

(defun ev-alt (exprs inp env gram)
  (let ((best nil))
    (dolist (e exprs)
      (let ((r (ev e inp env gram)))
        (when (ok-p r) (return-from ev-alt r))
        (when (or (null best) (> (fa-pos r) (fa-pos best)))
          (setf best r))))
    (or best (fa "no alt" (inp-pos inp)))))

(defun ev-star (args inp env gram)
  (let ((expr (car args)) (cur inp) (parts nil) (asts nil))
    (loop (let ((r (ev expr cur env gram)))
            (if (and (ok-p r) (> (inp-pos (ok-rest r)) (inp-pos cur)))
                (progn (push (ok-val r) parts)
                       (when (ok-ast r) (push (ok-ast r) asts))
                       (setf cur (ok-rest r)))
                (return (ok (apply #'concatenate 'string (nreverse parts)) cur nil
                            (nreverse asts))))))))

(defun ev-plus (args inp env gram)
  (let* ((expr (car args)) (first (ev expr inp env gram)))
    (if (fa-p first) first
        (let ((cur (ok-rest first))
              (parts (list (ok-val first)))
              (asts (when (ok-ast first) (list (ok-ast first)))))
          (loop (let ((r (ev expr cur env gram)))
                  (if (and (ok-p r) (> (inp-pos (ok-rest r)) (inp-pos cur)))
                      (progn (push (ok-val r) parts)
                             (when (ok-ast r) (push (ok-ast r) asts))
                             (setf cur (ok-rest r)))
                      (return (ok (apply #'concatenate 'string (nreverse parts))
                                  cur nil (nreverse asts))))))))))

(defun ev-opt (args inp env gram)
  (let ((r (ev (car args) inp env gram)))
    (if (ok-p r) r (ok "" inp nil nil))))

(defun ev-repeat (args inp env gram)
  (let ((n (eval-arith (car args) env)) (expr (cadr args))
        (cur inp) (parts nil))
    (dotimes (_ n)
      (let ((r (ev expr cur env gram)))
        (if (ok-p r) (progn (push (ok-val r) parts) (setf cur (ok-rest r)))
            (return-from ev-repeat r))))
    (ok (apply #'concatenate 'string (nreverse parts)) cur)))

(defun ev-not (args inp env gram)
  (if (fa-p (ev (car args) inp env gram))
      (ok "" inp)
      (fa "neg-lookahead hit" (inp-pos inp))))

(defun ev-minus (args inp env gram)
  (let ((ra (ev (car args) inp env gram)))
    (if (fa-p ra) ra
        (let ((rb (ev (cadr args) inp env gram)))
          (if (and (ok-p rb) (= (inp-pos (ok-rest rb)) (inp-pos (ok-rest ra))))
              (fa "excluded" (inp-pos inp))
              ra)))))

(defun ev-switch (args inp env gram)
  (let* ((param (car args)) (cases (cdr args))
         (val (let ((b (assoc param env :test #'eq)))
                (if b (cdr b)
                    (return-from ev-switch
                      (fa (format nil "switch unbound ~S" param) (inp-pos inp)))))))
    (dolist (c cases (fa (format nil "no case ~S=~S" param val) (inp-pos inp)))
      (when (ctx= (car c) val)
        (return (ev (cadr c) inp env gram))))))

;;; ── Rule reference with memoization ─────────────────────────────────

;;; ── Code-as-data: Let, Detect-Indent, Parse-Int, Parse-Sym, Val ────

(defun ev-let (args inp env gram)
  "(Let ((var1 expr1) (var2 expr2) ...) body)
   Parse each expr in sequence, bind result values to vars, parse body."
  (let ((bindings (car args))
        (body (cadr args))
        (cur inp)
        (new-env env))
    (dolist (b bindings)
      (let* ((var (car b))
             (expr (cadr b))
             (r (ev expr cur new-env gram)))
        (when (fa-p r)
          (return-from ev-let r))
        ;; The 'tag' field carries computed values (from Val, Parse-Int, etc)
        ;; The matched text is the fallback
        (let ((val (or (ok-tag r)
                       (let ((txt (ok-val r)))
                         (if (and (stringp txt) (every #'digit-char-p txt)
                                  (plusp (length txt)))
                             (parse-integer txt)
                             txt)))))
          (push (cons var val) new-env)
          (setf cur (ok-rest r)))))
    (ev body cur new-env gram)))

(defun ev-detect-indent (args inp env)
  "(Detect-Indent n) — detect indentation level m from current position.
   Counts leading spaces from current position (or next content line if
   current position is not at start of line or is on an empty line).
   Returns ok with tag = max(1, spaces - n)."
  (let* ((n (eval-arith (car args) env))
         (s (inp-str inp))
         (len (length s))
         (i (inp-pos inp)))
    ;; Count leading spaces from current position
    (let ((spaces 0))
      (loop while (and (< (+ i spaces) len)
                       (char= (char s (+ i spaces)) #\Space))
            do (incf spaces))
      ;; If we found content on this line (non-space, non-newline, non-eof)
      (when (and (< (+ i spaces) len)
                 (not (char= (char s (+ i spaces)) #\Newline)))
        (return-from ev-detect-indent
          (ok "" inp (max 1 (- spaces n))))))
    ;; Otherwise scan forward to next content line
    (loop while (and (< i len) (not (char= (char s i) #\Newline)))
          do (incf i))
    (loop while (< i len) do
      (when (char= (char s i) #\Newline) (incf i))
      (when (>= i len) (return))
      (let ((spaces 0))
        (loop while (and (< (+ i spaces) len)
                         (char= (char s (+ i spaces)) #\Space))
              do (incf spaces))
        (let ((next-pos (+ i spaces)))
          (if (or (>= next-pos len)
                  (char= (char s next-pos) #\Newline))
              (setf i next-pos)
              (return-from ev-detect-indent
                (ok "" inp (max 1 (- spaces n))))))))
    (ok "" inp 1)))

(defun ev-parse-int (args inp env gram)
  "(Parse-Int expr) — parse expr, interpret matched text as integer.
   Result tag is the integer value."
  (let ((r (ev (car args) inp env gram)))
    (if (fa-p r) r
        (let* ((txt (ok-val r))
               (n (if (and (stringp txt) (plusp (length txt))
                           (every #'digit-char-p txt))
                      (parse-integer txt)
                      0)))
          (ok (ok-val r) (ok-rest r) n)))))

(defun ev-parse-sym (args inp env gram)
  "(Parse-Sym pattern symbol) — parse pattern, tag result with symbol."
  (let ((r (ev (car args) inp env gram)))
    (if (fa-p r) r
        (ok (ok-val r) (ok-rest r) (cadr args)))))


;;; ── AST-building combinators ────────────────────────────────────────

(defun ev-build (args inp env gram)
  "(Build type expr) — parse expr, wrap result AST as (type child1 child2 ...).
   If child-ast is a list of AST nodes, they become children.
   If child-ast is a single AST node (a Build result or scalar), it's the only child."
  (let ((type (car args)) (expr (cadr args)))
    (let ((r (ev expr inp env gram)))
      (if (fa-p r) r
          (let* ((child (ok-ast r))
                 (children
                   (cond
                     ((null child) nil)
                     ;; A Build node (cons starting with a symbol) → single child
                     ((and (consp child) (symbolp (car child)))
                      (list child))
                     ;; A list of AST nodes → use as children
                     ((and (consp child) (listp child))
                      (if (every (lambda (c) (or (null c) (stringp c)
                                                 (and (consp c) (symbolp (car c)))))
                                 child)
                          child         ; list of nodes
                          (list child))) ; single complex node
                     ;; Scalar string → single child
                     ((stringp child) (list child))
                     (t (list child)))))
            (ok (ok-val r) (ok-rest r) (ok-tag r) (cons type children)))))))

(defun ev-collect (args inp env gram)
  "(Collect expr) — parse expr (typically Star/Plus), gather child ASTs into a list."
  (let ((r (ev (car args) inp env gram)))
    (if (fa-p r) r
        ;; The ast might already be a list from Star/Plus gathering
        (ok (ok-val r) (ok-rest r) (ok-tag r) (ok-ast r)))))

(defun ev-scalar (args inp env gram)
  "(Scalar expr) — parse expr, use matched text as a scalar AST leaf."
  (let ((r (ev (car args) inp env gram)))
    (if (fa-p r) r
        (ok (ok-val r) (ok-rest r) (ok-tag r) (ok-val r)))))

(defun ev-ref (name call-args inp env gram)
  (let* ((resolved (mapcar (lambda (a) (eval-arg a env)) call-args))
         (mkey (list name resolved (inp-pos inp)))
         (cached (gethash mkey *memo*)))
    (when cached (return-from ev-ref cached))
    (let ((rd (gethash name (gram-rules gram))))
      (unless rd
        (return-from ev-ref (fa (format nil "undef: ~S" name) (inp-pos inp))))
      (let ((new-env env))
        (when (rdef-params rd)
          (loop for p in (rdef-params rd) for a in resolved
                do (push (cons p a) new-env)))
        (when (and *trace* (or (eq *trace* t) (member name *trace*)))
          (format t "~&~V@T~A @~D ~S~%"
                  (min *depth* 40) name (inp-pos inp)
                  (subseq (inp-str inp) (inp-pos inp)
                          (min (length (inp-str inp)) (+ (inp-pos inp) 30)))))
        (let ((result (ev (rdef-body rd) inp new-env gram)))
          (when (ok-p result) (setf (ok-tag result) name))
          (setf (gethash mkey *memo*) result)
          result)))))

;;; ═══════════════════════════════════════════════════════════════════
;;; 6. PUBLIC API
;;; ═══════════════════════════════════════════════════════════════════

(defun yaml-parse (gram text &key (start-rule 'L-YAML-STREAM) args trace)
  (let ((*memo* (make-hash-table :test 'equal))
        (*trace* trace) (*depth* 0))
    (let ((r (ev-ref start-rule args (mkinp text) nil gram)))
      (if (ok-p r)
          (values t (ok-val r) (inp-pos (ok-rest r)) (ok-ast r))
          (values nil (fa-msg r) (fa-pos r) nil)))))

(defun yaml-parse-file (gram path &key (start-rule 'L-YAML-STREAM) trace)
  (yaml-parse gram
              (with-open-file (f path :external-format :utf-8)
                (let ((s (make-string (file-length f))))
                  (read-sequence s f) s))
              :start-rule start-rule :trace trace))

;;; ── AST to native CL value conversion ───────────────────────────────

(defun clean-scalar (s)
  "Strip leading/trailing whitespace and surrounding quotes from a scalar string."
  (when (stringp s)
    (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
      ;; Strip surrounding double quotes
      (when (and (>= (length s) 2)
                 (char= (char s 0) #\")
                 (char= (char s (1- (length s))) #\"))
        (setf s (subseq s 1 (1- (length s)))))
      ;; Strip surrounding single quotes
      (when (and (>= (length s) 2)
                 (char= (char s 0) #\')
                 (char= (char s (1- (length s))) #\'))
        (setf s (subseq s 1 (1- (length s)))))
      s)))

(defun yaml-value (ast)
  "Convert a YAML AST into native CL data structures.
   STREAM   -> unwrap single doc, or list of docs
   DOC      -> unwrap to content
   MAPPING  -> hash-table with string keys
   SEQUENCE -> list
   PAIR     -> used internally to build mappings
   strings  -> coerced scalars (null/bool/int/float/string)"
  (cond
    ((null ast) nil)
    ((stringp ast) (coerce-scalar (clean-scalar ast)))
    ((not (consp ast)) ast)
    ;; Stream: unwrap
    ((eq (car ast) 'STREAM)
     (let ((children (remove nil (mapcar #'yaml-value (cdr ast)))))
       (if (= 1 (length children)) (car children) children)))
    ;; Document: unwrap
    ((eq (car ast) 'DOC)
     (let ((children (remove nil (mapcar #'yaml-value (cdr ast)))))
       (if (= 1 (length children)) (car children) children)))
    ;; Mapping: build hash table from PAIR children
    ((eq (car ast) 'MAPPING)
     (let ((ht (make-hash-table :test 'equal)))
       (dolist (child (cdr ast))
         (cond
           ((and (consp child) (eq (car child) 'PAIR))
            (let ((k (yaml-value (cadr child)))
                  (v (yaml-value (caddr child))))
              (setf (gethash (if (stringp k) k (format nil "~A" k)) ht) v)))
           ;; Nested mapping — merge into this one
           ((and (consp child) (eq (car child) 'MAPPING))
            (let ((sub (yaml-value child)))
              (when (hash-table-p sub)
                (maphash (lambda (k v) (setf (gethash k ht) v)) sub))))
           ;; Might be a scalar that's actually a nested structure
           (t nil)))
       ht))
    ;; Sequence: collect children
    ((eq (car ast) 'SEQUENCE)
     (mapcar #'yaml-value (cdr ast)))
    ;; Pair: return as cons
    ((eq (car ast) 'PAIR)
     (cons (yaml-value (cadr ast)) (yaml-value (caddr ast))))
    ;; Anchor/Alias
    ((eq (car ast) 'ANCHOR) (yaml-value (caddr ast)))
    ((eq (car ast) 'ALIAS)  (list :alias (yaml-value (cadr ast))))
    ;; Tag
    ((eq (car ast) 'TAG) (yaml-value (caddr ast)))
    ;; Fallback
    (t ast)))

(defun coerce-scalar (s)
  "Auto-detect scalar type from string value."
  (cond
    ((not (stringp s)) s)
    ;; null
    ((member s '("null" "Null" "NULL" "~") :test #'string=) :null)
    ((string= s "") :null)
    ;; boolean
    ((member s '("true" "True" "TRUE") :test #'string=) :true)
    ((member s '("false" "False" "FALSE") :test #'string=) :false)
    ;; integer
    ((and (plusp (length s))
          (every (lambda (c) (or (digit-char-p c) (char= c #\-) (char= c #\+)))
                 s)
          (handler-case (parse-integer s) (error () nil)))
     (parse-integer s))
    ;; float
    ((and (plusp (length s))
          (handler-case (let ((v (read-from-string s)))
                          (and (numberp v) v))
            (error () nil))))
    ;; string
    (t s)))

(defun yaml-load (gram text)
  "Parse YAML text and return native CL value."
  (multiple-value-bind (ok val pos ast)
      (yaml-parse gram text)
    (declare (ignore val pos))
    (if ok (yaml-value ast) (error "YAML parse failed"))))

;;; ═══════════════════════════════════════════════════════════════════
;;; 7. TESTS
;;; ═══════════════════════════════════════════════════════════════════

(defun run-tests (&optional (path "yaml-grammar.scm"))
  (let ((g (load-yaml-grammar path)))
    (format t "~&~%; === Smoke Tests ===~%")
    (flet ((t1 (rule input expect &rest args)
             (let ((*memo* (make-hash-table :test 'equal)) (*depth* 0))
               (let* ((r (ev-ref rule args (mkinp input) nil g))
                      (pass (eq (not (not (ok-p r))) expect)))
                 (format t "; ~:[FAIL~;PASS~] ~A ~S → ~A~%"
                         pass rule input
                         (if (ok-p r) (ok-val r) (fa-msg r)))
                 pass))))
      (let ((all t))
        (flet ((chk (&rest a) (unless (apply #'t1 a) (setf all nil))))
          (chk 'C-SEQUENCE-ENTRY "-" t)
          (chk 'C-SEQUENCE-ENTRY "x" nil)
          (chk 'C-MAPPING-VALUE  ":" t)
          (chk 'C-MAPPING-KEY    "?" t)
          (chk 'C-COMMENT        "#" t)
          (chk 'S-SPACE          " " t)
          (chk 'S-TAB            (string #\Tab) t)
          (chk 'S-WHITE          " " t)
          (chk 'S-WHITE          "x" nil)
          (chk 'NS-DEC-DIGIT     "5" t)
          (chk 'NS-DEC-DIGIT     "a" nil)
          (chk 'NS-CHAR          "x" t)
          (chk 'NB-CHAR          "a" t)
          (chk 'NS-HEX-DIGIT     "F" t)
          (chk 'NS-HEX-DIGIT     "g" nil)
          (chk 'C-ESCAPE         "\\" t)
          (chk 'C-DIRECTIVES-END "---" t)
          (chk 'C-DOCUMENT-END   "..." t)
          (chk 'C-SECONDARY-TAG-HANDLE "!!" t)
          (chk 'NS-ESC-NULL      "0" t)
          (chk 'NS-ESC-LINE-FEED "n" t)
          (chk 'C-DOUBLE-QUOTED  "\"hello\"" t 0 'FLOW-OUT)
          (chk 'C-DOUBLE-QUOTED  "\"\"" t 0 'FLOW-OUT)
          (chk 'C-SINGLE-QUOTED  "'hello'" t 0 'FLOW-OUT)
          (chk 'C-INDICATOR      "-" t)
          (chk 'C-INDICATOR      "[" t)
          (chk 'C-INDICATOR      "a" nil)
          (chk 'C-FLOW-INDICATOR "," t)
          (chk 'C-FLOW-INDICATOR "[" t)
          (chk 'C-FLOW-INDICATOR "-" nil)
          ;; Flow sequences
          (chk 'C-FLOW-SEQUENCE  "[1, 2, 3]" t 0 'FLOW-OUT)
          (chk 'C-FLOW-SEQUENCE  "[]" t 0 'FLOW-OUT)
          ;; Flow mappings
          (chk 'C-FLOW-MAPPING   "{a: b}" t 0 'FLOW-OUT)
          (chk 'C-FLOW-MAPPING   "{}" t 0 'FLOW-OUT))
        (format t "~%; === ~:[SOME FAILURES~;ALL PASSED~] ===~%" all)
        all))))

;;; ═══════════════════════════════════════════════════════════════════
;;; 8. CLI
;;; ═══════════════════════════════════════════════════════════════════

(defun main ()
  (let* ((args (cdr sb-ext:*posix-argv*))
         (gpath (or (find-if (lambda (a) (search ".scm" a)) args)
                    "yaml-grammar.scm"))
         (ypath (find-if (lambda (a) (and (not (search ".scm" a))
                                          (not (string= a "--")))) args)))
    (format t "; Loading grammar: ~A~%" gpath)
    (let ((g (load-yaml-grammar gpath)))
      (cond
        (ypath
         (format t "; Parsing: ~A~%" ypath)
         (multiple-value-bind (ok? val pos) (yaml-parse-file g ypath)
           (if ok? (format t "; ✓ OK ~D chars~%" pos)
               (format t "; ✗ FAIL @~D: ~A~%" pos val))))
        (t
         (format t "~%; Demo:~%")
         (dolist (pair '(("key: value" NS-FLOW-NODE 0 FLOW-OUT)
                         ("- item" C-L-BLOCK-SEQ-ENTRY 0)
                         ("42" NS-FLOW-NODE 0 FLOW-OUT)
                         ("\"hello world\"" C-DOUBLE-QUOTED 0 FLOW-OUT)
                         ("'single'" C-SINGLE-QUOTED 0 FLOW-OUT)
                         ("[1, 2]" C-FLOW-SEQUENCE 0 FLOW-OUT)
                         ("{a: b}" C-FLOW-MAPPING 0 FLOW-OUT)))
           (let ((text (car pair)) (rule (cadr pair)) (rargs (cddr pair)))
             (format t "~%; ~S via ~A:~%" text rule)
             (let ((*memo* (make-hash-table :test 'equal)) (*depth* 0))
               (let ((r (ev-ref rule rargs (mkinp text) nil g)))
                 (format t ";   → ~:[FAIL: ~A~;OK: ~S~]~%"
                         (ok-p r)
                         (if (ok-p r) (ok-val r) (fa-msg r))))))))))))
