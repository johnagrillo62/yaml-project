;;;; emit-yaml-peg.lisp — Universal PEG grammar projector
;;;;
;;;; One emitter. Two inputs:
;;;;   1. yaml-grammar.scm — the 211 YAML rules
;;;;   2. A target spec     — peg-cpp.lisp or peg-rust.lisp
;;;;
;;;; The target spec provides:
;;;;   - ident: keyword list, prefix, escape
;;;;   - wrap: lambda syntax for ref closures and boxed closures
;;;;   - comb: combinator call templates
;;;;   - runtime: all function bodies as strings
;;;;   - frame: preamble, namespace, api, main
;;;;
;;;; Usage:
;;;;   sbcl --load yaml-eval.lisp --load peg-cpp.lisp --load emit-yaml-peg.lisp
;;;;   sbcl --load yaml-eval.lisp --load peg-rust.lisp --load emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ═══════════════════════════════════════════════════════════════════
;;; TARGET SPEC — filled by peg-cpp.lisp or peg-rust.lisp
;;; ═══════════════════════════════════════════════════════════════════

(defvar *tgt* (make-hash-table :test 'equal)
  "Target spec: string keys → values (strings, functions, lists)")

(defun tgt (key) (gethash key *tgt*))

(defun def-tgt (key val)
  (setf (gethash key *tgt*) val))

;;; ═══════════════════════════════════════════════════════════════════
;;; CALL FORMATTING — C-style f(inp, a, b) vs Haskell f a b inp
;;; ═══════════════════════════════════════════════════════════════════

(defun inp-name () (or (tgt "inp-name") "inp"))

(defun tcall (fn &rest args)
  "Format a combinator call."
  (let ((style (tgt "call-style"))
        (iv (inp-name)))
    (cond
      ((and style (string= style "haskell"))
       (if args
           (format nil "~A ~{~A ~}~A" fn args iv)
           (format nil "~A ~A" fn iv)))
      ((and style (or (string= style "bash") (string= style "wrapper")))
       (if args
           (format nil "~A ~{~A~^ ~}" fn args)
           fn))
      (t
       (if args
           (format nil "~A(~A, ~{~A~^, ~})" fn iv args)
           (format nil "~A(~A)" fn iv))))))

(defun trcall (fn &rest args)
  "Format a rule call."
  (let ((style (tgt "call-style"))
        (iv (inp-name)))
    (cond
      ((and style (string= style "haskell"))
       (if args
           (format nil "~A ~A ~{~A~^ ~}" fn iv args)
           (format nil "~A ~A" fn iv)))
      ((and style (or (string= style "bash") (string= style "wrapper")))
       (if args
           (format nil "~A ~{~A~^ ~}" fn args)
           fn))
      (t
       (if args
           (format nil "~A(~A, ~{~A~^, ~})" fn iv args)
           (format nil "~A(~A)" fn iv))))))

(defun tcall0 (fn &rest args)
  "Format a non-PFn call (no inp parameter)."
  (let ((style (tgt "call-style")))
    (cond
      ((and style (string= style "haskell"))
       (if args
           (format nil "(~A ~{~A~^ ~})" fn args)
           fn))
      ((and style (string= style "bash"))
       (if args
           (format nil "$(~A ~{~A~^ ~})" fn args)
           (format nil "$~A" fn)))
      (t
       (format nil "~A(~{~A~^, ~})" fn args)))))

;;; ═══════════════════════════════════════════════════════════════════
;;; IDENTIFIER — dispatches to target
;;; ═══════════════════════════════════════════════════════════════════

(defun peg-ident (sym)
  "Convert rule name symbol to target identifier."
  (let* ((s (string-downcase (symbol-name sym)))
         (s (substitute #\_ #\- s))
         (s (remove #\+ s))
         (keywords (tgt "keywords"))
         (prefix (or (tgt "keyword-prefix") "r_")))
    (if (or (digit-char-p (char s 0))
            (member s keywords :test #'string=))
        (format nil "~A~A" prefix s)
        s)))

;;; ═══════════════════════════════════════════════════════════════════
;;; OUTPUT
;;; ═══════════════════════════════════════════════════════════════════

(defvar *out* nil)
(defun emit (&rest args) (dolist (a args) (princ a *out*)) (terpri *out*))
(defun emitf (fmt &rest args) (apply #'format *out* fmt args))
(defun blank () (terpri *out*))
(defun emit-block (text)
  "Emit a multi-line string block."
  (when text (princ text *out*) (terpri *out*)))

;;; ═══════════════════════════════════════════════════════════════════
;;; BASH WRAPPER FUNCTIONS — bash can't pass expressions as args,
;;; so we generate named wrapper functions and pass the name.
;;; ═══════════════════════════════════════════════════════════════════

(defvar *bash-wrappers* nil
  "Accumulated wrapper function definitions for bash target.")
(defvar *bash-wrapper-counter* 0)

(defun bash-style-p ()
  (let ((s (tgt "call-style")))
    (and s (string= s "bash"))))

(defun make-bash-wrapper (body)
  "Generate a named wrapper function for BODY, return the name."
  (let ((name (format nil "_w~D" (incf *bash-wrapper-counter*))))
    (push (format nil "~A() { ~A; }" name body) *bash-wrappers*)
    name))

;;; ═══════════════════════════════════════════════════════════════════
;;; WRAP — lambda/closure wrapping, dispatched to target
;;; ═══════════════════════════════════════════════════════════════════

(defun lw (body env)
  "Ref-closure wrap: for star/plus/opt/neg/etc (called by reference)."
  (if (bash-style-p)
      (make-bash-wrapper body)
      (funcall (tgt "ref-wrap") body env)))

(defun bw (body env)
  "Box-closure wrap: for seq/alt (stored in collection)."
  (if (bash-style-p)
      (make-bash-wrapper body)
      (funcall (tgt "box-wrap") body env)))

;;; ═══════════════════════════════════════════════════════════════════
;;; COMBINATOR NAME RESOLUTION — allows target override via comb-* keys
;;; ═══════════════════════════════════════════════════════════════════

(defvar *comb-defaults* (make-hash-table :test 'equal)
  "Default combinator names (C-style) when no comb-* override exists.")

(progn
  (setf (gethash "match-cp"    *comb-defaults*) "match_cp")
  (setf (gethash "match-range" *comb-defaults*) "match_range")
  (setf (gethash "match-str"   *comb-defaults*) "match_str")
  (setf (gethash "star"        *comb-defaults*) "star")
  (setf (gethash "plus"        *comb-defaults*) "plus_")
  (setf (gethash "opt"         *comb-defaults*) "opt")
  (setf (gethash "neg"         *comb-defaults*) "neg")
  (setf (gethash "rep"         *comb-defaults*) "rep")
  (setf (gethash "ahead"       *comb-defaults*) "ahead")
  (setf (gethash "behind"      *comb-defaults*) "behind")
  (setf (gethash "minus"       *comb-defaults*) "minus")
  (setf (gethash "build"       *comb-defaults*) "build")
  (setf (gethash "scalar"      *comb-defaults*) "scalar")
  (setf (gethash "collect"     *comb-defaults*) "collect")
  (setf (gethash "sol"         *comb-defaults*) "sol")
  (setf (gethash "eof"         *comb-defaults*) "eof_ok")
  (setf (gethash "ok"          *comb-defaults*) "ok")
  (setf (gethash "detect"      *comb-defaults*) "detect_indent")
  (setf (gethash "parse-int"   *comb-defaults*) "parse_int")
  (setf (gethash "parse-sym"   *comb-defaults*) "parse_sym")
  (setf (gethash "val"         *comb-defaults*) "val"))

(defun comb (key)
  "Resolve a combinator name. Check for comb-KEY override in target, else use default."
  (or (tgt (concatenate 'string "comb-" key))
      (gethash key *comb-defaults*)
      key))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPRESSION COMPILER — target-independent
;;; ═══════════════════════════════════════════════════════════════════

(defvar *gram* nil)

(defun ce (expr env)
  "Compile grammar expression → target language Result expression."
  (cond
    ((null expr) (tcall (comb "ok")))
    ((eq expr 'EMPTY) (tcall (comb "ok")))
    ((integerp expr) (tcall (comb "match-cp") (format nil "~D" expr)))
    ((symbolp expr)
     (if (member expr env) (peg-ident expr)
         (tcall (peg-ident expr))))
    ((listp expr)
     (let ((op (car expr)) (args (cdr expr)))
       (case op
         (EMPTY  (tcall (comb "ok")))
         (CHAR   (ce-char (car args) env))
         (HEX    (ce-hex (car args)))
         (RANGE  (ce-range (car args) (cadr args)))
         (STR    (let ((s (format nil "~S" (string (car args)))))
                   (tcall (comb "match-str") (if (tgt "str-wrap") (funcall (tgt "str-wrap") s) s))))
         (SEQ    (ce-seq args env))
         (ALT    (ce-alt args env))
         (STAR   (tcall (comb "star") (lw (ce (car args) env) env)))
         (PLUS   (tcall (comb "plus") (lw (ce (car args) env) env)))
         (OPT    (tcall (comb "opt") (lw (ce (car args) env) env)))
         (REF    (ce-ref (car args) (cdr args) env))
         (NOT    (tcall (comb "neg") (lw (ce (car args) env) env)))
         (MINUS  (tcall (comb "minus") (lw (ce (car args) env) env) (lw (ce (cadr args) env) env)))
         (REPEAT (tcall (comb "rep") (ca (car args) env) (lw (ce (cadr args) env) env)))
         (SWITCH (ce-switch (car args) (cdr args) env))
         (LOOKAHEAD  (tcall (comb "ahead") (lw (ce (car args) env) env)))
         (LOOKBEHIND (tcall (comb "behind") (lw (ce (car args) env) env)))
         (STARTOFLINE (tcall (comb "sol")))
         (ENDOFINPUT  (tcall (comb "eof")))
         (BUILD   (let ((s (format nil "~S" (string (car args)))))
                   (tcall (comb "build") (if (tgt "str-wrap") (funcall (tgt "str-wrap") s) s) (lw (ce (cadr args) env) env))))
         (SCALAR  (tcall (comb "scalar") (lw (ce (car args) env) env)))
         (COLLECT (tcall (comb "collect") (lw (ce (car args) env) env)))
         (LET           (ce-let (car args) (cadr args) env))
         (DETECT-INDENT (tcall (comb "detect") (ca (car args) env)))
         (PARSE-INT     (tcall (comb "parse-int") (lw (ce (car args) env) env)))
         (PARSE-SYM     (let ((s (format nil "~S" (string (cadr args)))))
                          (tcall (comb "parse-sym") (lw (ce (car args) env) env) (if (tgt "str-wrap") (funcall (tgt "str-wrap") s) s))))
         (VAL           (let ((s (format nil "~S" (string (car args)))))
                          (tcall (comb "val") (if (tgt "str-wrap") (funcall (tgt "str-wrap") s) s))))
         ((+ -)  (ca expr env))
         (IN-FLOW    (tcall0 "in_flow" (ca (cadr expr) env)))
         (SEQ-SPACES (tcall0 "seq_spaces" (ca (cadr expr) env) (ca (caddr expr) env)))
         (t (let ((rd (gethash op (gram-rules *gram*))))
              (if rd
                  (if (cdr args)
                      (apply #'trcall (peg-ident op) (mapcar (lambda (a) (ca a env)) args))
                      (if args
                          (trcall (peg-ident op) (ca (car args) env))
                          (trcall (peg-ident op))))
                  (format nil "/* UNKNOWN ~S */" expr)))))))
    (t (format nil "/* ?? ~S */" expr))))

;;; ── Sub-compilers (target-independent) ──────────────────────────

(defun ce-char (x env)
  (cond
    ((integerp x) (tcall (comb "match-cp") (format nil "~D" x)))
    ((and (symbolp x) (string= (symbol-name x) "SQUOTE")) (tcall (comb "match-cp") "39"))
    ((and (symbolp x) (string= (symbol-name x) "DQUOTE")) (tcall (comb "match-cp") "34"))
    ((and (symbolp x) (member x env))
     (tcall (comb "match-cp") (funcall (tgt "char-cast") (peg-ident x))))
    ((symbolp x) (tcall (comb "match-cp") (format nil "~D" (char-code (char (symbol-name x) 0)))))
    (t (tcall (comb "match-cp") (format nil "~D" x)))))

(defun hex-fmt (hex-str)
  "Format a hex value using target's hex-prefix (default 0x)."
  (let ((pfx (or (tgt "hex-prefix") "0x")))
    (format nil "~A~A" pfx hex-str)))

(defun ce-hex (v)
  (let ((s (string-upcase (if (integerp v) (format nil "~D" v) (symbol-name v)))))
    (tcall (comb "match-cp") (hex-fmt s))))

(defun ce-range (lo hi)
  (flet ((hv (x)
           (if (and (listp x) (eq (car x) 'HEX))
               (hex-fmt (string-upcase
                         (if (integerp (cadr x)) (format nil "~D" (cadr x))
                             (symbol-name (cadr x)))))
               (if (integerp x) (format nil "~D" x)
                   (format nil "~D" (char-code (char (symbol-name x) 0)))))))
    (tcall (comb "match-range") (hv lo) (hv hi))))

;;; ═══════════════════════════════════════════════════════════════════
;;; LINE FORMATTING — structure-aware, driven from ce-seq / ce-alt
;;; ═══════════════════════════════════════════════════════════════════

(defparameter *max-line-length* 100)
(defvar *indent-level* 1
  "Current indentation depth for multi-line emission (1 = inside function body).")

(defun indent-str ()
  "Return indentation string for current depth."
  (make-string (* 4 *indent-level*) :initial-element #\Space))

(defun ml-join (wrapped)
  "Join closure list with separator+newline+indent for multi-line emission.
   For bash: space-separated. For others: comma-separated."
  (let* ((pad (indent-str))
         (comma (cond ((and (tgt "call-style") (string= (tgt "call-style") "bash")) "")
                      ((tgt "list-sep") (tgt "list-sep"))
                      (t ",")))
         (sep (concatenate 'string comma (string #\Newline) pad)))
    (concatenate 'string
                 (string #\Newline) pad
                 (format nil (concatenate 'string "~{~A~^" sep "~}") wrapped))))

(defun ce-seq (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      ;; First try single-line at current indent
      (let ((wrapped (mapcar (lambda (e) (bw (ce e env) env)) exprs)))
        (let ((one-line (funcall (tgt "seq-emit") wrapped)))
          (if (<= (length one-line) *max-line-length*)
              one-line
              ;; Too long — recompile with bumped indent so nested exprs indent deeper
              (let* ((*indent-level* (1+ *indent-level*))
                     (wrapped2 (mapcar (lambda (e) (bw (ce e env) env)) exprs)))
                (funcall (tgt "seq-emit") (list (ml-join wrapped2)))))))))

(defun ce-alt (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      (let ((wrapped (mapcar (lambda (e) (bw (ce e env) env)) exprs)))
        (let ((one-line (funcall (tgt "alt-emit") wrapped)))
          (if (<= (length one-line) *max-line-length*)
              one-line
              (let* ((*indent-level* (1+ *indent-level*))
                     (wrapped2 (mapcar (lambda (e) (bw (ce e env) env)) exprs)))
                (funcall (tgt "alt-emit") (list (ml-join wrapped2)))))))))

(defun ce-ref (name call-args env)
  (if call-args
      (apply #'trcall (peg-ident name)
             (mapcar (lambda (a) (ca a env)) call-args))
      (trcall (peg-ident name))))

(defun ce-switch (param cases env)
  (funcall (tgt "switch-emit") (peg-ident param)
           (mapcar (lambda (c)
                     (list (string-upcase (symbol-name (car c)))
                           (ce (cadr c) env)))
                   cases)))

(defun ce-let (bindings body env)
  (if (null bindings) (ce body env)
      (let* ((b (car bindings))
             (var (car b))
             (val-expr (cadr b))
             (vn (peg-ident var))
             (numeric-p (member (symbol-name var) '("N" "M") :test #'string-equal)))
        (funcall (if numeric-p (tgt "let-int") (tgt "let-ctx"))
                 vn (ce val-expr env)
                 (ce-let (cdr bindings) body (cons var env))))))

;;; ── Argument compiler (target-aware for context strings) ────────

(defun ca (expr env)
  "Compile arg → target int or context expression."
  (cond
    ((integerp expr)
     (let ((style (tgt "call-style")))
       (if (and style (string= style "haskell") (< expr 0))
           (format nil "(~D)" expr)
           (format nil "~D" expr))))
    ((and (symbolp expr) (eq expr 'EMPTY)) "0")
    ((and (symbolp expr) (member expr env))
     (funcall (tgt "param-ref") expr env))
    ((and (symbolp expr)
          (member (symbol-name expr)
                  '("BLOCK-IN" "BLOCK-OUT" "FLOW-IN" "FLOW-OUT"
                    "BLOCK-KEY" "FLOW-KEY" "STRIP" "CLIP" "KEEP" "EMPTY")
                  :test #'string-equal))
     (funcall (tgt "ctx-literal") (string-upcase (symbol-name expr))))
    ((symbolp expr) (peg-ident expr))
    ((and (listp expr) (eq (car expr) '+))
     (let ((parts (mapcar (lambda (e) (ca e env)) (cdr expr))))
       (if (bash-style-p)
           (format nil "$(( ~{~A~^ + ~} ))" parts)
           (format nil "(~{~A~^ + ~})" parts))))
    ((and (listp expr) (eq (car expr) '-))
     (let ((ps (mapcar (lambda (e) (ca e env)) (cdr expr))))
       (if (bash-style-p)
           (if (= 1 (length ps))
               (format nil "$(( -~A ))" (car ps))
               (format nil "$(( ~A~{ - ~A~} ))" (car ps) (cdr ps)))
           (if (= 1 (length ps)) (format nil "(-~A)" (car ps))
               (format nil "(~A~{ - ~A~})" (car ps) (cdr ps))))))
    ((and (listp expr) (eq (car expr) 'IN-FLOW))
     (funcall (tgt "in-flow-call") (ca (cadr expr) env)))
    ((and (listp expr) (eq (car expr) 'SEQ-SPACES))
     (funcall (tgt "seq-spaces-call") (ca (cadr expr) env) (ca (caddr expr) env)))
    (t (format nil "/* arg?~S */" expr))))

;;; ═══════════════════════════════════════════════════════════════════
;;; EMITTER — reads target spec, writes output
;;; ═══════════════════════════════════════════════════════════════════

(defun grammar-rules-ordered (gram)
  (let ((rules nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v rules)) (gram-rules gram))
    (sort rules #'< :key #'rdef-num)))

(defun emit-peg (gram path)
  (let ((*gram* gram)
        (*bash-wrappers* nil)
        (*bash-wrapper-counter* 0))
    (with-open-file (*out* path :direction :output :if-exists :supersede :external-format :utf-8)
      ;; Header
      (emit-block (tgt "header"))
      (blank)
      ;; Runtime (all combinator impls from target spec)
      (dolist (section (tgt "runtime-sections"))
        (emit-block section)
        (blank))
      ;; Forward declarations (if target needs them)
      (let ((fwd-fn (tgt "fwd-decl")))
        (when fwd-fn
          (dolist (rd (grammar-rules-ordered gram))
            (unless (member (rdef-name rd) '(IN-FLOW SEQ-SPACES))
              (emit-block (funcall fwd-fn (peg-ident (rdef-name rd)) (rdef-params rd)))))
          (blank)))
      ;; Compile all rules first (accumulates bash wrappers)
      (let ((cmt (or (tgt "comment-prefix") "//"))
            (cmte (or (tgt "comment-suffix") ""))
            (compiled-rules nil))
        (dolist (rd (grammar-rules-ordered gram))
          (unless (member (rdef-name rd) '(IN-FLOW SEQ-SPACES))
            (let* ((nm (peg-ident (rdef-name rd)))
                   (ps (rdef-params rd))
                   (sig (funcall (tgt "fn-sig") nm ps))
                   (body-str (ce (rdef-body rd) ps)))
              (push (list (rdef-num rd) (rdef-name rd) sig body-str) compiled-rules))))
        (setq compiled-rules (nreverse compiled-rules))
        ;; Emit bash wrappers (if any)
        (when (and (bash-style-p) *bash-wrappers*)
          (emitf "~A ════════════════════════════════════════════════════════════════ ~A~%" cmt cmte)
          (emitf "~A Wrapper functions (bash has no closures) ~A~%" cmt cmte)
          (emitf "~A ════════════════════════════════════════════════════════════════ ~A~%" cmt cmte)
          (blank)
          (dolist (w (nreverse *bash-wrappers*))
            (emit w))
          (blank))
        ;; Emit rules
        (emitf "~A ════════════════════════════════════════════════════════════════ ~A~%" cmt cmte)
        (emitf "~A YAML 1.2 Grammar — 211 rules ~A~%" cmt cmte)
        (emitf "~A ════════════════════════════════════════════════════════════════ ~A~%" cmt cmte)
        (blank)
        (dolist (cr compiled-rules)
          (destructuring-bind (num name sig body-str) cr
            (emitf "~A [~D] ~A ~A~%" cmt num name cmte)
            (emit-block (funcall (tgt "fn-body") sig body-str))
            (blank))))
      ;; API then concerns then namespace-close then main
      (emit-block (tgt "api"))
      (blank)
      ;; Concern layer — generated from vocab
      (when (tgt "cv")
        (emit-yaml-concerns))
      ;; Legacy string block concerns (transitional)
      (when (and (tgt "yaml-concerns") (not (tgt "cv")))
        (emit-block (tgt "yaml-concerns"))
        (blank))
      (when (tgt "namespace-close")
        (emit-block (tgt "namespace-close"))
        (blank))
      (emit-block (tgt "main-fn")))))

;;; ═══════════════════════════════════════════════════════════════════
;;; ENTRY POINT
;;; ═══════════════════════════════════════════════════════════════════

(defun project-yaml (&optional (grammar-path "yaml-grammar.scm")
                                (output-path nil))
  (let ((out (or output-path (tgt "default-output"))))
    (format t "~&; Loading grammar: ~A~%" grammar-path)
    (let ((gram (load-yaml-grammar grammar-path)))
      (format t "; Target: ~A → ~A~%" (tgt "target-name") out)
      (format t "; Projecting ~D rules~%" (hash-table-count (gram-rules gram)))
      (emit-peg gram out)
      (format t "; Done. ~A written.~%" out))))
