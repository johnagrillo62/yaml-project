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
;;; WRAP — lambda/closure wrapping, dispatched to target
;;; ═══════════════════════════════════════════════════════════════════

(defun lw (body env)
  "Ref-closure wrap: for star/plus/opt/neg/etc (called by reference)."
  (funcall (tgt "ref-wrap") body env))

(defun bw (body env)
  "Box-closure wrap: for seq/alt (stored in collection)."
  (funcall (tgt "box-wrap") body env))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPRESSION COMPILER — target-independent
;;; ═══════════════════════════════════════════════════════════════════

(defvar *gram* nil)

(defun ce (expr env)
  "Compile grammar expression → target language Result expression."
  (cond
    ((null expr) "ok(inp)")
    ((eq expr 'EMPTY) "ok(inp)")
    ((integerp expr) (format nil "match_cp(inp, ~D)" expr))
    ((symbolp expr)
     (if (member expr env) (peg-ident expr)
         (format nil "~A(inp)" (peg-ident expr))))
    ((listp expr)
     (let ((op (car expr)) (args (cdr expr)))
       (case op
         (EMPTY  "ok(inp)")
         (CHAR   (ce-char (car args) env))
         (HEX    (ce-hex (car args)))
         (RANGE  (ce-range (car args) (cadr args)))
         (STR    (format nil "match_str(inp, ~S)" (string (car args))))
         (SEQ    (ce-seq args env))
         (ALT    (ce-alt args env))
         (STAR   (format nil "star(inp, ~A)" (lw (ce (car args) env) env)))
         (PLUS   (format nil "plus_(inp, ~A)" (lw (ce (car args) env) env)))
         (OPT    (format nil "opt(inp, ~A)" (lw (ce (car args) env) env)))
         (REF    (ce-ref (car args) (cdr args) env))
         (NOT    (format nil "neg(inp, ~A)" (lw (ce (car args) env) env)))
         (MINUS  (format nil "minus(inp, ~A, ~A)"
                         (lw (ce (car args) env) env) (lw (ce (cadr args) env) env)))
         (REPEAT (format nil "rep(inp, ~A, ~A)"
                         (ca (car args) env) (lw (ce (cadr args) env) env)))
         (SWITCH (ce-switch (car args) (cdr args) env))
         (LOOKAHEAD  (format nil "ahead(inp, ~A)" (lw (ce (car args) env) env)))
         (LOOKBEHIND (format nil "behind(inp, ~A)" (lw (ce (car args) env) env)))
         (STARTOFLINE "sol(inp)")
         (ENDOFINPUT  "eof_ok(inp)")
         (BUILD   (format nil "build(inp, ~S, ~A)"
                          (string (car args)) (lw (ce (cadr args) env) env)))
         (SCALAR  (format nil "scalar(inp, ~A)" (lw (ce (car args) env) env)))
         (COLLECT (format nil "collect(inp, ~A)" (lw (ce (car args) env) env)))
         (LET           (ce-let (car args) (cadr args) env))
         (DETECT-INDENT (format nil "detect_indent(inp, ~A)" (ca (car args) env)))
         (PARSE-INT     (format nil "parse_int(inp, ~A)" (lw (ce (car args) env) env)))
         (PARSE-SYM     (format nil "parse_sym(inp, ~A, ~S)"
                                (lw (ce (car args) env) env) (string (cadr args))))
         (VAL           (format nil "val(inp, ~S)" (string (car args))))
         ((+ -)  (ca expr env))
         (IN-FLOW    (format nil "in_flow(~A)" (ca (cadr expr) env)))
         (SEQ-SPACES (format nil "seq_spaces(~A, ~A)"
                             (ca (cadr expr) env) (ca (caddr expr) env)))
         (t (let ((rd (gethash op (gram-rules *gram*))))
              (if rd
                  (format nil "~A(inp~{, ~A~})" (peg-ident op)
                          (mapcar (lambda (a) (ca a env)) args))
                  (format nil "/* UNKNOWN ~S */" expr)))))))
    (t (format nil "/* ?? ~S */" expr))))

;;; ── Sub-compilers (target-independent) ──────────────────────────

(defun ce-char (x env)
  (cond
    ((integerp x) (format nil "match_cp(inp, ~D)" x))
    ((and (symbolp x) (string= (symbol-name x) "SQUOTE")) "match_cp(inp, 39)")
    ((and (symbolp x) (string= (symbol-name x) "DQUOTE")) "match_cp(inp, 34)")
    ((and (symbolp x) (member x env))
     (format nil "match_cp(inp, ~A)" (funcall (tgt "char-cast") (peg-ident x))))
    ((symbolp x) (format nil "match_cp(inp, ~D)" (char-code (char (symbol-name x) 0))))
    (t (format nil "match_cp(inp, ~D)" x))))

(defun ce-hex (v)
  (let ((s (string-upcase (if (integerp v) (format nil "~D" v) (symbol-name v)))))
    (format nil "match_cp(inp, 0x~A)" s)))

(defun ce-range (lo hi)
  (flet ((hv (x)
           (if (and (listp x) (eq (car x) 'HEX))
               (format nil "0x~A" (string-upcase
                                   (if (integerp (cadr x)) (format nil "~D" (cadr x))
                                       (symbol-name (cadr x)))))
               (if (integerp x) (format nil "~D" x)
                   (format nil "~D" (char-code (char (symbol-name x) 0)))))))
    (format nil "match_range(inp, ~A, ~A)" (hv lo) (hv hi))))

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
  "Join closure list with ,\\n+indent for multi-line emission."
  (let* ((pad (indent-str))
         (sep (concatenate 'string "," (string #\Newline) pad)))
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
      (format nil "~A(inp~{, ~A~})" (peg-ident name)
              (mapcar (lambda (a) (ca a env)) call-args))
      (format nil "~A(inp)" (peg-ident name))))

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
    ((integerp expr) (format nil "~D" expr))
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
     (format nil "(~{~A~^ + ~})" (mapcar (lambda (e) (ca e env)) (cdr expr))))
    ((and (listp expr) (eq (car expr) '-))
     (let ((ps (mapcar (lambda (e) (ca e env)) (cdr expr))))
       (if (= 1 (length ps)) (format nil "(-~A)" (car ps))
           (format nil "(~A~{ - ~A~})" (car ps) (cdr ps)))))
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
  (let ((*gram* gram))
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
      ;; Rules
      (let ((cmt (or (tgt "comment-prefix") "//")))
        (emitf "~A ════════════════════════════════════════════════════════════════~%" cmt)
        (emitf "~A YAML 1.2 Grammar — 211 rules~%" cmt)
        (emitf "~A ════════════════════════════════════════════════════════════════~%" cmt)
        (blank)
        (dolist (rd (grammar-rules-ordered gram))
          (unless (member (rdef-name rd) '(IN-FLOW SEQ-SPACES))
            (let* ((nm (peg-ident (rdef-name rd)))
                   (ps (rdef-params rd))
                   (sig (funcall (tgt "fn-sig") nm ps))
                   (body-str (ce (rdef-body rd) ps)))
              (emitf "~A [~D] ~A~%" cmt (rdef-num rd) (rdef-name rd))
              (emit-block (funcall (tgt "fn-body") sig body-str))
              (blank)))))
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
