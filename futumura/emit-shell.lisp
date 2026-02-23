;;;; emit-shell.lisp — Compile shell s-expressions to bash
;;;;
;;;; Walk the AST, emit text. Same as the PEG compiler.

(defvar *indent* 0)
(defun ind () (make-string (* *indent* 2) :initial-element #\Space))

;;; ── The compiler: one function, recursive dispatch ───────────

(defun ce (node s)
  "Compile expression NODE to stream S."
  (cond
    ((null node))
    ((stringp node) (format s "~A~A~%" (ind) node))
    ((atom node)    (format s "~A~A~%" (ind) node))
    (t (let ((op (car node)) (args (cdr node)))
         (case op
           ;; ── structure ──
           (progn   (dolist (a args) (ce a s)))
           (fn      (let ((name (car args)) (body (cdr args)))
                      (format s "~A~A() {~%" (ind) name)
                      (let ((*indent* (1+ *indent*)))
                        (dolist (b body) (ce b s)))
                      (format s "~A}~%" (ind))))
           (comment (format s "~A# ~A~%" (ind) (car args)))
           (blank   (terpri s))

           ;; ── assignment ──
           (set     (format s "~A~A=~A~%" (ind) (car args) (cadr args)))
           (local   (format s "~Alocal ~A=~A~%" (ind) (car args) (cadr args)))
           (incr    (format s "~A~A=$(($~A+1))~%" (ind) (car args) (car args)))

           ;; ── output ──
           (echo    (format s "~Aecho ~A~%" (ind) (car args)))
           (printf  (format s "~Aprintf ~A~%" (ind) (car args)))

           ;; ── control flow ──
           (if      (let ((test (car args)) (then (cadr args)) (els (caddr args)))
                      (format s "~Aif ~A; then~%" (ind) (ce-inline test))
                      (let ((*indent* (1+ *indent*))) (ce then s))
                      (when els
                        (format s "~Aelse~%" (ind))
                        (let ((*indent* (1+ *indent*))) (ce els s)))
                      (format s "~Afi~%" (ind))))
           (when    (format s "~Aif ~A; then~%" (ind) (ce-inline (car args)))
                    (let ((*indent* (1+ *indent*)))
                      (dolist (b (cdr args)) (ce b s)))
                    (format s "~Afi~%" (ind)))
           (unless  (format s "~Aif ! ~A; then~%" (ind) (ce-inline (car args)))
                    (let ((*indent* (1+ *indent*)))
                      (dolist (b (cdr args)) (ce b s)))
                    (format s "~Afi~%" (ind)))
           (for     (let ((var (car args)) (expr (cadr args)) (body (cddr args)))
                      (format s "~Afor ~A in ~A; do~%" (ind) var expr)
                      (let ((*indent* (1+ *indent*)))
                        (dolist (b body) (ce b s)))
                      (format s "~Adone~%" (ind))))
           (case    (let ((expr (car args)) (clauses (cdr args)))
                      (format s "~Acase ~A in~%" (ind) expr)
                      (let ((*indent* (1+ *indent*)))
                        (dolist (c clauses)
                          (format s "~A~A)~%" (ind) (car c))
                          (let ((*indent* (1+ *indent*)))
                            (dolist (b (cdr c)) (ce b s)))
                          (format s "~A;;~%" (ind))))
                      (format s "~Aesac~%" (ind))))
           (continue (format s "~Acontinue~%" (ind)))

           ;; ── commands ──
           (exec    (format s "~A~A~%" (ind) (car args)))
           (mkdir   (format s "~Amkdir -p ~A~%" (ind) (car args)))
           (exit    (format s "~Aexit ~A~%" (ind) (car args)))
           (subsh   (format s "~A(~A)~%" (ind) (car args)))
           (call    (format s "~A~A~%" (ind) (car args)))

           ;; ── fallback ──
           (t       (format s "~A~A~%" (ind) node)))))))

(defun ce-inline (node)
  "Compile NODE to a string (for inline use in conditions)."
  (cond
    ((stringp node) node)
    ((atom node) (format nil "~A" node))
    (t (let ((op (car node)) (args (cdr node)))
         (case op
           (cmd?  (format nil "command -v ~A >/dev/null 2>&1" (car args)))
           (file? (format nil "[ -f ~A ]" (car args)))
           (dir?  (format nil "[ -d ~A ]" (car args)))
           (exec? (format nil "[ -x ~A ]" (car args)))
           (not   (format nil "! ~A" (ce-inline (car args))))
           (and   (format nil "~{~A~^ && ~}" (mapcar #'ce-inline args)))
           (or    (format nil "~{~A~^ || ~}" (mapcar #'ce-inline args)))
           (pipe  (format nil "~{~A~^ | ~}" (mapcar #'ce-inline args)))
           (capture (format nil "$(~A)" (ce-inline (car args))))
           (t     (format nil "~{~A~^ ~}" (cons op args))))))))

;;; ── Emit a complete script ───────────────────────────────────

(defun emit-shell (ast path)
  "Compile AST to a bash script at PATH."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (format s "#!/usr/bin/env bash~%")
    (let ((*indent* 0))
      (ce ast s)))
  (format t ";   ~A~%" path))
