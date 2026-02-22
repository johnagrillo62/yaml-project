;;;; emit-format.lisp — Shared line-breaking logic for all emitters
;;;;
;;;; Loaded by both direct emitters and emit-yaml-peg.lisp.
;;;; Provides ml-join for breaking long seq/alt arrays across lines.

(in-package #:yaml-eval)

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

(defun maybe-ml (one-line-fn wrapped)
  "Try single-line. If too long, recompile at deeper indent and ml-join.
   ONE-LINE-FN takes a list of wrapped strings and returns the joined string.
   WRAPPED is a thunk that produces the list (called twice if multi-line needed)."
  (let ((one-line (funcall one-line-fn (funcall wrapped))))
    (if (<= (length one-line) *max-line-length*)
        one-line
        (let ((*indent-level* (1+ *indent-level*)))
          (funcall one-line-fn (list (ml-join (funcall wrapped))))))))
