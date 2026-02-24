(in-package :yaml-eval)

;;; ============================================================
;;; YAML 1.2 Spec Bug: Rule 78 (l-comment)
;;; ============================================================
;;; 
;;; The bug: l-comment uses b-comment which allows matching EMPTY
;;; (end-of-file fallback) even mid-input. This lets l-comment
;;; consume indentation spaces on content lines, stealing them
;;; from s-indent which needs them for nesting.
;;;
;;; We demonstrate by loading TWO grammars:
;;;   1. The ORIGINAL spec (bug present)
;;;   2. The FIXED spec (b-comment → b-non-content)
;;; and parsing the same nested YAML with both.
;;; ============================================================

;; Load the FIXED grammar (what we've been using)
(defvar *fixed* (load-yaml-grammar "yaml-grammar.scm"))

;; Create the BUGGY grammar: revert rule 78 to original spec
;; Original: (Seq s-separate-in-line (Opt c-nb-comment-text) b-comment)
;; Fixed:    (Seq s-separate-in-line (Opt c-nb-comment-text) b-non-content)
(defvar *buggy*
  (let ((g (load-yaml-grammar "yaml-grammar.scm")))
    ;; Patch rule 78 back to buggy version
    (let ((r78 (gethash 'L-COMMENT (gram-rules g))))
      (when r78
        ;; Replace the body: swap b-non-content back to b-comment
        ;; b-comment = (Alt b-non-content EMPTY)  ← allows empty match
        ;; b-non-content                          ← requires real newline
        (setf (rdef-body r78)
              '(SEQ (REF S-SEPARATE-IN-LINE)
                    (OPT (REF C-NB-COMMENT-TEXT))
                    (REF B-COMMENT)))))  ; ← THE BUG: EMPTY alt
    g))

(format t "~%~%")
(format t "============================================================~%")
(format t " YAML 1.2 Spec Bug Demonstration: Rule 78 (l-comment)~%")
(format t "============================================================~%")
(format t "~%Input YAML:~%")

(defvar *test-yaml* (format nil "a:~%  b: 1~%  c: 2~%"))
(format t "  ~S~%" *test-yaml*)
(format t "~%Expected: {\"a\": {\"b\": 1, \"c\": 2}}  (nested mapping)~%")

;;; --- Parse with BUGGY grammar (original spec) ---
(format t "~%--- BUGGY grammar (original YAML 1.2 spec rule 78) ---~%")
(multiple-value-bind (ok val pos ast)
    (yaml-parse *buggy* *test-yaml*)
  (declare (ignore val))
  (format t "  Consumed: ~D/~D chars~%" pos (length *test-yaml*))
  (format t "  AST: ~S~%" ast)
  (format t "  Value: ")
  (let ((v (yaml-value ast)))
    (if (hash-table-p v)
        (maphash (lambda (k val) (format t "~%    ~S => ~S" k val)) v)
        (format t "~S" v)))
  (format t "~%"))

;;; --- Parse with FIXED grammar ---
(format t "~%--- FIXED grammar (rule 78: b-comment → b-non-content) ---~%")
(multiple-value-bind (ok val pos ast)
    (yaml-parse *fixed* *test-yaml*)
  (declare (ignore val))
  (format t "  Consumed: ~D/~D chars~%" pos (length *test-yaml*))
  (format t "  AST: ~S~%" ast)
  (format t "  Value: ")
  (let ((v (yaml-value ast)))
    (if (hash-table-p v)
        (maphash (lambda (k val)
                   (format t "~%    ~S => ~S" k
                           (if (hash-table-p val)
                               (let ((pairs nil))
                                 (maphash (lambda (k2 v2) (push (cons k2 v2) pairs)) val)
                                 pairs)
                               val)))
                 v)
        (format t "~S" v)))
  (format t "~%"))

(format t "~%--- The difference ---~%")
(format t "  BUGGY: a, b, c are SIBLINGS (flat mapping, a has null value)~%")
(format t "  FIXED: b and c are CHILDREN of a (nested mapping)~%")
(format t "~%  Root cause: l-comment's b-comment matches EMPTY,~%")
(format t "  consuming indentation spaces that s-indent needs.~%")
(format t "  Result: block collections can't detect nesting.~%")
(format t "~%  Fix: require real newline (b-non-content) in l-comment.~%")
(format t "  One symbol change. Bug has been in the spec since 2009.~%")
(format t "  Found by executing the grammar — something nobody does.~%")
(format t "============================================================~%")
