;;;; emit-yaml-concerns.lisp — Concern compiler for projected YAML parsers
;;;;
;;;; The algorithm structure lives here. Each PEG spec provides vocab
;;;; blocks via (def-tgt "cv" hash). The compiler assembles them.
;;;;
;;;; Vocab keys (all strings):
;;;;   "value-type-decl"  — YamlValue type + constructors
;;;;   "accessors"        — get/at/str methods (optional, nil for Python)
;;;;   "coerce-fn"        — coerce_scalar function body
;;;;   "converter-decl"   — converter struct with anchors field
;;;;   "convert-fn"       — complete convert(node) function
;;;;   "load-fn"          — public load() function

(in-package #:yaml-eval)

(defun cv (key)
  "Get concern vocab entry."
  (let ((vocab (tgt "cv")))
    (when vocab (gethash key vocab))))

(defun emit-yaml-concerns ()
  "Emit concern layer from vocab blocks."
  (let ((vocab (tgt "cv")))
    (unless vocab (return-from emit-yaml-concerns))
    (let ((cmt (or (tgt "comment-prefix") "//")))
      (emitf "~A ── Native Value Type ──~%" cmt)
      (blank)
      (emit-block (cv "value-type-decl"))
      (blank)
      (when (cv "accessors")
        (emit-block (cv "accessors"))
        (blank))
      (emitf "~A ── Schema Coercion ──~%" cmt)
      (blank)
      (emit-block (cv "coerce-fn"))
      (blank)
      (emitf "~A ── AST → Native Conversion with Anchor Resolution ──~%" cmt)
      (blank)
      (when (cv "converter-decl")
        (emit-block (cv "converter-decl"))
        (blank))
      (emit-block (cv "convert-fn"))
      (blank)
      (emitf "~A ── Public API ──~%" cmt)
      (blank)
      (emit-block (cv "load-fn"))
      (blank))))
