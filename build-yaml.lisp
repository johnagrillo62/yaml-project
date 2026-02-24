;;;; build-yaml.lisp — Project yaml-grammar.scm → all target languages
;;;;
;;;; Usage:
;;;;   cd futumura
;;;;   sbcl --load build-yaml.lisp --quit
;;;;
;;;; Loads the evaluator once, then runs each emitter against the grammar.
;;;; Direct emitters: emit-yaml-{cpp,java,rust}.lisp
;;;; PEG emitters:    peg-{go,python,objc,cpp,rust}.lisp → emit-yaml-peg.lisp

(load "eval/yaml-eval.lisp")

(in-package #:yaml-eval)

;;; Silence redefinition and forward-reference warnings
(declaim (sb-ext:muffle-conditions style-warning warning))

;;; Shared formatting (ml-join, maybe-ml)
(load "emit/emit-format.lisp")

(defvar *grammar-path* "grammar/yaml-grammar.scm")
(defvar *gen-dir*      "gen/")

(ensure-directories-exist (concatenate 'string *gen-dir* "x"))

;;; ═══════════════════════════════════════════════════════════════════
;;; DIRECT EMITTERS
;;; emit-yaml-{lang}.lisp each define project-yaml-to-{lang}
;;; ═══════════════════════════════════════════════════════════════════

(defparameter *direct-emitters*
  '(
    ("java" "emit/emit-yaml-java.lisp" project-yaml-to-java "YamlReader.java")
    )
  )

;;; ═══════════════════════════════════════════════════════════════════
;;; PEG EMITTERS
;;; emit-yaml-peg.lisp defines def-tgt, *tgt*, project-yaml.
;;; Each spec fills *tgt*, then we call project-yaml.
;;; ═══════════════════════════════════════════════════════════════════

(defparameter *peg-emitters*
  '(
    ("bash"     "spec/peg-bash.lisp"     "peg_yaml.sh")
    ("cpp"      "spec/peg-cpp.lisp"      "peg_yaml.cpp")
    ("csharp"   "spec/peg-csharp.lisp"   "PegYaml.cs")
    ("erlang"   "spec/peg-erlang.lisp"   "peg_yaml.erl")
    ("fsharp"   "spec/peg-fsharp.lisp"   "PegYaml.fs")
    ("go"       "spec/peg-go.lisp"       "peg_yaml.go")
    ("haskell"  "spec/peg-haskell.lisp"  "PegYaml.hs")
    ("kotlin"   "spec/peg-kotlin.lisp"   "PegYaml.kt")
    ("lua"      "spec/peg-lua.lisp"      "peg_yaml.lua")
    ("objc"     "spec/peg-objc.lisp"     "PegYaml.m")
    ("ocaml"    "spec/peg-ocaml.lisp"    "peg_yaml.ml")
    ("python"   "spec/peg-python.lisp"   "peg_yaml.py")
    ("powershell"   "spec/peg-powershell.lisp"   "peg_yaml.ps1")
    ("rust"     "spec/peg-rust.lisp"     "peg_yaml.rs")
    ("swift"    "spec/peg-swift.lisp"    "PegYaml.swift")
    ("x86"      "spec/peg-x86.lisp"      "peg_yaml.x86")
    ("zig"      "spec/peg-zig.lisp"      "peg_yaml.zig")
    )
  )

;;; ═══════════════════════════════════════════════════════════════════
;;; BUILD
;;; ═══════════════════════════════════════════════════════════════════

(defun build-all ()
  (format t "~&; ════════════════════════════════════════~%")
  (format t "; Futumura — YAML grammar projector~%")
  (format t "; Grammar: ~A~%" *grammar-path*)
  (format t "; Output:  ~A~%" *gen-dir*)
  (format t "; ════════════════════════════════════════~%")

  ;; Direct emitters
  (dolist (entry *direct-emitters*)
    (destructuring-bind (lang file fn output) entry
      (format t "~&; ── ~A ──~%" lang)
      (handler-case
          (progn
            (load file)
            (funcall (find-symbol (symbol-name fn) :yaml-eval)
                     *grammar-path*
                     (concatenate 'string *gen-dir* output)))
        (error (e)
          (format t ";   ERROR [~A]: ~A~%" lang e)))))

  ;; PEG emitters — load engine once (defines def-tgt, *tgt*, project-yaml)
  (load "emit/emit-yaml-peg.lisp")
  (load "emit/emit-yaml-concerns.lisp")

  (dolist (entry *peg-emitters*)
    (destructuring-bind (lang spec-file output) entry
      (format t "~&; ── ~A (peg) ──~%" lang)
      (handler-case
          (progn
            ;; Reset target spec
            (clrhash *tgt*)
            ;; Load language spec (fills *tgt*)
            (load spec-file)
            ;; Project
            (project-yaml *grammar-path*
                          (concatenate 'string *gen-dir* output)))
        (error (e)
          (format t ";   ERROR [~A]: ~A~%" lang e)))))

  (format t "~&; ════════════════════════════════════════~%")
  (format t "; Done. All outputs in ~A~%" *gen-dir*))

(build-all)
