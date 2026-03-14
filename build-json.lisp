;;;; build-json.lisp — Project json-grammar.scm → all target languages
;;;;
;;;; Usage:
;;;;   sbcl --load build-json.lisp --quit
;;;;
;;;; Same engine as build-yaml.lisp. Different grammar. Different output names.

(load "eval/yaml-eval.lisp")

(in-package #:yaml-eval)

(declaim (sb-ext:muffle-conditions style-warning warning))

(load "emit/emit-format.lisp")

(defvar *grammar-path* "grammar/json-grammar.scm")
(defvar *gen-dir*      "gen/")

(ensure-directories-exist (concatenate 'string *gen-dir* "x"))

(defparameter *peg-emitters*
  '(
    ("bash"        "spec/json-peg-bash.lisp"        "peg_json.sh")
    ("cpp"         "spec/json-peg-cpp.lisp"         "peg_json.cpp")
    ("csharp"      "spec/json-peg-csharp.lisp"      "PegJson.cs")
    ("erlang"      "spec/json-peg-erlang.lisp"      "peg_json.erl")
    ("fsharp"      "spec/json-peg-fsharp.lisp"      "PegJson.fs")
    ("go"          "spec/json-peg-go.lisp"          "peg_json.go")
    ("haskell"     "spec/json-peg-haskell.lisp"     "PegJson.hs")
    ("kotlin"      "spec/json-peg-kotlin.lisp"      "PegJson.kt")
    ("lua"         "spec/json-peg-lua.lisp"         "peg_json.lua")
    ("objc"        "spec/json-peg-objc.lisp"        "PegJson.m")
    ("ocaml"       "spec/json-peg-ocaml.lisp"       "peg_json.ml")
    ("python"      "spec/json-peg-python.lisp"      "peg_json.py")
    ("powershell"  "spec/json-peg-powershell.lisp"  "peg_json.ps1")
    ("rust"        "spec/json-peg-rust.lisp"        "peg_json.rs")
    ("swift"       "spec/json-peg-swift.lisp"       "PegJson.swift")
    ("zig"         "spec/json-peg-zig.lisp"         "peg_json.zig")
    ("x86"         "spec/json-peg-x86.lisp"         "peg_json.x86")
    ))

(defun build-all ()
  (format t "~&; ════════════════════════════════════════~%")
  (format t "; Futumura — JSON grammar projector~%")
  (format t "; Grammar: ~A~%" *grammar-path*)
  (format t "; Output:  ~A~%" *gen-dir*)
  (format t "; ════════════════════════════════════════~%")

  (load "emit/emit-yaml-peg.lisp")

  (dolist (entry *peg-emitters*)
    (destructuring-bind (lang spec-file output) entry
      (format t "~&; ── ~A (peg) ──~%" lang)
      (handler-case
          (progn
            (clrhash *tgt*)
            (load spec-file)
            (project-yaml *grammar-path*
                          (concatenate 'string *gen-dir* output)))
        (error (e)
          (format t ";   ERROR [~A]: ~A~%" lang e)))))

  (format t "~&; ════════════════════════════════════════~%")
  (format t "; Done. All outputs in ~A~%" *gen-dir*))

(build-all)
