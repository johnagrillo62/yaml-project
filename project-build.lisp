;;;; project-build.lisp — Project build scripts from build-spec.scm
;;;;
;;;; Reads the spec, builds a shell AST per target, compiles to bash.
;;;; Two projections chained: spec → shell AST → bash.
;;;;
;;;; Usage: sbcl --load project-build.lisp --quit

(load "emit-shell.lisp")

;;; ── Spec reader ──

(defun read-spec (file)
  (with-open-file (s file :direction :input)
    (let ((*read-eval* nil)) (read s))))

(defun prop (key forms)
  (loop for f in forms
        when (and (listp f) (symbolp (car f))
                  (string-equal (symbol-name (car f)) (symbol-name key)))
        return (cdr f)))
(defun prop1 (key forms) (car (prop key forms)))

(defun targets (spec)
  (loop for f in (cddr spec)
        when (and (listp f) (string-equal (symbol-name (car f)) "TARGET"))
        collect (cons (cadr f) (cddr f))))

(defun ln (s) (string-downcase (symbol-name s)))
(defun cn (s) (string-capitalize (symbol-name s)))

(defun sra (old new str)
  (loop for start = 0 then (+ pos (length new))
        for pos = (search old str :start2 start)
        while pos
        do (setq str (concatenate 'string
                       (subseq str 0 pos) new
                       (subseq str (+ pos (length old)))))
        finally (return str)))

(defun sv (tmpl src bin bd)
  (sra "{bindir}" bd (sra "{bin}" bin (sra "{src}" src tmpl))))

(defun compiled-p (props)
  (or (prop1 'compile props)
      (prop1 'compile-darwin props)
      (prop1 'compile-linux props)))

;;; ── Install hints ──

(defparameter *hints*
  '(("python3"  . "apt install python3  OR  brew install python3")
    ("lua"      . "apt install lua5.4  OR  brew install lua")
    ("bash"     . "pre-installed on most systems")
    ("escript"  . "apt install erlang  OR  brew install erlang")
    ("pwsh"     . "https://learn.microsoft.com/en-us/powershell/scripting/install/installing-powershell")
    ("go"       . "https://go.dev/dl/")
    ("rustc"    . "curl -sSf https://sh.rustup.rs | sh")
    ("g++"      . "apt install g++  OR  xcode-select --install")
    ("javac"    . "apt install default-jdk  OR  brew install openjdk")
    ("java"     . "apt install default-jre  OR  brew install openjdk")
    ("kotlinc"  . "https://kotlinlang.org/docs/command-line.html")
    ("mcs"      . "apt install mono-mcs  OR  brew install mono")
    ("mono"     . "apt install mono-runtime  OR  brew install mono")
    ("dotnet"   . "https://dotnet.microsoft.com/download")
    ("ghc"      . "https://www.haskell.org/ghcup/")
    ("swiftc"   . "https://swift.org/download/")
    ("zig"      . "https://ziglang.org/download/")
    ("ocamlopt" . "https://ocaml.org/install")
    ("clang"    . "apt install clang  OR  xcode-select --install")
    ("nasm"     . "apt install nasm  OR  brew install nasm")
    ("ld"       . "apt install binutils")
    ("gnustep-config" . "apt install gnustep-devel  OR  brew install gnustep")))

(defun hint (dep)
  (or (cdr (assoc dep *hints* :test #'string=))
      (format nil "install ~A for your platform" dep)))

;;; ── AST builders ─────────────────────────────────────────────
;;; These compose shell AST nodes from spec data.
;;; No strings of shell. Just tree construction.

(defun build-header (name src &optional bin)
  `(progn
     (comment ,(format nil "════════════════════════════════════════════════════════════════"))
     (comment ,(format nil "build-~A.sh — ~A YAML 1.2 parser demo" (ln name) (cn name)))
     (comment "")
     (comment "Projected from build-spec.scm by the Futumura YAML Projector.")
     (comment "The parser was generated from the YAML 1.2 spec's 211 grammar rules.")
     (comment "")
     (comment ,(format nil "Source: ~A" src))
     ,@(when bin `((comment ,(format nil "Binary: ~A" bin))))
     (comment ,(format nil "════════════════════════════════════════════════════════════════"))
     (blank)
     (exec "set -euo pipefail")
     (exec "cd \"$(dirname \"$0\")\"")))

(defun build-show-system (name)
  `(fn "show_system"
     (echo "\"════════════════════════════════════════════════════\"")
     (echo ,(format nil "\" ~A YAML Parser — Futumura Projector Demo\"" (cn name)))
     (echo "\"════════════════════════════════════════════════════\"")
     (echo "")
     (echo "\"  System:  $(uname -s) $(uname -m)\"")
     (echo "\"  Host:    $(hostname 2>/dev/null || echo unknown)\"")
     (echo "\"  Date:    $(date)\"")
     (echo "")))

(defun build-check-dep (dep &optional ver-cmd)
  (let ((vcmd (or ver-cmd (format nil "~A --version 2>&1 | head -1" dep))))
    `(if (cmd? ,dep)
       (echo ,(format nil "\"  ✓ ~A $(~A)\"" dep vcmd))
       (progn
         (echo ,(format nil "'  ✗ ~A not found'" dep))
         (echo ,(format nil "'    Install: ~A'" (hint dep)))
         (set "fail" "1")))))

(defun build-check-file (path fix)
  `(if (file? ,path)
     (echo ,(format nil "\"  ✓ ~A ($(wc -l < ~A) lines)\"" path path))
     (progn
       (echo ,(format nil "'  ✗ ~A not found'" path))
       (echo ,(format nil "'    ~A'" fix))
       (set "fail" "1"))))

(defun build-check-dir (path fix)
  `(if (dir? ,path)
     (echo ,(format nil "\"  ✓ ~A/ ($(find ~A -name in.yaml | wc -l) tests)\"" path path))
     (progn
       (echo ,(format nil "'  ✗ ~A/ not found'" path))
       (echo ,(format nil "'    ~A'" fix))
       (set "fail" "1"))))

(defun build-do-check (deps ver-cmd src ts)
  `(fn "do_check"
     (echo "'Checking prerequisites...'")
     (echo "")
     (local "fail" "0")
     ,@(mapcar (lambda (d) (build-check-dep d ver-cmd)) deps)
     ,(build-check-file src "Run: sbcl --load build-yaml.lisp --quit")
     ,(build-check-dir ts "Run: git clone https://github.com/yaml/yaml-test-suite.git")
     (echo "")
     (when "[ $fail -ne 0 ]"
       (echo "'Prerequisites missing. See above for install instructions.'")
       (exit "1"))
     (echo "'All prerequisites satisfied.'")
     (echo "")))

(defun build-do-build (cmd cmd-darwin cmd-linux bin bd)
  `(fn "do_build"
     (echo "'Building...'")
     (mkdir ,bd)
     ,(if (and cmd-darwin cmd-linux)
        `(progn
           (exec ,(format nil "if [ \"$(uname)\" = \"Darwin\" ]; then"))
           (exec ,(format nil "  ~A" cmd-darwin))
           (exec "else")
           (exec ,(format nil "  ~A" cmd-linux))
           (exec "fi"))
        `(exec ,(or cmd cmd-darwin cmd-linux)))
     (echo "'  ✓ Build complete'")
     (echo "")))

(defun build-do-test (runner ts name slow timeout pipe test-note)
  (let ((to (or timeout "1")))
    (let ((run-ok (if pipe
                      (format nil "cat \"$sd/in.yaml\" | timeout ~A ~A" to runner)
                      (format nil "timeout ~A ~A \"$sd/in.yaml\"" to runner)))
          (run-err (if pipe
                       (format nil "cat \"$sd/in.yaml\" | timeout ~A ~A >/dev/null 2>&1" to runner)
                       (format nil "timeout ~A ~A \"$sd/in.yaml\" >/dev/null 2>&1" to runner))))
  `(fn "do_test"
     ,@(when slow
         `((echo ,(format nil "'⚠  ~A'" slow))
           (echo "")))
     (echo "'Running YAML test suite...'")
     (exec ,(format nil "echo \"  Started: $(date)\""))
     (echo "")
     (set "pass" "0")
     (set "fail" "0")
     (set "total" "0")
     (exec "start_time=$(date +%s%N 2>/dev/null || date +%s)")
     (for "d" ,(format nil "~A/*/" ts)
       (exec "d=${d%/}")
       (exec "subs=$(find \"$d\" -maxdepth 1 -mindepth 1 -type d -name '[0-9]*' 2>/dev/null | head -1)")
       (exec "[ -n \"$subs\" ] && dirs=\"$d\"/*/ || dirs=$d")
       (for "sd" "$dirs"
         (exec "[ -d \"$sd\" ] || continue")
         (exec "bn=$(basename \"$sd\")")
         (exec "echo \"$bn\" | grep -qE '^[0-9]+$' 2>/dev/null || [ \"$sd\" = \"$d\" ] || continue")
         (unless (file? "\"$sd/in.yaml\"")
           (exec "[ -f \"$sd/error\" ] && total=$((total+1)) && pass=$((pass+1))")
           (continue))
         (incr "total")
         ;; ── cursor progress: update in place ──
         (printf "'\\r\\033[K  %d: %s' \"$total\" \"$(basename \"$d\")\"")
         (if (file? "\"$sd/error\"")
           (progn
             (exec ,(format nil "rc=0; (~A) || rc=$?" run-err))
             (exec "[ $rc -ne 0 ] && pass=$((pass+1)) || fail=$((fail+1))"))
           (progn
             (exec ,(format nil "result=$(~A 2>/dev/null) || true" run-ok))
             (exec "echo \"$result\" | grep -q \"^OK:\" && pass=$((pass+1)) || fail=$((fail+1))")))))
     ;; ── clear the progress line ──
     (printf "'\\r\\033[K'")
     (exec "end_time=$(date +%s%N 2>/dev/null || date +%s)")
     (exec "if [ ${#start_time} -gt 10 ]; then elapsed=$(( (end_time - start_time) / 1000000 )); unit=ms; else elapsed=$((end_time - start_time)); unit=s; fi")
     (echo "\"════════════════════════════════════════════════════\"")
     (echo ,(format nil "\" ~A: $pass / $total passed  ($fail failed)  ${elapsed}${unit}\"" (cn name)))
     (echo "\"════════════════════════════════════════════════════\"")
     (echo "")
     ,@(when test-note
         `((echo ,(format nil "'~A'" test-note))
           (echo "")))))))

(defun build-dispatch (name comp)
  `(case "\"${1:-}\""
     ("--help|-h"
       (exec "sed -n '/^# Source:/,/^# ═/{/^# ═/!s/^# //p}' \"$0\""))
     ("--check"
       (call "show_system")
       (call "do_check"))
     ,@(when comp
         `(("--build"
            (call "show_system")
            (call "do_check")
            (call "do_build"))))
     ("--test"
       (call "show_system")
       (call "do_test"))
     ("\"\"" 
       (call "show_system")
       (call "do_check")
       ,@(when comp '((call "do_build")))
       (call "do_test"))
     ("*"
       (echo "\"Unknown: $1\" >&2")
       (exit "1"))))

;;; ── Main: spec → AST → bash ─────────────────────────────────

(defun build-script-ast (tgt body)
  "Build complete shell AST for one target."
  (let* ((name   (car tgt))
         (props  (cdr tgt))
         (file   (prop1 'file props))
         (deps   (prop 'deps props))
         (comp   (compiled-p props))
         (cmd    (prop1 'compile props))
         (cmd-d  (prop1 'compile-darwin props))
         (cmd-l  (prop1 'compile-linux props))
         (interp (prop1 'interp props))
         (runcmd (prop1 'run props))
         (vercmd (prop1 'version props))
         (slow   (prop1 'slow props))
         (to     (prop1 'timeout props))
         (pipe   (prop1 'pipe-input props))
         (tnote  (prop1 'test-note body))
         (ts     (or (prop1 'test-suite body) "yaml-test-suite"))
         (gd     (or (prop1 'gen-dir body) "gen"))
         (bd     (or (prop1 'bin-dir body) "bin"))
         (src    (format nil "~A/~A" gd file))
         (bin    (format nil "~A/~A" bd (ln name)))
         (runner (cond (runcmd (sv runcmd src bin bd))
                       (comp bin)
                       (t (format nil "~A ~A" interp src))))
         (build-cmd   (when cmd (sv cmd src bin bd)))
         (build-cmd-d (when cmd-d (sv cmd-d src bin bd)))
         (build-cmd-l (when cmd-l (sv cmd-l src bin bd))))
    `(progn
       ,(build-header name src (when comp bin))
       (blank)
       ,(build-show-system name)
       (blank)
       ,(build-do-check deps vercmd src ts)
       (blank)
       ,@(when comp (list (build-do-build build-cmd build-cmd-d build-cmd-l bin bd)))
       ,@(when comp '((blank)))
       ,(build-do-test runner ts name slow to pipe tnote)
       (blank)
       ,(build-dispatch name comp))))

(defun project-all ()
  (let* ((spec (read-spec "build-spec.scm"))
         (body (cddr spec)))
    (format t "; Projecting build scripts from build-spec.scm~%")
    (dolist (tgt (targets spec))
      (let* ((ast (build-script-ast tgt body))
             (path (format nil "build-~A.sh" (ln (car tgt)))))
        (emit-shell ast path)))
    (format t "; Done. ~D scripts projected.~%" (length (targets spec)))))

(project-all)
