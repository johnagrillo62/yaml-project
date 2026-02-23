;;;; gen-makefile.lisp — Generate Makefile from build-spec.scm
;;;;
;;;; Philosophy: No templates. Everything is composed from small
;;;; Makefile fragments derived from the spec declarations.
;;;;
;;;; Usage: sbcl --load gen-makefile.lisp --quit

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

;;; ── String helpers ──

(defun str-replace-all (old new str)
  (loop for start = 0 then (+ pos (length new))
        for pos = (search old str :start2 start)
        while pos
        do (setq str (concatenate 'string
                       (subseq str 0 pos) new
                       (subseq str (+ pos (length old)))))
        finally (return str)))

(defun subst-vars (s &key src bin bindir)
  (when src    (setq s (str-replace-all "{src}" src s)))
  (when bin    (setq s (str-replace-all "{bin}" bin s)))
  (when bindir (setq s (str-replace-all "{bindir}" bindir s)))
  s)

(defun ln (sym) (string-downcase (symbol-name sym)))
(defun un (sym) (string-upcase (symbol-name sym)))
(defun cn (sym) (string-capitalize (symbol-name sym)))

;;; ── Makefile composition ──
;;;
;;; A Makefile is a list of fragments. Each fragment is a string.
;;; We compose fragments from spec entries, then join them.

(defun comment-block (text)
  (format nil "# ~A" text))

(defun var-def (name value)
  (format nil "~A~20T:= ~A" name value))

(defun rule (target deps &rest commands)
  "Compose a Make rule from target, deps, and command lines."
  (with-output-to-string (s)
    (format s "~A:~@[ ~A~]~%" target deps)
    (dolist (c commands)
      (format s "	~A~%" c))))

(defun ifeq-rule (target deps var val then-cmds else-cmds)
  "Platform-conditional build rule."
  (with-output-to-string (s)
    (format s "~A:~@[ ~A~]~%" target deps)
    (format s "ifeq ($(~A),~A)~%" var val)
    (dolist (c then-cmds) (format s "	~A~%" c))
    (format s "else~%")
    (dolist (c else-cmds) (format s "	~A~%" c))
    (format s "endif~%")))

(defun phony (&rest targets)
  (format nil ".PHONY:~{ ~A~}" targets))

;;; ── Fragment generators (one per spec concept) ──

(defun gen-header (spec-file)
  (list
   (format nil "# ════════════════════════════════════════════════════════════════")
   (format nil "# Makefile — generated from ~A" spec-file)
   (format nil "# Re-generate:  sbcl --load gen-makefile.lisp --quit")
   (format nil "# ════════════════════════════════════════════════════════════════")
   ""
   (var-def "SHELL" "/bin/bash")
   (var-def "TIMEOUT" "1")
   (var-def "UNAME_S" "$(shell uname -s)")))

(defun gen-suite-var (body)
  (list (var-def "TEST_DIR" (prop1 'test-suite body))))

(defun gen-src-vars (tgts gen-dir)
  (cons "# ── Sources ──"
        (cons ""
              (mapcar (lambda (tgt)
                        (var-def (format nil "GEN_~A" (un (car tgt)))
                                 (format nil "~A/~A" gen-dir (prop1 'file (cdr tgt)))))
                      tgts))))

(defun gen-bin-vars (tgts bin-dir)
  (cons "# ── Binaries ──"
        (cons ""
              (loop for tgt in tgts
                    when (or (prop1 'compile (cdr tgt))
                             (prop1 'compile-darwin (cdr tgt)))
                    collect (var-def (format nil "BIN_~A" (un (car tgt)))
                                    (format nil "~A/~A" bin-dir (ln (car tgt))))))))

(defun gen-phony (tgts)
  (list (apply #'phony
               (append '("all" "project" "clean" "distclean" "check" "test")
                       (loop for tgt in tgts
                             collect (format nil "test-~A" (ln (car tgt)))
                             when (or (prop1 'compile (cdr tgt))
                                      (prop1 'compile-darwin (cdr tgt)))
                             collect (format nil "build-~A" (ln (car tgt))))))))

(defun gen-project (projector gen-dir)
  (list (rule "all" "project")
        (rule "project" nil
              (format nil "@mkdir -p ~A" gen-dir)
              projector)))

(defun gen-build-target (tgt gen-dir bin-dir)
  "Generate build rule for one compiled target."
  (let* ((name (car tgt))
         (props (cdr tgt))
         (file (prop1 'file props))
         (cmd (prop1 'compile props))
         (cmd-darwin (prop1 'compile-darwin props))
         (cmd-linux (prop1 'compile-linux props))
         (setup (prop 'setup-linux props))
         (src (format nil "~A/~A" gen-dir file))
         (bin (format nil "~A/~A" bin-dir (ln name)))
         (target (format nil "build-~A" (ln name)))
         (dep (format nil "$(GEN_~A)" (un name)))
         (mkdir (format nil "@mkdir -p ~A" bin-dir)))
    (cond
      ((or cmd-darwin cmd-linux)
       (let ((darwin-cmds (list mkdir (subst-vars (or cmd-darwin cmd)
                                                  :src src :bin bin :bindir bin-dir)))
             (linux-cmds (append
                          (list mkdir)
                          (mapcar (lambda (s) (format nil "@# ~A" s)) (or setup nil))
                          (list (subst-vars (or cmd-linux cmd)
                                            :src src :bin bin :bindir bin-dir)))))
         (ifeq-rule target dep "UNAME_S" "Darwin" darwin-cmds linux-cmds)))
      (cmd
       (rule target dep mkdir
             (subst-vars cmd :src src :bin bin :bindir bin-dir)))
      (t ""))))

(defun gen-test-harness ()
  "The run_test macro — composed as a single string."
  ;; This is the ONE place where we have the test loop logic.
  ;; It's not a template — it's the implementation of the test runner.
  (format nil "~
define run_test
	@echo \"Testing $(2)...\"
	@pass=0; fail=0; total=0; \\
	for d in $(TEST_DIR)/*/; do \\
		d=\"$${d%%/}\"; \\
		for sd in \"$$d\" \"$$d\"/*/; do \\
			[ -d \"$$sd\" ] || continue; \\
			[ \"$$sd\" != \"$$d\" ] && { bn=$$(basename \"$$sd\"); echo \"$$bn\" | grep -qE '^[0-9]+$$' || continue; }; \\
			[ -f \"$$sd/in.yaml\" ] || { [ -f \"$$sd/error\" ] && total=$$((total+1)) && pass=$$((pass+1)); continue; }; \\
			total=$$((total+1)); \\
			if [ -f \"$$sd/error\" ]; then \\
				(timeout $(TIMEOUT) $(1) \"$$sd/in.yaml\" >/dev/null 2>&1); \\
				[ $$? -ne 0 ] && pass=$$((pass+1)) || fail=$$((fail+1)); \\
			else \\
				result=$$( (timeout $(TIMEOUT) $(1) \"$$sd/in.yaml\") 2>/dev/null) || true; \\
				echo \"$$result\" | grep -q \"^OK:\" && pass=$$((pass+1)) || fail=$$((fail+1)); \\
			fi; \\
		done; \\
	done; \\
	echo \"$(2): $$pass/$$total passed ($$fail failed)\"
endef"))

(defun gen-test-target (tgt bin-dir)
  "Generate test rule for one target."
  (let* ((name (car tgt))
         (props (cdr tgt))
         (interp (prop1 'interp props))
         (run-cmd (prop1 'run props))
         (bin (format nil "~A/~A" bin-dir (ln name))))
    (cond
      ;; Interpreted: depends on source, runs via interpreter
      (interp
       (rule (format nil "test-~A" (ln name))
             (format nil "$(GEN_~A)" (un name))
             (format nil "$(call run_test,~A $(GEN_~A),~A)"
                     interp (un name) (cn name))))
      ;; Compiled with custom run command
      (run-cmd
       (rule (format nil "test-~A" (ln name))
             (format nil "build-~A" (ln name))
             (format nil "$(call run_test,~A,~A)"
                     (subst-vars run-cmd :bin bin :bindir bin-dir) (cn name))))
      ;; Compiled, binary is the runner
      (t
       (rule (format nil "test-~A" (ln name))
             (format nil "build-~A" (ln name))
             (format nil "$(call run_test,~A,~A)" bin (cn name)))))))

(defun gen-test-all (tgts)
  (list (format nil "test:~{ test-~(~A~)~}" (mapcar #'car tgts))))

(defun gen-check (tgts)
  (cons (format nil "check:~%	@echo \"=== Toolchains ===\"")
        (mapcar (lambda (tgt)
                  (let ((dep1 (car (prop 'deps (cdr tgt)))))
                    (format nil "	@which ~A >/dev/null 2>&1 && echo \"  ~A: $$(~A --version 2>&1 | head -1)\" || echo \"  ~A: NOT FOUND\""
                            dep1 (cn (car tgt)) dep1 (cn (car tgt)))))
                tgts)))

(defun gen-clean (gen-dir bin-dir)
  (list (rule "clean" nil
              (format nil "rm -rf ~A/" bin-dir)
              (format nil "rm -f ~A/*.class ~A/*.o ~A/*.hi" gen-dir gen-dir gen-dir))
        (rule "distclean" "clean"
              (format nil "rm -rf ~A/" gen-dir))))

;;; ── Compose and emit ──

(defun emit-makefile (fragments stream)
  "Write list of fragments (strings) to stream, blank-line separated."
  (dolist (f fragments)
    (cond
      ((listp f) (dolist (line f) (write-line line stream)) (terpri stream))
      ((string= f "") (terpri stream))
      (t (write-line f stream)))))

(defun gen-makefile (spec-file output-file)
  (let* ((spec (read-spec spec-file))
         (body (cddr spec))
         (projector (prop1 'projector body))
         (test-suite (or (prop1 'test-suite body) "yaml-test-suite"))
         (gen-dir (or (prop1 'gen-dir body) "gen"))
         (bin-dir (or (prop1 'bin-dir body) "bin"))
         (tgts (targets spec)))
    (with-open-file (out output-file :direction :output :if-exists :supersede)
      (emit-makefile
       (list
        ;; Header & variables
        (gen-header spec-file)
        (gen-suite-var body)
        (gen-src-vars tgts gen-dir)
        (gen-bin-vars tgts bin-dir)
        (gen-phony tgts)
        ;; Project
        (gen-project projector gen-dir)
        ;; Build targets (one per compiled target)
        (comment-block "── Build ──")
        (mapcar (lambda (tgt)
                  (when (or (prop1 'compile (cdr tgt))
                            (prop1 'compile-darwin (cdr tgt)))
                    (gen-build-target tgt gen-dir bin-dir)))
                tgts)
        ;; Test harness + targets
        (comment-block "── Test ──")
        (gen-test-harness)
        (mapcar (lambda (tgt) (gen-test-target tgt bin-dir)) tgts)
        (gen-test-all tgts)
        ;; Check + clean
        (gen-check tgts)
        (gen-clean gen-dir bin-dir))
       out))
    (format t "; Makefile generated → ~A~%" output-file)))

(gen-makefile "build-spec.scm" "Makefile")
