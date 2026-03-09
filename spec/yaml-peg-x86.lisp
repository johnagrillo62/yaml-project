;;;; peg-x86.lisp — x86-64 NASM (Linux) target for emit-yaml-peg.lisp
;;;;
;;;; x86-64 has no closures, no GC, no strings, no data structures.
;;;; Like bash: every combinator is a procedure using call/ret.
;;;; Grammar rules become labeled procedures. Wrapper functions
;;;; stand in for closures. All state in global .bss variables.
;;;; AST is flat parallel arrays. Input is position-tracked globals.
;;;;
;;;; Calling convention:
;;;;   - All PEG state in globals (pos, line, col, FAILED, RVAL, etc.)
;;;;   - Parameters passed via dedicated globals (pN, pC, pM, pT)
;;;;   - Combinator args passed in rdi, rsi (function pointers, ints, string ptrs)
;;;;   - seq/alt are inlined as call/check sequences
;;;;   - Each rule: label, body, ret

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "x86-64")
(def-tgt "default-output" "yaml_reader.asm")
(def-tgt "comment-prefix" ";")
(def-tgt "call-style" "x86")  ;; x86 register-based calling

(def-tgt "keywords"
  '("mov" "push" "pop" "call" "ret" "jmp" "je" "jne" "jl" "jg" "jle" "jge"
    "cmp" "add" "sub" "inc" "dec" "xor" "or" "and" "not" "test" "shl" "shr"
    "section" "global" "extern" "db" "dw" "dd" "dq" "resb" "resd" "resq"
    "bits" "default" "rel" "rep" "times" "neg"))
(def-tgt "keyword-prefix" "r_")

;;; ── Identifier rules ──

(def-tgt "ident-prefix" "")
(def-tgt "ident-transform" :snake)

;;; ── String data accumulator ──
;;; x86 needs string literals in .data section. We collect them during
;;; compilation and emit them as part of the wrapper section.

(defvar *x86-strings* nil "Alist of (label . string-value)")
(defvar *x86-string-counter* 0)

(def-tgt "str-wrap"
  (lambda (quoted-str)
    ;; quoted-str looks like "\"---\"" — strip the outer quotes
    (let* ((s (string-trim "\"" quoted-str))
           (existing (rassoc s *x86-strings* :test #'string=)))
      (if existing
          (car existing)
          (let ((label (format nil "_str~D" (incf *x86-string-counter*))))
            (push (cons label s) *x86-strings*)
            label)))))

;;; ── Wrapper format — asm label + body + ret ──

(def-tgt "wrapper-fmt"
  (lambda (name body)
    (format nil "~A:~%    ~A~%    ret" name body)))

;;; ── Pre-wrappers hook — emit accumulated string data ──

(def-tgt "pre-wrappers"
  (lambda ()
    (when *x86-strings*
      (emitf "~%section .data~%")
      (emitf "; ── String literals ──~%")
      (dolist (pair (reverse *x86-strings*))
        (let ((label (car pair))
              (val (cdr pair)))
          ;; Emit as null-terminated byte string
          (emitf "    ~A: db " label)
          (loop for c across val
                for i from 0
                do (when (> i 0) (princ ", " *out*))
                   (princ (char-code c) *out*))
          (emitf ", 0~%")))
      (emitf "~%section .text~%"))))

;;; ── Closure wrapping ──
;;; Not used directly (call-style is "wrapper"), but needed as fallback.

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    body))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    body))

;;; ── Seq/Alt ──
;;; Inline call/check sequences. Each wrapper label is called in order.

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (let ((id (incf *bash-wrapper-counter*)))
      (with-output-to-string (s)
        (format s "call save_inp~%")
        (loop for fn in wrapped-fns
              do (progn
                   (format s "    call ~A~%" fn)
                   (format s "    cmp byte [FAILED], 1~%")
                   (format s "    je .seq_fail_~D~%" id)))
        (format s "    jmp .seq_ok_~D~%" id)
        (format s ".seq_fail_~D:~%" id)
        (format s "    call restore_inp~%")
        (format s "    mov byte [FAILED], 1~%")
        (format s ".seq_ok_~D:" id)))))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (let ((id (incf *bash-wrapper-counter*)))
      (with-output-to-string (s)
        (let ((end-lbl (format nil ".alt_ok_~D" id)))
          (loop for fn in wrapped-fns
                for i from 0
                do (progn
                     (format s "    call save_inp~%")
                     (format s "    call ~A~%" fn)
                     (format s "    cmp byte [FAILED], 0~%")
                     (format s "    je ~A~%" end-lbl)
                     (format s "    call restore_inp~%")))
          (format s "    mov byte [FAILED], 1~%")
          (format s "~A:" end-lbl))))))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (let ((id (incf *bash-wrapper-counter*)))
      (with-output-to-string (s)
        (let ((end-lbl (format nil ".sw_end_~D" id)))
          (loop for (val body) in cases
                for i from 0
                do (let ((skip (format nil ".sw_skip_~D_~D" id i)))
                     (format s "    mov rdi, [~A]~%" param)
                     (format s "    lea rsi, [str_~A]~%"
                             (string-downcase (substitute #\_ #\- val)))
                     (format s "    call str_eq~%")
                     (format s "    test al, al~%")
                     (format s "    jz ~A~%" skip)
                     (format s "    ~A~%" body)
                     (format s "    jmp ~A~%" end-lbl)
                     (format s "~A:~%" skip)))
          (format s "    call fail_no_case~%")
          (format s "~A:" end-lbl))))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (let ((id (incf *bash-wrapper-counter*)))
      (format nil "~A~%    cmp byte [FAILED], 1~%    je .let_fail_~D~%    mov rax, [RTAGINT]~%    mov [~A], rax~%    call save_inp~%    ~A~%.let_fail_~D:"
              expr id vname rest id))))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (let ((id (incf *bash-wrapper-counter*)))
      (format nil "~A~%    cmp byte [FAILED], 1~%    je .let_fail_~D~%    mov rax, [RTAG]~%    mov [~A], rax~%    call save_inp~%    ~A~%.let_fail_~D:"
              expr id vname rest id))))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (format nil "qword [~A]" (peg-ident sym))))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "str_~A" (string-downcase (substitute #\_ #\- s)))))

(def-tgt "char-cast"
  (lambda (name) name))

(def-tgt "in-flow-call"
  (lambda (arg)
    (let ((w (make-bash-wrapper
              (if (and (stringp arg) (>= (length arg) 6) (string= (subseq arg 0 6) "ARITH:"))
                  (format nil "call ~A~%    mov rax, qword [_arith_tmp]~%    mov [pC], rax~%    call in_flow~%    mov rax, [RTAG]~%    mov [_arith_tmp], rax" (subseq arg 6))
                  (format nil "mov rax, ~A~%    mov [pC], rax~%    call in_flow~%    mov rax, [RTAG]~%    mov [_arith_tmp], rax" arg)))))
      (format nil "ARITH:~A" w))))

(def-tgt "seq-spaces-call"
  (lambda (n c)
    (let ((w (make-bash-wrapper
              (with-output-to-string (s)
                (if (and (stringp n) (>= (length n) 6) (string= (subseq n 0 6) "ARITH:"))
                    (format s "call ~A~%    mov rax, qword [_arith_tmp]~%    mov [pN], rax~%    " (subseq n 6))
                    (format s "mov rax, ~A~%    mov [pN], rax~%    " n))
                (if (and (stringp c) (>= (length c) 6) (string= (subseq c 0 6) "ARITH:"))
                    (format s "call ~A~%    mov rax, qword [_arith_tmp]~%    mov [pC], rax~%    " (subseq c 6))
                    (format s "mov rax, ~A~%    mov [pC], rax~%    " c))
                (format s "call seq_spaces~%    mov rax, [RTAGINT]~%    mov [_arith_tmp], rax")))))
      (format nil "ARITH:~A" w))))

;;; ── Combinator overrides ──
;;; The "wrapper" call-style produces fn(arg1, arg2) syntax.
;;; We override each combinator to emit proper x86 register setup + call.

(def-tgt "comb-match-cp" "match_cp")
(def-tgt "comb-match-range" "match_range")
(def-tgt "comb-match-str" "match_str")
(def-tgt "comb-ok" "ok_r")
(def-tgt "comb-star" "peg_star")
(def-tgt "comb-plus" "peg_plus")
(def-tgt "comb-opt" "peg_opt")
(def-tgt "comb-neg" "peg_neg")
(def-tgt "comb-rep" "peg_rep")
(def-tgt "comb-ahead" "peg_ahead")
(def-tgt "comb-behind" "peg_behind")
(def-tgt "comb-minus" "peg_minus")
(def-tgt "comb-build" "peg_build")
(def-tgt "comb-scalar" "peg_scalar")
(def-tgt "comb-collect" "peg_collect")
(def-tgt "comb-detect" "peg_detect_indent")
(def-tgt "comb-parse-int" "peg_parse_int")
(def-tgt "comb-parse-sym" "peg_parse_sym")
(def-tgt "comb-val" "peg_val")

;;; ── Rule call with params ──
;;; When a rule is called with parameters (n, c, m, t), set up the
;;; global parameter slots before calling.

(def-tgt "rule-call-with-params"
  (lambda (name params compiled-args)
    (with-output-to-string (s)
      (loop for p in params
            for a in compiled-args
            for pn = (peg-ident p)
            do (if (and (stringp a) (>= (length a) 6) (string= (subseq a 0 6) "ARITH:"))
                   (let ((wrapper (subseq a 6)))
                     (format s "call ~A~%    mov rax, qword [_arith_tmp]~%    mov [~A], rax~%    " wrapper pn))
                   (format s "mov rax, ~A~%    mov [~A], rax~%    " a pn)))
      (format s "call ~A" name))))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (with-output-to-string (s)
          (format s "~A:" name)
          (loop for p in params
                for pn = (symbol-name p)
                for ident = (peg-ident p)
                do (if (member pn '("N" "M") :test #'string-equal)
                       (format s "~%    ; param ~A = int (in [~A])" ident ident)
                       (format s "~%    ; param ~A = ctx (in [~A])" ident ident))))
        (format nil "~A:" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "~A~%    ~A~%    ret~%" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"; ════════════════════════════════════════════════════════════════
; yaml_reader.asm — YAML 1.2 parser (x86-64 NASM, Linux)
; ════════════════════════════════════════════════════════════════

bits 64
default rel
global _start

section .data
    str_block_in:    db 'BLOCK-IN', 0
    str_block_out:   db 'BLOCK-OUT', 0
    str_flow_in:     db 'FLOW-IN', 0
    str_flow_out:    db 'FLOW-OUT', 0
    str_block_key:   db 'BLOCK-KEY', 0
    str_flow_key:    db 'FLOW-KEY', 0
    str_strip:       db 'STRIP', 0
    str_clip:        db 'CLIP', 0
    str_keep:        db 'KEEP', 0
    ok_prefix:       db 'OK: ', 0
    fail_prefix:     db 'FAIL', 10, 0
    chars_suffix:    db ' chars', 10, 0

section .bss
    src_ptr:     resq 1
    src_len:     resq 1
    pos:         resq 1
    line_num:    resq 1
    col_num:     resq 1
    FAILED:      resb 1
    RVAL_BUF:    resb 65536
    RVAL_LEN:    resq 1
    RTAG:        resq 1
    RTAGINT:     resq 1
    SAVE_SP:     resq 1
    SAVE_STACK:  resq 49152
    ast_tag:     resq 16384
    ast_text:    resq 16384
    ast_tlen:    resq 16384
    ast_first:   resq 16384
    ast_next:    resq 16384
    ast_leaf:    resb 16384
    ast_count:   resq 1
    RAST:        resq 1
    RAST_LIST:   resq 4096
    RAST_LCNT:   resq 1
    pN:          resq 1
    pC:          resq 1
    pM:          resq 1
    pT:          resq 1
    n:           resq 1
    c:           resq 1
    m:           resq 1
    t:           resq 1
    itoa_buf:    resb 32
    read_buf:    resb 1048576
    _star_save:  resq 1
    _arith_tmp:  resq 1

section .text")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"; ── Input ──

save_inp:
    mov rdi, [SAVE_SP]
    lea rsi, [SAVE_STACK]
    mov rax, [pos]
    mov [rsi + rdi*8], rax
    mov rax, [line_num]
    mov [rsi + rdi*8 + 8], rax
    mov rax, [col_num]
    mov [rsi + rdi*8 + 16], rax
    add qword [SAVE_SP], 3
    ret

restore_inp:
    sub qword [SAVE_SP], 3
    mov rdi, [SAVE_SP]
    lea rsi, [SAVE_STACK]
    mov rax, [rsi + rdi*8]
    mov [pos], rax
    mov rax, [rsi + rdi*8 + 8]
    mov [line_num], rax
    mov rax, [rsi + rdi*8 + 16]
    mov [col_num], rax
    ret

at_eof:
    mov rax, [pos]
    cmp rax, [src_len]
    setge al
    movzx eax, al
    ret

peek_cp:
    mov rax, [pos]
    cmp rax, [src_len]
    jge .eof
    mov rsi, [src_ptr]
    movzx eax, byte [rsi + rax]
    ret
.eof:
    mov eax, -1
    ret

adv_one:
    mov rax, [pos]
    cmp rax, [src_len]
    jge .done
    mov rsi, [src_ptr]
    movzx ecx, byte [rsi + rax]
    inc qword [pos]
    cmp cl, 10
    jne .not_nl
    inc qword [line_num]
    mov qword [col_num], 0
    ret
.not_nl:
    inc qword [col_num]
.done:
    ret"

"; ── Result helpers ──

ok_r:
    mov byte [FAILED], 0
    ret

fail_r:
    mov byte [FAILED], 1
    ret

fail_no_case:
    call fail_r
    ret"

"; ── Core combinators ──

match_cp:
    ; rdi = expected codepoint
    push rdi
    call peek_cp
    pop rdi
    cmp eax, edi
    jne .mc_fail
    lea rsi, [RVAL_BUF]
    add rsi, [RVAL_LEN]
    mov [rsi], al
    inc qword [RVAL_LEN]
    call adv_one
    mov byte [FAILED], 0
    ret
.mc_fail:
    mov byte [FAILED], 1
    ret

match_range:
    ; rdi = lo, rsi = hi
    push rdi
    push rsi
    call peek_cp
    pop rsi
    pop rdi
    cmp eax, edi
    jl .mr_fail
    cmp eax, esi
    jg .mr_fail
    lea rcx, [RVAL_BUF]
    add rcx, [RVAL_LEN]
    mov [rcx], al
    inc qword [RVAL_LEN]
    call adv_one
    mov byte [FAILED], 0
    ret
.mr_fail:
    mov byte [FAILED], 1
    ret

match_str:
    ; rdi = pointer to null-terminated target string
    push rdi
    mov rsi, [src_ptr]
    add rsi, [pos]
    mov rcx, [src_len]
    sub rcx, [pos]
    xor rdx, rdx
.ms_loop:
    movzx eax, byte [rdi + rdx]
    test al, al
    jz .ms_match
    cmp rdx, rcx
    jge .ms_fail_pop
    movzx r8d, byte [rsi + rdx]
    cmp al, r8b
    jne .ms_fail_pop
    inc rdx
    jmp .ms_loop
.ms_match:
    ; matched rdx chars
    pop rdi
    mov rcx, rdx
    lea rsi, [RVAL_BUF]
    add rsi, [RVAL_LEN]
    push rcx
    xor r8, r8
.ms_copy:
    cmp r8, rcx
    jge .ms_adv
    movzx eax, byte [rdi + r8]
    mov [rsi + r8], al
    inc r8
    jmp .ms_copy
.ms_adv:
    pop rcx
    add [RVAL_LEN], rcx
.ms_adv_loop:
    test rcx, rcx
    jz .ms_done
    call adv_one
    dec rcx
    jmp .ms_adv_loop
.ms_done:
    mov byte [FAILED], 0
    ret
.ms_fail_pop:
    pop rdi
    mov byte [FAILED], 1
    ret

sol:
    cmp qword [col_num], 0
    jne .sol_fail
    mov byte [FAILED], 0
    ret
.sol_fail:
    mov byte [FAILED], 1
    ret

eof_ok:
    call at_eof
    test eax, eax
    jz .eof_fail
    mov byte [FAILED], 0
    ret
.eof_fail:
    mov byte [FAILED], 1
    ret

str_eq:
    ; rdi, rsi = null-terminated strings, al = 1 if equal
.se_loop:
    movzx eax, byte [rdi]
    movzx ecx, byte [rsi]
    cmp al, cl
    jne .se_neq
    test al, al
    jz .se_eq
    inc rdi
    inc rsi
    jmp .se_loop
.se_eq:
    mov al, 1
    ret
.se_neq:
    xor al, al
    ret"

"; ── Higher-order combinators ──
; These take function pointers (label addresses) and call them.

peg_star:
    ; rdi = function pointer (wrapper label)
    ; Calls f repeatedly until it fails or pos doesn't advance
    mov [_star_save], rdi
.ps_loop:
    mov rax, [pos]
    push rax
    call save_inp
    mov rdi, [_star_save]
    call rdi
    pop rax
    cmp byte [FAILED], 1
    je .ps_done
    cmp [pos], rax
    jle .ps_done
    jmp .ps_loop
.ps_done:
    mov byte [FAILED], 0
    ret

peg_plus:
    ; rdi = function pointer
    push rdi
    call rdi
    pop rdi
    cmp byte [FAILED], 1
    je .pp_fail
    call peg_star
    mov byte [FAILED], 0
    ret
.pp_fail:
    ret

peg_opt:
    ; rdi = function pointer
    call save_inp
    call rdi
    cmp byte [FAILED], 0
    je .po_ok
    call restore_inp
    mov byte [FAILED], 0
.po_ok:
    ret

peg_neg:
    ; rdi = function pointer
    call save_inp
    call rdi
    cmp byte [FAILED], 1
    je .pn_yes
    call restore_inp
    mov byte [FAILED], 1
    ret
.pn_yes:
    call restore_inp
    mov byte [FAILED], 0
    ret

peg_rep:
    ; rdi = count, rsi = function pointer
    push rsi
    push rdi
    mov rcx, rdi
.pr_loop:
    test rcx, rcx
    jz .pr_done
    push rcx
    mov rdi, [rsp + 8]   ; function pointer
    call rdi
    pop rcx
    cmp byte [FAILED], 1
    je .pr_fail
    dec rcx
    jmp .pr_loop
.pr_done:
    pop rdi
    pop rsi
    mov byte [FAILED], 0
    ret
.pr_fail:
    pop rdi
    pop rsi
    ret

peg_ahead:
    ; rdi = function pointer
    call save_inp
    call rdi
    cmp byte [FAILED], 1
    je .pa_fail
    call restore_inp
    mov byte [FAILED], 0
    ret
.pa_fail:
    call restore_inp
    ret

peg_behind:
    ; rdi = function pointer
    mov rax, [pos]
    test rax, rax
    jz .pb_fail
    push rdi
    dec qword [pos]
    ; Approximate: don't fix line/col for lookbehind
    call save_inp
    pop rdi
    call rdi
    cmp byte [FAILED], 1
    je .pb_fail2
    call restore_inp
    inc qword [pos]
    mov byte [FAILED], 0
    ret
.pb_fail2:
    call restore_inp
    inc qword [pos]
.pb_fail:
    mov byte [FAILED], 1
    ret

peg_minus:
    ; rdi = function pointer a, rsi = function pointer b
    push rsi
    push rdi
    call save_inp
    call rdi
    cmp byte [FAILED], 1
    je .pm_fail
    mov rax, [pos]
    push rax
    call restore_inp
    call save_inp
    mov rdi, [rsp + 8]   ; fn b
    call rdi
    pop rax              ; pos from a
    cmp byte [FAILED], 1
    je .pm_a_wins
    ; b also matched — check if same length
    cmp [pos], rax
    je .pm_excl
.pm_a_wins:
    ; restore and run a again
    call restore_inp
    pop rdi
    pop rsi
    call rdi
    ret
.pm_excl:
    call restore_inp
    pop rdi
    pop rsi
    mov byte [FAILED], 1
    ret
.pm_fail:
    call restore_inp
    pop rdi
    pop rsi
    ret"

"; ── Context ──

in_flow:
    mov rdi, [pC]
    lea rsi, [str_flow_out]
    call str_eq
    test al, al
    jnz .if_fi
    mov rdi, [pC]
    lea rsi, [str_flow_in]
    call str_eq
    test al, al
    jnz .if_fi
    lea rax, [str_flow_key]
    mov [RTAG], rax
    ret
.if_fi:
    lea rax, [str_flow_in]
    mov [RTAG], rax
    ret

seq_spaces:
    mov rdi, [pC]
    lea rsi, [str_block_out]
    call str_eq
    test al, al
    jz .ss_no
    mov rax, [pN]
    dec rax
    mov [RTAGINT], rax
    ret
.ss_no:
    mov rax, [pN]
    mov [RTAGINT], rax
    ret"

"; ── YAML extensions ──

peg_build:
    ; rdi = tag string ptr, rsi = function pointer (body wrapper)
    push rdi
    push rsi
    call rsi
    cmp byte [FAILED], 1
    je .b_fail
    mov rax, [ast_count]
    pop rsi
    pop rdi
    mov [ast_tag + rax*8], rdi
    mov byte [ast_leaf + rax], 0
    mov qword [ast_first + rax*8], -1
    inc qword [ast_count]
    mov [RAST], rax
    mov byte [FAILED], 0
    ret
.b_fail:
    pop rsi
    pop rdi
    ret

peg_scalar:
    ; rdi = function pointer (body wrapper)
    push rdi
    call rdi
    pop rdi
    cmp byte [FAILED], 1
    je .sc_fail
    mov rax, [ast_count]
    lea rsi, [RVAL_BUF]
    mov [ast_text + rax*8], rsi
    mov rcx, [RVAL_LEN]
    mov [ast_tlen + rax*8], rcx
    mov byte [ast_leaf + rax], 1
    inc qword [ast_count]
    mov [RAST], rax
    mov byte [FAILED], 0
    ret
.sc_fail:
    ret

peg_collect:
    ; rdi = function pointer — passthrough
    jmp rdi

peg_detect_indent:
    ; rdi = n
    push rdi
    mov rax, [pos]
    mov rsi, [src_ptr]
    mov rcx, [src_len]
    xor edx, edx
.di_sp:
    lea r8, [rax + rdx]
    cmp r8, rcx
    jge .di_def
    cmp byte [rsi + r8], 32
    jne .di_chk
    inc edx
    jmp .di_sp
.di_chk:
    cmp byte [rsi + r8], 10
    je .di_next
    pop rdi
    sub edx, edi
    cmp edx, 1
    jge .di_set
    mov edx, 1
.di_set:
    mov [RTAGINT], rdx
    mov byte [FAILED], 0
    ret
.di_next:
    lea r8, [rax + rdx]
.di_skip:
    cmp r8, rcx
    jge .di_def
    cmp byte [rsi + r8], 10
    je .di_nl
    inc r8
    jmp .di_skip
.di_nl:
    inc r8
    mov rax, r8
    xor edx, edx
    jmp .di_sp
.di_def:
    pop rdi
    mov qword [RTAGINT], 1
    mov byte [FAILED], 0
    ret

peg_parse_int:
    ; rdi = function pointer
    push rdi
    call rdi
    pop rdi
    cmp byte [FAILED], 1
    je .pi_fail
    xor eax, eax
    lea rsi, [RVAL_BUF]
    mov rcx, [RVAL_LEN]
    xor edx, edx
.pi_loop:
    cmp rdx, rcx
    jge .pi_done
    movzx r8d, byte [rsi + rdx]
    sub r8d, 48
    imul eax, 10
    add eax, r8d
    inc edx
    jmp .pi_loop
.pi_done:
    mov [RTAGINT], rax
    ret
.pi_fail:
    ret

peg_parse_sym:
    ; rdi = function pointer, rsi = symbol string ptr
    push rsi
    push rdi
    call rdi
    pop rdi
    pop rsi
    cmp byte [FAILED], 1
    je .ps_fail
    mov [RTAG], rsi
    ret
.ps_fail:
    ret

peg_val:
    ; rdi = value string ptr
    mov byte [FAILED], 0
    mov [RTAG], rdi
    ret"

"; ── I/O and Entry ──

print_str:
    push rdi
    xor ecx, ecx
.ps_len:
    cmp byte [rdi + rcx], 0
    je .ps_out
    inc ecx
    jmp .ps_len
.ps_out:
    mov rax, 1
    pop rsi
    mov edx, ecx
    mov rdi, 1
    syscall
    ret

print_int:
    lea rsi, [itoa_buf + 30]
    mov byte [rsi], 0
    mov rax, rdi
    test rax, rax
    jnz .pi_conv
    dec rsi
    mov byte [rsi], 48
    jmp .pi_pr
.pi_conv:
    mov rcx, 10
.pi_div:
    test rax, rax
    jz .pi_pr
    xor edx, edx
    div rcx
    add dl, 48
    dec rsi
    mov [rsi], dl
    jmp .pi_div
.pi_pr:
    mov rdi, rsi
    call print_str
    ret

_start:
    mov rax, 0
    mov rdi, 0
    lea rsi, [read_buf]
    mov rdx, 1048576
    syscall
    cmp rax, 0
    jle .exit_fail

    lea rcx, [read_buf]
    mov [src_ptr], rcx
    mov [src_len], rax
    mov qword [pos], 0
    mov qword [line_num], 1
    mov qword [col_num], 0
    mov byte [FAILED], 0
    mov qword [ast_count], 0
    mov qword [SAVE_SP], 0
    mov qword [RVAL_LEN], 0

    call l_yaml_stream

    cmp byte [FAILED], 1
    je .exit_fail

    lea rdi, [ok_prefix]
    call print_str
    mov rdi, [pos]
    call print_int
    lea rdi, [chars_suffix]
    call print_str

    mov rax, 60
    xor rdi, rdi
    syscall

.exit_fail:
    lea rdi, [fail_prefix]
    call print_str
    mov rax, 60
    mov rdi, 1
    syscall
"))

;;; ── API / Concerns / Namespace ──

(def-tgt "api" "")
(def-tgt "main-fn" nil)
(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)
