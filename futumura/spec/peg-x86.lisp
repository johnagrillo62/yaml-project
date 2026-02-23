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
;;;;   - seq/alt are inlined as call/check sequences
;;;;   - Each rule: label, body, ret

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "x86-64")
(def-tgt "default-output" "yaml_reader.asm")
(def-tgt "comment-prefix" ";")
(def-tgt "call-style" "bash")  ;; wrapper-based, no closures

(def-tgt "keywords"
  '("mov" "push" "pop" "call" "ret" "jmp" "je" "jne" "jl" "jg" "jle" "jge"
    "cmp" "add" "sub" "inc" "dec" "xor" "or" "and" "not" "test" "shl" "shr"
    "section" "global" "extern" "db" "dw" "dd" "dq" "resb" "resd" "resq"
    "bits" "default" "rel" "rep" "times" "neg"))
(def-tgt "keyword-prefix" "r_")

;;; ── Identifier rules ──

(def-tgt "ident-prefix" "")
(def-tgt "ident-transform" :snake)

;;; ── Closure wrapping ──
;;; Like bash: wrappers are labels. ref-wrap/box-wrap return label names.

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
    (format nil "~A~%    cmp byte [FAILED], 1~%    je .ret~%    mov rax, [RTAGINT]~%    mov [~A], rax~%    call save_inp~%    ~A"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "~A~%    cmp byte [FAILED], 1~%    je .ret~%    mov rax, [RTAG]~%    mov [~A], rax~%    call save_inp~%    ~A"
            expr vname rest)))

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
    (format nil "call in_flow")))

(def-tgt "seq-spaces-call"
  (lambda (n c)
    (format nil "call seq_spaces")))

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
    ; edi = expected codepoint
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
    ; edi = lo, esi = hi
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

build:
    ; rdi = tag string ptr, rsi = function pointer (body)
    push rdi
    call rsi
    cmp byte [FAILED], 1
    je .b_fail
    mov rax, [ast_count]
    pop rdi
    mov [ast_tag + rax*8], rdi
    mov byte [ast_leaf + rax], 0
    mov qword [ast_first + rax*8], -1
    inc qword [ast_count]
    mov [RAST], rax
    mov byte [FAILED], 0
    ret
.b_fail:
    pop rdi
    ret

scalar:
    ; rdi = function pointer (body)
    call rdi
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

collect:
    ; rdi = function pointer — passthrough
    jmp rdi

detect_indent:
    ; edi = n
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
    mov qword [RTAGINT], 1
    mov byte [FAILED], 0
    ret

parse_int:
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

parse_sym:
    ; rdi = function pointer, rsi = symbol string ptr
    push rsi
    call rdi
    pop rsi
    cmp byte [FAILED], 1
    je .ps_fail
    mov [RTAG], rsi
    ret
.ps_fail:
    ret

val:
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
