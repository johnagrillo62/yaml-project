;;;; emit-yaml-rust.lisp — Project yaml-grammar.scm → yaml_reader.rs
;;;;
;;;; Same 211 rules. Rust target. Uses move closures + String clone.
;;;;
;;;; Usage:
;;;;   sbcl --load yaml-eval.lisp --load emit-yaml-rust.lisp --quit
;;;;   rustc -O -o yaml-reader yaml_reader.rs

(in-package #:yaml-eval)

;;; ═══════════════════════════════════════════════════════════════════
;;; NAME MANGLING
;;; ═══════════════════════════════════════════════════════════════════

(defun rs-ident (sym)
  (let* ((s (string-downcase (symbol-name sym)))
         (s (substitute #\_ #\- s))
         (s (remove #\+ s)))
    (if (or (digit-char-p (char s 0))
            (member s '("as" "break" "const" "continue" "crate" "else" "enum"
                        "extern" "false" "fn" "for" "if" "impl" "in" "let"
                        "loop" "match" "mod" "move" "mut" "pub" "ref" "return"
                        "self" "static" "struct" "super" "trait" "true" "type"
                        "unsafe" "use" "where" "while" "yield" "box" "do")
                    :test #'string=))
        (format nil "r#~A" s)
        s)))

;;; ═══════════════════════════════════════════════════════════════════
;;; OUTPUT
;;; ═══════════════════════════════════════════════════════════════════

(defvar *out* nil)
(defun emit (&rest args) (dolist (a args) (princ a *out*)) (terpri *out*))
(defun emitf (fmt &rest args) (apply #'format *out* fmt args))
(defun blank () (terpri *out*))

;;; ═══════════════════════════════════════════════════════════════════
;;; EXPRESSION COMPILER
;;; ═══════════════════════════════════════════════════════════════════
;;;
;;; Rust strategy: closures use `move`, String params are cloned
;;; before each closure boundary. i32 params are Copy, no issue.
;;;
;;; env = list of param symbols currently in scope

(defvar *gram* nil)

(defun str-params (env)
  "Return the string (non-numeric) params in env."
  (remove-if (lambda (p) (member (symbol-name p) '("N" "M") :test #'string-equal)) env))

(defun clone-prefix (env)
  "Emit let-clones for all String params before a move closure."
  (let ((sp (str-params env)))
    (if sp
        (format nil "~{let ~A = ~A.clone(); ~}"
                (loop for p in sp
                      collect (format nil "~A__c" (rs-ident p))
                      collect (rs-ident p)))
        "")))

(defun rebind-suffix (env)
  "Inside the closure, rebind cloned names back to the original names."
  (let ((sp (str-params env)))
    (if sp
        (format nil "~{let ~A = ~A__c; ~}"
                (loop for p in sp
                      collect (rs-ident p)
                      collect (rs-ident p)))
        "")))

(defun lw (body env)
  "Closure for &dyn Fn — can borrow from enclosing scope."
  (format nil "|inp: Input| -> Result { ~A }" body))

(defun bw (body env)
  "Boxed closure for Vec<Box<dyn Fn>> — must clone String params before move."
  (let ((sp (str-params env)))
    (if sp
        (format nil "{ ~{let ~A = ~A.clone(); ~}Box::new(move |inp: Input| -> Result { ~{let ~A = ~A.clone(); ~}~A }) }"
                (loop for p in sp
                      collect (format nil "~A_c" (rs-ident p))
                      collect (rs-ident p))
                (loop for p in sp
                      collect (rs-ident p)
                      collect (format nil "~A_c" (rs-ident p)))
                body)
        (format nil "Box::new(move |inp: Input| -> Result { ~A })" body))))

(defun ce (expr env)
  "Compile expression → Rust Result expression."
  (cond
    ((null expr) "ok(inp)")
    ((eq expr 'EMPTY) "ok(inp)")
    ((integerp expr) (format nil "match_cp(inp, ~D)" expr))
    ((symbolp expr)
     (if (member expr env) (format nil "~A" (rs-ident expr))
         (format nil "~A(inp)" (rs-ident expr))))
    ((listp expr)
     (let ((op (car expr)) (args (cdr expr)))
       (case op
         (EMPTY  "ok(inp)")
         (CHAR   (ce-char (car args) env))
         (HEX    (ce-hex (car args)))
         (RANGE  (ce-range (car args) (cadr args)))
         (STR    (format nil "match_str(inp, ~S)" (string (car args))))
         (SEQ    (ce-seq args env))
         (ALT    (ce-alt args env))
         (STAR   (format nil "star(inp, &~A)" (lw (ce (car args) env) env)))
         (PLUS   (format nil "plus_(inp, &~A)" (lw (ce (car args) env) env)))
         (OPT    (format nil "opt(inp, &~A)" (lw (ce (car args) env) env)))
         (REF    (ce-ref (car args) (cdr args) env))
         (NOT    (format nil "neg(inp, &~A)" (lw (ce (car args) env) env)))
         (MINUS  (format nil "minus(inp, &~A, &~A)"
                         (lw (ce (car args) env) env) (lw (ce (cadr args) env) env)))
         (REPEAT (format nil "rep(inp, ~A, &~A)"
                         (ca (car args) env) (lw (ce (cadr args) env) env)))
         (SWITCH (ce-switch (car args) (cdr args) env))
         (LOOKAHEAD  (format nil "ahead(inp, &~A)" (lw (ce (car args) env) env)))
         (LOOKBEHIND (format nil "behind(inp, &~A)" (lw (ce (car args) env) env)))
         (STARTOFLINE "sol(inp)")
         (ENDOFINPUT  "eof_ok(inp)")
         (BUILD   (format nil "build(inp, ~S, &~A)"
                          (string (car args)) (lw (ce (cadr args) env) env)))
         (SCALAR  (format nil "scalar(inp, &~A)" (lw (ce (car args) env) env)))
         (COLLECT (format nil "collect(inp, &~A)" (lw (ce (car args) env) env)))
         (LET           (ce-let (car args) (cadr args) env))
         (DETECT-INDENT (format nil "detect_indent(inp, ~A)" (ca (car args) env)))
         (PARSE-INT     (format nil "parse_int(inp, &~A)" (lw (ce (car args) env) env)))
         (PARSE-SYM     (format nil "parse_sym(inp, &~A, ~S)"
                                (lw (ce (car args) env) env) (string (cadr args))))
         (VAL           (format nil "val(inp, ~S)" (string (car args))))
         ((+ -)  (ca expr env))
         (IN-FLOW    (format nil "in_flow(&~A)" (ca (cadr expr) env)))
         (SEQ-SPACES (format nil "seq_spaces(~A, &~A)" (ca (cadr expr) env) (ca (caddr expr) env)))
         (t (let ((rd (gethash op (gram-rules *gram*))))
              (if rd
                  (format nil "~A(inp~{, ~A~})" (rs-ident op)
                          (mapcar (lambda (a) (ca a env)) args))
                  (format nil "/* UNKNOWN ~S */" expr)))))))
    (t (format nil "/* ?? ~S */" expr))))

(defun ce-char (x env)
  (cond
    ((integerp x) (format nil "match_cp(inp, ~D)" x))
    ((and (symbolp x) (string= (symbol-name x) "SQUOTE")) "match_cp(inp, 39)")
    ((and (symbolp x) (string= (symbol-name x) "DQUOTE")) "match_cp(inp, 34)")
    ((and (symbolp x) (member x env)) (format nil "match_cp(inp, ~A as i32)" (rs-ident x)))
    ((symbolp x) (format nil "match_cp(inp, ~D)" (char-code (char (symbol-name x) 0))))
    (t (format nil "match_cp(inp, ~D)" x))))

(defun ce-hex (v)
  (let ((s (string-upcase (if (integerp v) (format nil "~D" v) (symbol-name v)))))
    (format nil "match_cp(inp, 0x~A)" s)))

(defun ce-range (lo hi)
  (flet ((hv (x)
           (if (and (listp x) (eq (car x) 'HEX))
               (format nil "0x~A" (string-upcase
                                   (if (integerp (cadr x)) (format nil "~D" (cadr x))
                                       (symbol-name (cadr x)))))
               (if (integerp x) (format nil "~D" x)
                   (format nil "~D" (char-code (char (symbol-name x) 0)))))))
    (format nil "match_range(inp, ~A, ~A)" (hv lo) (hv hi))))

(defun ce-seq (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      (format nil "seq(inp, vec![~{~A~^, ~}])"
              (mapcar (lambda (e) (bw (ce e env) env)) exprs))))

(defun ce-alt (exprs env)
  (if (= 1 (length exprs)) (ce (car exprs) env)
      (format nil "alt(inp, vec![~{~A~^, ~}])"
              (mapcar (lambda (e) (bw (ce e env) env)) exprs))))

(defun ce-ref (name call-args env)
  (if call-args
      (format nil "~A(inp~{, ~A~})" (rs-ident name)
              (mapcar (lambda (a) (ca a env)) call-args))
      (format nil "~A(inp)" (rs-ident name))))

(defun ce-switch (param cases env)
  (format nil "(|| -> Result {~{ ~A~} fail(inp.clone(), \"no case\") })()"
          (mapcar (lambda (c)
                    (format nil "if ~A == ~S { return ~A; }"
                            (rs-ident param)
                            (string-upcase (symbol-name (car c)))
                            (ce (cadr c) env)))
                  cases)))

(defun ce-let (bindings body env)
  (if (null bindings) (ce body env)
      (let* ((b (car bindings))
             (var (car b))
             (val-expr (cadr b))
             (vn (rs-ident var))
             (numeric-p (member (symbol-name var) '("N" "M") :test #'string-equal)))
        (if numeric-p
            (format nil "(|| -> Result { let r = ~A; if r.fail { return r; } let ~A: i32 = r.tag_int; let inp = r.rest; ~A })()"
                    (ce val-expr env) vn (ce-let (cdr bindings) body (cons var env)))
            (format nil "(|| -> Result { let r = ~A; if r.fail { return r; } let ~A: String = r.tag.clone(); let inp = r.rest; ~A })()"
                    (ce val-expr env) vn (ce-let (cdr bindings) body (cons var env)))))))

(defun ca (expr env)
  (cond
    ((integerp expr) (format nil "~D" expr))
    ((and (symbolp expr) (eq expr 'EMPTY)) "0")
    ((and (symbolp expr) (member expr env))
     (let ((pn (symbol-name expr)))
       (if (member pn '("N" "M") :test #'string-equal)
           (rs-ident expr)
           (format nil "~A.clone()" (rs-ident expr)))))
    ((and (symbolp expr)
          (member (symbol-name expr)
                  '("BLOCK-IN" "BLOCK-OUT" "FLOW-IN" "FLOW-OUT"
                    "BLOCK-KEY" "FLOW-KEY" "STRIP" "CLIP" "KEEP" "EMPTY")
                  :test #'string-equal))
     (format nil "~S.to_string()" (string-upcase (symbol-name expr))))
    ((symbolp expr) (rs-ident expr))
    ((and (listp expr) (eq (car expr) '+))
     (format nil "(~{~A~^ + ~})" (mapcar (lambda (e) (ca e env)) (cdr expr))))
    ((and (listp expr) (eq (car expr) '-))
     (let ((ps (mapcar (lambda (e) (ca e env)) (cdr expr))))
       (if (= 1 (length ps)) (format nil "(-~A)" (car ps))
           (format nil "(~A~{ - ~A~})" (car ps) (cdr ps)))))
    ((and (listp expr) (eq (car expr) 'IN-FLOW))
     (format nil "in_flow(&~A)" (ca (cadr expr) env)))
    ((and (listp expr) (eq (car expr) 'SEQ-SPACES))
     (format nil "seq_spaces(~A, &~A)" (ca (cadr expr) env) (ca (caddr expr) env)))
    (t (format nil "/* arg?~S */" expr))))

;;; ═══════════════════════════════════════════════════════════════════
;;; EMITTER
;;; ═══════════════════════════════════════════════════════════════════

(defun grammar-rules-ordered (gram)
  (let ((rules nil))
    (maphash (lambda (k v) (declare (ignore k)) (push v rules)) (gram-rules gram))
    (sort rules #'< :key #'rdef-num)))

(defun emit-yaml-reader-rs (gram path)
  (let ((*gram* gram))
    (with-open-file (*out* path :direction :output :if-exists :supersede :external-format :utf-8)
      (emit-rs-all))))

(defun emit-rs-all ()
  (emit "// ════════════════════════════════════════════════════════════════")
  (emit "// yaml_reader.rs — YAML 1.2 parser, projected from yaml-grammar.scm")
  (emit "// ════════════════════════════════════════════════════════════════")
  (emit "// Generated by emit-yaml-rust.lisp. DO NOT EDIT.")
  (emit "// ════════════════════════════════════════════════════════════════")
  (blank)
  (emit "#![allow(non_snake_case, unused_parens, unused_variables, dead_code, unused_mut)]")
  (emit "#![allow(clippy::all)]")
  (blank)
  (emit "use std::env;")
  (emit "use std::fs;")
  (emit "use std::io::{self, Read};")
  (blank)

  ;; ── Input ──
  (emit "#[derive(Clone, Debug)]")
  (emit "struct Input { src: *const String, pos: usize, line: usize, col: usize }")
  (emit "unsafe impl Send for Input {}")
  (emit "unsafe impl Sync for Input {}")
  (blank)
  (emit "impl Input {")
  (emit "    fn new(src: &String) -> Self { Input { src: src as *const String, pos: 0, line: 1, col: 0 } }")
  (emit "    fn s(&self) -> &str { unsafe { &*self.src } }")
  (emit "    fn at_eof(&self) -> bool { self.pos >= self.s().len() }")
  (emit "    fn peek(&self) -> i32 { if self.at_eof() { -1 } else { self.s().as_bytes()[self.pos] as i32 } }")
  (emit "    fn adv(&self) -> Input {")
  (emit "        if self.at_eof() { return self.clone(); }")
  (emit "        let c = self.s().as_bytes()[self.pos];")
  (emit "        Input { src: self.src, pos: self.pos+1,")
  (emit "            line: if c==b'\\n' { self.line+1 } else { self.line },")
  (emit "            col: if c==b'\\n' { 0 } else { self.col+1 } }")
  (emit "    }")
  (emit "}")
  (blank)

  ;; ── AST ──
  (emit "#[derive(Clone, Debug)]")
  (emit "enum Ast { Branch(String, Vec<Ast>), Leaf(String) }")
  (blank)

  ;; ── Result ──
  (emit "#[derive(Clone, Debug)]")
  (emit "struct Result { fail: bool, val: String, rest: Input, tag: String, tag_int: i32, ast: Option<Box<Ast>>, ast_list: Vec<Ast>, err: String }")
  (blank)
  (emit "fn ok(inp: Input) -> Result { Result { fail:false, val:String::new(), rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:String::new() } }")
  (emit "fn ok_v(inp: Input, v: String) -> Result { Result { fail:false, val:v, rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:String::new() } }")
  (emit "fn fail(inp: Input, m: &str) -> Result { Result { fail:true, val:String::new(), rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:m.to_string() } }")
  (blank)

  ;; ── Context ──
  (emit "fn in_flow(c: &str) -> String { if c==\"FLOW-OUT\"||c==\"FLOW-IN\" { \"FLOW-IN\".into() } else { \"FLOW-KEY\".into() } }")
  (emit "fn seq_spaces(n: i32, c: &str) -> i32 { if c==\"BLOCK-OUT\" { n-1 } else { n } }")
  (blank)

  ;; ── Combinators ──
  (emit "fn match_cp(inp: Input, cp: i32) -> Result {")
  (emit "    let c=inp.peek(); if c==cp { ok_v(inp.adv(), String::from(c as u8 as char)) } else { fail(inp,\"cp\") } }")
  (emit "fn match_range(inp: Input, lo: i32, hi: i32) -> Result {")
  (emit "    let c=inp.peek(); if c>=lo&&c<=hi { ok_v(inp.adv(), String::from(c as u8 as char)) } else { fail(inp,\"rng\") } }")
  (emit "fn match_str(inp: Input, t: &str) -> Result {")
  (emit "    let n=t.len(); if inp.pos+n>inp.s().len() { return fail(inp,\"str\"); }")
  (emit "    if &inp.s()[inp.pos..inp.pos+n]!=t { return fail(inp,\"str\"); }")
  (emit "    let mut c=inp.clone(); for _ in 0..n { c=c.adv(); } ok_v(c, t.to_string()) }")
  (blank)

  (emit "fn seq(inp: Input, fns: Vec<Box<dyn Fn(Input)->Result>>) -> Result {")
  (emit "    let mut cur=inp; let mut acc=String::new(); let mut asts: Vec<Ast>=vec![];")
  (emit "    for f in &fns { let r=f(cur); if r.fail { return r; } acc.push_str(&r.val);")
  (emit "        if let Some(a)=r.ast { asts.push(*a); }")
  (emit "        else { asts.extend(r.ast_list); }")
  (emit "        cur=r.rest; }")
  (emit "    let mut res=ok_v(cur,acc);")
  (emit "    if asts.len()==1 { res.ast=Some(Box::new(asts.remove(0))); }")
  (emit "    else if asts.len()>1 { res.ast_list=asts; }")
  (emit "    res }")
  (blank)
  (emit "fn alt(inp: Input, fns: Vec<Box<dyn Fn(Input)->Result>>) -> Result {")
  (emit "    for f in &fns { let r=f(inp.clone()); if !r.fail { return r; } }")
  (emit "    fail(inp,\"alt\") }")
  (blank)

  (emit "fn star(inp: Input, f: &dyn Fn(Input)->Result) -> Result {")
  (emit "    let mut cur=inp; let mut acc=String::new(); let mut asts: Vec<Ast>=vec![];")
  (emit "    loop { let r=f(cur.clone()); if r.fail||r.rest.pos<=cur.pos { break; } acc.push_str(&r.val);")
  (emit "        if let Some(a)=r.ast { asts.push(*a); } else { asts.extend(r.ast_list); }")
  (emit "        cur=r.rest; }")
  (emit "    let mut res=ok_v(cur,acc); if !asts.is_empty() { res.ast_list=asts; } res }")
  (blank)

  (emit "fn plus_(inp: Input, f: &dyn Fn(Input)->Result) -> Result {")
  (emit "    let first=f(inp); if first.fail { return first; }")
  (emit "    let rest=star(first.rest.clone(),f);")
  (emit "    let mut res=ok_v(rest.rest, format!(\"{}{}\",first.val,rest.val));")
  (emit "    let mut asts: Vec<Ast>=vec![];")
  (emit "    if let Some(a)=first.ast { asts.push(*a); } else { asts.extend(first.ast_list); }")
  (emit "    asts.extend(rest.ast_list);")
  (emit "    if !asts.is_empty() { res.ast_list=asts; } res }")
  (blank)

  (emit "fn opt(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { ok(inp) } else { r } }")
  (emit "fn neg(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { ok(inp) } else { fail(inp,\"neg\") } }")
  (emit "fn minus(inp: Input, fa: &dyn Fn(Input)->Result, fb: &dyn Fn(Input)->Result) -> Result {")
  (emit "    let ra=fa(inp.clone()); if ra.fail { return ra; }")
  (emit "    let rb=fb(inp.clone()); if !rb.fail&&rb.rest.pos==ra.rest.pos { fail(inp,\"excl\") } else { ra } }")
  (emit "fn rep(inp: Input, n: i32, f: &dyn Fn(Input)->Result) -> Result {")
  (emit "    let mut cur=inp; let mut acc=String::new();")
  (emit "    for _ in 0..n { let r=f(cur); if r.fail { return r; } acc.push_str(&r.val); cur=r.rest; }")
  (emit "    ok_v(cur,acc) }")
  (emit "fn ahead(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { r } else { ok(inp) } }")
  (emit "fn behind(inp: Input, f: &dyn Fn(Input)->Result) -> Result {")
  (emit "    if inp.pos==0 { return fail(inp,\"bh\"); }")
  (emit "    let t=Input{src:inp.src,pos:inp.pos-1,line:inp.line,col:if inp.col>0{inp.col-1}else{0}};")
  (emit "    let r=f(t); if r.fail { fail(inp,\"bh\") } else { ok(inp) } }")
  (emit "fn sol(inp: Input) -> Result { if inp.col==0 { ok(inp) } else { fail(inp,\"sol\") } }")
  (emit "fn eof_ok(inp: Input) -> Result { if inp.at_eof() { ok(inp) } else { fail(inp,\"eof\") } }")
  (blank)

  ;; ── YAML extensions ──
  (emit "fn build(inp: Input, typ: &str, f: &dyn Fn(Input)->Result) -> Result {")
  (emit "    let mut r=f(inp); if r.fail { return r; }")
  (emit "    let mut kids=vec![]; if let Some(a)=r.ast.take() { kids.push(*a); } else { kids.extend(r.ast_list.drain(..)); }")
  (emit "    r.ast=Some(Box::new(Ast::Branch(typ.to_string(),kids))); r }")
  (blank)
  (emit "fn scalar(inp: Input, f: &dyn Fn(Input)->Result) -> Result {")
  (emit "    let mut r=f(inp); if r.fail { return r; } r.ast=Some(Box::new(Ast::Leaf(r.val.clone()))); r }")
  (blank)
  (emit "fn collect(inp: Input, f: &dyn Fn(Input)->Result) -> Result { f(inp) }")
  (blank)
  (emit "fn detect_indent(inp: Input, n: i32) -> Result {")
  (emit "    let s=inp.s().as_bytes(); let len=s.len(); let i=inp.pos;")
  (emit "    let mut sp=0; while i+sp<len&&s[i+sp]==b' ' { sp+=1; }")
  (emit "    if i+sp<len&&s[i+sp]!=b'\\n' { let mut r=ok(inp); r.tag_int=std::cmp::max(1,sp as i32-n); return r; }")
  (emit "    let mut j=i; while j<len&&s[j]!=b'\\n' { j+=1; }")
  (emit "    while j<len { if s[j]==b'\\n' { j+=1; } if j>=len { break; }")
  (emit "        sp=0; while j+sp<len&&s[j+sp]==b' ' { sp+=1; }")
  (emit "        let nx=j+sp; if nx>=len||s[nx]==b'\\n' { j=nx; continue; }")
  (emit "        let mut r=ok(inp); r.tag_int=std::cmp::max(1,sp as i32-n); return r; }")
  (emit "    let mut r=ok(inp); r.tag_int=1; r }")
  (blank)
  (emit "fn parse_int(inp: Input, f: &dyn Fn(Input)->Result) -> Result {")
  (emit "    let mut r=f(inp); if r.fail { return r; }")
  (emit "    r.tag_int=r.val.chars().filter(|c|c.is_ascii_digit()).fold(0i32,|a,c|a*10+(c as i32-'0' as i32)); r }")
  (emit "fn parse_sym(inp: Input, f: &dyn Fn(Input)->Result, sym: &str) -> Result {")
  (emit "    let mut r=f(inp); if r.fail { return r; } r.tag=sym.to_string(); r }")
  (emit "fn val(inp: Input, v: &str) -> Result { let mut r=ok(inp); r.tag=v.to_string(); r }")
  (blank)
  (blank)

  ;; ── Rules ──
  (emit "// ════════════════════════════════════════════════════════════════")
  (emit "// YAML 1.2 Grammar — 211 rules")
  (emit "// ════════════════════════════════════════════════════════════════")
  (blank)
  (dolist (rd (grammar-rules-ordered *gram*))
    (unless (member (rdef-name rd) '(IN-FLOW SEQ-SPACES))
      (let* ((nm (rs-ident (rdef-name rd)))
             (ps (rdef-params rd))
             (sig (if ps (format nil "inp: Input~{, ~A~}"
                                 (mapcar (lambda (p)
                                           (let ((pn (symbol-name p)))
                                             (if (member pn '("N" "M") :test #'string-equal)
                                                 (format nil "~A: i32" (rs-ident p))
                                                 (format nil "~A: String" (rs-ident p)))))
                                         ps))
                      "inp: Input"))
             (body-str (ce (rdef-body rd) ps)))
        (emitf "// [~D] ~A~%" (rdef-num rd) (rdef-name rd))
        (emitf "fn ~A(~A) -> Result {~%" nm sig)
        (emitf "    ~A~%" body-str)
        (emit "}")
        (blank))))

  ;; ── API + main ──
  (emit "fn print_ast(node: &Ast, depth: usize) {")
  (emit "    let indent = \"  \".repeat(depth);")
  (emit "    match node {")
  (emit "        Ast::Leaf(text) => println!(\"{indent}SCALAR: \\\"{text}\\\"\"),")
  (emit "        Ast::Branch(tag, children) => {")
  (emit "            println!(\"{indent}{tag}\");")
  (emit "            for c in children { print_ast(c, depth+1); }")
  (emit "        }")
  (emit "    }")
  (emit "}")
  (blank)
  (emit "fn main() {")
  (emit "    let args: Vec<String> = env::args().collect();")
  (emit "    let text = if args.len()>1 { fs::read_to_string(&args[1]).expect(\"Cannot open\") }")
  (emit "    else { let mut s=String::new(); io::stdin().read_to_string(&mut s).unwrap(); s };")
  (emit "    let inp = Input::new(&text);")
  (emit "    let r = l_yaml_stream(inp);")
  (emit "    if !r.fail { println!(\"OK: {} chars\", r.rest.pos); if let Some(a)=&r.ast { print_ast(a,0); } }")
  (emit "    else { eprintln!(\"FAIL @{}: {}\", r.rest.pos, r.err); std::process::exit(1); }")
  (emit "}"))

;;; ═══════════════════════════════════════════════════════════════════
;;; ENTRY POINT
;;; ═══════════════════════════════════════════════════════════════════

(defun project-yaml-to-rust (&optional (grammar-path "yaml-grammar.scm")
                                        (output-path "yaml_reader.rs"))
  (format t "~&; Loading grammar: ~A~%" grammar-path)
  (let ((*gram* (load-yaml-grammar grammar-path)))
    (format t "; Projecting ~D rules to ~A~%" (hash-table-count (gram-rules *gram*)) output-path)
    (emit-yaml-reader-rs *gram* output-path)
    (format t "; Done. ~A written.~%" output-path)))

(project-yaml-to-rust "yaml-grammar.scm" "yaml_reader.rs")
