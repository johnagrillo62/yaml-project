;;;; peg-rust.lisp — Rust target for emit-yaml-peg.lisp
;;;; Load before emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Rust")
(def-tgt "default-output" "yaml_reader.rs")

(def-tgt "keywords"
  '("as" "break" "const" "continue" "crate" "else" "enum"
    "extern" "false" "fn" "for" "if" "impl" "in" "let"
    "loop" "match" "mod" "move" "mut" "pub" "ref" "return"
    "self" "static" "struct" "super" "trait" "true" "type"
    "unsafe" "use" "where" "while" "yield" "box" "do"))
(def-tgt "keyword-prefix" "r#")

;;; ── Helpers ──

(defun rs-str-params (env)
  (remove-if (lambda (p) (member (symbol-name p) '("N" "M") :test #'string-equal)) env))

;;; ── Closure wrapping ──

;; Rust ref closures: |inp: Input| -> Result { body }
(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "&|inp: Input| -> Result { ~A }" body)))

;; Rust boxed closures: clone String params, move into Box
(def-tgt "box-wrap"
  (lambda (body env)
    (let ((sp (rs-str-params env)))
      (if sp
          (format nil "{ ~{let ~A = ~A.clone(); ~}Box::new(move |inp: Input| -> Result { ~{let ~A = ~A.clone(); ~}~A }) }"
                  (loop for p in sp
                        collect (format nil "~A_c" (peg-ident p))
                        collect (peg-ident p))
                  (loop for p in sp
                        collect (peg-ident p)
                        collect (format nil "~A_c" (peg-ident p)))
                  body)
          (format nil "Box::new(move |inp: Input| -> Result { ~A })" body)))))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "seq(inp, vec![~{~A~^, ~}])" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "alt(inp, vec![~{~A~^, ~}])" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "(|| -> Result {~{ if ~A == ~S { return ~A; }~} fail(inp.clone(), \"no case\") })()"
            (loop for (val body) in cases
                  collect param collect val collect body))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "(|| -> Result { let r = ~A; if r.fail { return r; } let ~A: i32 = r.tag_int; let inp = r.rest; ~A })()"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "(|| -> Result { let r = ~A; if r.fail { return r; } let ~A: String = r.tag.clone(); let inp = r.rest; ~A })()"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (let ((pn (symbol-name sym)))
      (if (member pn '("N" "M") :test #'string-equal)
          (peg-ident sym)
          (format nil "~A.clone()" (peg-ident sym))))))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S.to_string()" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "~A as i32" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "in_flow(&~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seq_spaces(~A, &~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(inp: Input~{, ~A~})" name
                (mapcar (lambda (p)
                          (let ((pn (symbol-name p)))
                            (if (member pn '("N" "M") :test #'string-equal)
                                (format nil "~A: i32" (peg-ident p))
                                (format nil "~A: String" (peg-ident p)))))
                        params))
        (format nil "~A(inp: Input)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "fn ~A -> Result {~%    ~A~%}" sig body)))

(def-tgt "fwd-decl" nil)  ;; Rust doesn't need forward declarations

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
// yaml_reader.rs — YAML 1.2 parser, projected from yaml-grammar.scm
// ════════════════════════════════════════════════════════════════
// Generated. DO NOT EDIT — regenerate from the grammar.
// ════════════════════════════════════════════════════════════════

#![allow(non_snake_case, unused_parens, unused_variables, dead_code, unused_mut)]
#![allow(clippy::all)]

use std::env;
use std::fs;
use std::io::{self, Read};")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Input ──

#[derive(Clone, Debug)]
struct Input { src: *const String, pos: usize, line: usize, col: usize }
unsafe impl Send for Input {}
unsafe impl Sync for Input {}

impl Input {
    fn new(src: &String) -> Self { Input { src: src as *const String, pos: 0, line: 1, col: 0 } }
    fn s(&self) -> &str { unsafe { &*self.src } }
    fn at_eof(&self) -> bool { self.pos >= self.s().len() }
    fn peek(&self) -> i32 {
        if self.at_eof() { return -1; }
        let b = self.s().as_bytes();
        let b0 = b[self.pos] as u32;
        if b0 < 0x80 { return b0 as i32; }
        let (cp, _) = Self::decode_utf8(b, self.pos);
        cp as i32
    }
    fn decode_utf8(b: &[u8], pos: usize) -> (u32, usize) {
        let b0 = b[pos] as u32;
        if b0 < 0x80 { return (b0, 1); }
        if b0 < 0xC0 { return (b0, 1); } // invalid, advance 1
        if pos+1 >= b.len() { return (b0, 1); }
        let b1 = (b[pos+1] & 0x3F) as u32;
        if b0 < 0xE0 { return ((b0 & 0x1F) << 6 | b1, 2); }
        if pos+2 >= b.len() { return (b0, 1); }
        let b2 = (b[pos+2] & 0x3F) as u32;
        if b0 < 0xF0 { return ((b0 & 0x0F) << 12 | b1 << 6 | b2, 3); }
        if pos+3 >= b.len() { return (b0, 1); }
        let b3 = (b[pos+3] & 0x3F) as u32;
        ((b0 & 0x07) << 18 | b1 << 12 | b2 << 6 | b3, 4)
    }
    fn adv(&self) -> Input {
        if self.at_eof() { return self.clone(); }
        let c = self.s().as_bytes()[self.pos];
        Input { src: self.src, pos: self.pos+1,
            line: if c==b'\\n' { self.line+1 } else { self.line },
            col: if c==b'\\n' { 0 } else { self.col+1 } }
    }
}"

"// ── AST ──

#[derive(Clone, Debug)]
enum Ast { Branch(String, Vec<Ast>), Leaf(String) }"

"// ── Result ──

#[derive(Clone, Debug)]
struct Result { fail: bool, val: String, rest: Input, tag: String, tag_int: i32, ast: Option<Box<Ast>>, ast_list: Vec<Ast>, err: String }

fn ok(inp: Input) -> Result { Result { fail:false, val:String::new(), rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:String::new() } }
fn ok_v(inp: Input, v: String) -> Result { Result { fail:false, val:v, rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:String::new() } }
fn fail(inp: Input, m: &str) -> Result { Result { fail:true, val:String::new(), rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:m.to_string() } }"

"// ── Context ──

fn in_flow(c: &str) -> String { if c==\"FLOW-OUT\"||c==\"FLOW-IN\" { \"FLOW-IN\".into() } else { \"FLOW-KEY\".into() } }
fn seq_spaces(n: i32, c: &str) -> i32 { if c==\"BLOCK-OUT\" { n-1 } else { n } }"

"// ── Combinators ──

fn match_cp(inp: Input, cp: i32) -> Result {
    let c=inp.peek(); if c==cp { let ch = char::from_u32(c as u32).unwrap_or('?');
    let mut buf=[0u8;4]; let s=ch.encode_utf8(&mut buf);
    let mut cur=inp.clone(); for _ in 0..s.len() { cur=cur.adv(); }
    ok_v(cur, s.to_string()) } else { fail(inp,\"cp\") } }
fn match_range(inp: Input, lo: i32, hi: i32) -> Result {
    let c=inp.peek(); if c>=lo&&c<=hi { let ch = char::from_u32(c as u32).unwrap_or('?');
    let mut buf=[0u8;4]; let s=ch.encode_utf8(&mut buf);
    let mut cur=inp.clone(); for _ in 0..s.len() { cur=cur.adv(); }
    ok_v(cur, s.to_string()) } else { fail(inp,\"rng\") } }
fn match_str(inp: Input, t: &str) -> Result {
    let n=t.len(); let s=inp.s().as_bytes(); if inp.pos+n>s.len() { return fail(inp,\"str\"); }
    if &s[inp.pos..inp.pos+n]!=t.as_bytes() { return fail(inp,\"str\"); }
    let mut c=inp.clone(); for _ in 0..n { c=c.adv(); } ok_v(c, t.to_string()) }

fn seq(inp: Input, fns: Vec<Box<dyn Fn(Input)->Result>>) -> Result {
    let mut cur=inp; let mut acc=String::new(); let mut asts: Vec<Ast>=vec![];
    for f in &fns { let r=f(cur); if r.fail { return r; } acc.push_str(&r.val);
        if let Some(a)=r.ast { asts.push(*a); } else { asts.extend(r.ast_list); }
        cur=r.rest; }
    let mut res=ok_v(cur,acc);
    if asts.len()==1 { res.ast=Some(Box::new(asts.remove(0))); }
    else if asts.len()>1 { res.ast_list=asts; }
    res }

fn alt(inp: Input, fns: Vec<Box<dyn Fn(Input)->Result>>) -> Result {
    for f in &fns { let r=f(inp.clone()); if !r.fail { return r; } }
    fail(inp,\"alt\") }

fn star(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    let mut cur=inp; let mut acc=String::new(); let mut asts: Vec<Ast>=vec![];
    loop { let r=f(cur.clone()); if r.fail||r.rest.pos<=cur.pos { break; } acc.push_str(&r.val);
        if let Some(a)=r.ast { asts.push(*a); } else { asts.extend(r.ast_list); }
        cur=r.rest; }
    let mut res=ok_v(cur,acc); if !asts.is_empty() { res.ast_list=asts; } res }

fn plus_(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    let first=f(inp); if first.fail { return first; }
    let rest=star(first.rest.clone(),f);
    let mut res=ok_v(rest.rest, format!(\"{}{}\",first.val,rest.val));
    let mut asts: Vec<Ast>=vec![];
    if let Some(a)=first.ast { asts.push(*a); } else { asts.extend(first.ast_list); }
    asts.extend(rest.ast_list);
    if !asts.is_empty() { res.ast_list=asts; } res }

fn opt(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { ok(inp) } else { r } }
fn neg(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { ok(inp) } else { fail(inp,\"neg\") } }
fn minus(inp: Input, fa: &dyn Fn(Input)->Result, fb: &dyn Fn(Input)->Result) -> Result {
    let ra=fa(inp.clone()); if ra.fail { return ra; }
    let rb=fb(inp.clone()); if !rb.fail&&rb.rest.pos==ra.rest.pos { fail(inp,\"excl\") } else { ra } }
fn rep(inp: Input, n: i32, f: &dyn Fn(Input)->Result) -> Result {
    let mut cur=inp; let mut acc=String::new();
    for _ in 0..n { let r=f(cur); if r.fail { return r; } acc.push_str(&r.val); cur=r.rest; }
    ok_v(cur,acc) }
fn ahead(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { r } else { ok(inp) } }
fn behind(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    if inp.pos==0 { return fail(inp,\"bh\"); }
    let t=Input{src:inp.src,pos:inp.pos-1,line:inp.line,col:if inp.col>0{inp.col-1}else{0}};
    let r=f(t); if r.fail { fail(inp,\"bh\") } else { ok(inp) } }
fn sol(inp: Input) -> Result { if inp.col==0 { ok(inp) } else { fail(inp,\"sol\") } }
fn eof_ok(inp: Input) -> Result { if inp.at_eof() { ok(inp) } else { fail(inp,\"eof\") } }"

"// ── YAML extensions ──

fn build(inp: Input, typ: &str, f: &dyn Fn(Input)->Result) -> Result {
    let mut r=f(inp); if r.fail { return r; }
    let mut kids=vec![]; if let Some(a)=r.ast.take() { kids.push(*a); } else { kids.extend(r.ast_list.drain(..)); }
    r.ast=Some(Box::new(Ast::Branch(typ.to_string(),kids))); r }

fn scalar(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    let mut r=f(inp); if r.fail { return r; } r.ast=Some(Box::new(Ast::Leaf(r.val.clone()))); r }

fn collect(inp: Input, f: &dyn Fn(Input)->Result) -> Result { f(inp) }

fn detect_indent(inp: Input, n: i32) -> Result {
    let s=inp.s().as_bytes(); let len=s.len(); let i=inp.pos;
    let mut sp=0; while i+sp<len&&s[i+sp]==b' ' { sp+=1; }
    if i+sp<len&&s[i+sp]!=b'\\n' { let mut r=ok(inp); r.tag_int=std::cmp::max(1,sp as i32-n); return r; }
    let mut j=i; while j<len&&s[j]!=b'\\n' { j+=1; }
    while j<len { if s[j]==b'\\n' { j+=1; } if j>=len { break; }
        sp=0; while j+sp<len&&s[j+sp]==b' ' { sp+=1; }
        let nx=j+sp; if nx>=len||s[nx]==b'\\n' { j=nx; continue; }
        let mut r=ok(inp); r.tag_int=std::cmp::max(1,sp as i32-n); return r; }
    let mut r=ok(inp); r.tag_int=1; r }

fn parse_int(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    let mut r=f(inp); if r.fail { return r; }
    r.tag_int=r.val.chars().filter(|c|c.is_ascii_digit()).fold(0i32,|a,c|a*10+(c as i32-'0' as i32)); r }
fn parse_sym(inp: Input, f: &dyn Fn(Input)->Result, sym: &str) -> Result {
    let mut r=f(inp); if r.fail { return r; } r.tag=sym.to_string(); r }
fn val(inp: Input, v: &str) -> Result { let mut r=ok(inp); r.tag=v.to_string(); r }"
))

;;; ── API ──

(def-tgt "api"
"// ── API ──

fn print_ast(node: &Ast, depth: usize) {
    let indent = \"  \".repeat(depth);
    match node {
        Ast::Leaf(text) => println!(\"{indent}SCALAR: \\\"{text}\\\"\"),
        Ast::Branch(tag, children) => {
            println!(\"{indent}{tag}\");
            for c in children { print_ast(c, depth+1); }
        }
    }
}")

;;; ── Main ──

(def-tgt "main-fn"
"fn main() {
    let args: Vec<String> = env::args().collect();
    let text = if args.len()>1 { fs::read_to_string(&args[1]).expect(\"Cannot open\") }
    else { let mut s=String::new(); io::stdin().read_to_string(&mut s).unwrap(); s };
    let inp = Input::new(&text);
    let r = l_yaml_stream(inp);
    if !r.fail { println!(\"OK: {} chars\", r.rest.pos); if let Some(a)=&r.ast { print_ast(a,0); } }
    else { eprintln!(\"FAIL @{}: {}\", r.rest.pos, r.err); std::process::exit(1); }
}")

;;; ── Concerns (native API layer) ──

(load "emit/yaml-concerns.lisp")
(def-tgt "yaml-concerns" *yaml-concerns-rust*)
