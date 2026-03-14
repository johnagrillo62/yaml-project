// ════════════════════════════════════════════════════════════════
#![allow(non_snake_case, unused_parens, unused_variables, dead_code, unused_mut)]
#![allow(clippy::all)]

use std::env;
use std::fs;
use std::io::{self, Read};

// ── Input ──

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
            line: if c==b'\n' { self.line+1 } else { self.line },
            col: if c==b'\n' { 0 } else { self.col+1 } }
    }
}

// ── AST ──

#[derive(Clone, Debug)]
enum Ast { Branch(String, Vec<Ast>), Leaf(String) }

// ── Result ──

#[derive(Clone, Debug)]
struct Result { fail: bool, val: String, rest: Input, tag: String, tag_int: i32, ast: Option<Box<Ast>>, ast_list: Vec<Ast>, err: String }

fn ok(inp: Input) -> Result { Result { fail:false, val:String::new(), rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:String::new() } }
fn ok_v(inp: Input, v: String) -> Result { Result { fail:false, val:v, rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:String::new() } }
fn fail(inp: Input, m: &str) -> Result { Result { fail:true, val:String::new(), rest:inp, tag:String::new(), tag_int:0, ast:None, ast_list:vec![], err:m.to_string() } }

// ── Context ──

fn in_flow(c: &str) -> String { if c=="FLOW-OUT"||c=="FLOW-IN" { "FLOW-IN".into() } else { "FLOW-KEY".into() } }
fn seq_spaces(n: i32, c: &str) -> i32 { if c=="BLOCK-OUT" { n-1 } else { n } }

// ── Combinators ──

fn match_cp(inp: Input, cp: i32) -> Result {
    let c=inp.peek(); if c==cp { let ch = char::from_u32(c as u32).unwrap_or('?');
    let mut buf=[0u8;4]; let s=ch.encode_utf8(&mut buf);
    let mut cur=inp.clone(); for _ in 0..s.len() { cur=cur.adv(); }
    ok_v(cur, s.to_string()) } else { fail(inp,"cp") } }
fn match_range(inp: Input, lo: i32, hi: i32) -> Result {
    let c=inp.peek(); if c>=lo&&c<=hi { let ch = char::from_u32(c as u32).unwrap_or('?');
    let mut buf=[0u8;4]; let s=ch.encode_utf8(&mut buf);
    let mut cur=inp.clone(); for _ in 0..s.len() { cur=cur.adv(); }
    ok_v(cur, s.to_string()) } else { fail(inp,"rng") } }
fn match_str(inp: Input, t: &str) -> Result {
    let n=t.len(); let s=inp.s().as_bytes(); if inp.pos+n>s.len() { return fail(inp,"str"); }
    if &s[inp.pos..inp.pos+n]!=t.as_bytes() { return fail(inp,"str"); }
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
    fail(inp,"alt") }

fn star(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    let mut cur=inp; let mut acc=String::new(); let mut asts: Vec<Ast>=vec![];
    loop { let r=f(cur.clone()); if r.fail||r.rest.pos<=cur.pos { break; } acc.push_str(&r.val);
        if let Some(a)=r.ast { asts.push(*a); } else { asts.extend(r.ast_list); }
        cur=r.rest; }
    let mut res=ok_v(cur,acc); if !asts.is_empty() { res.ast_list=asts; } res }

fn plus_(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    let first=f(inp); if first.fail { return first; }
    let rest=star(first.rest.clone(),f);
    let mut res=ok_v(rest.rest, format!("{}{}",first.val,rest.val));
    let mut asts: Vec<Ast>=vec![];
    if let Some(a)=first.ast { asts.push(*a); } else { asts.extend(first.ast_list); }
    asts.extend(rest.ast_list);
    if !asts.is_empty() { res.ast_list=asts; } res }

fn opt(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { ok(inp) } else { r } }
fn neg(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { ok(inp) } else { fail(inp,"neg") } }
fn minus(inp: Input, fa: &dyn Fn(Input)->Result, fb: &dyn Fn(Input)->Result) -> Result {
    let ra=fa(inp.clone()); if ra.fail { return ra; }
    let rb=fb(inp.clone()); if !rb.fail&&rb.rest.pos==ra.rest.pos { fail(inp,"excl") } else { ra } }
fn rep(inp: Input, n: i32, f: &dyn Fn(Input)->Result) -> Result {
    let mut cur=inp; let mut acc=String::new();
    for _ in 0..n { let r=f(cur); if r.fail { return r; } acc.push_str(&r.val); cur=r.rest; }
    ok_v(cur,acc) }
fn ahead(inp: Input, f: &dyn Fn(Input)->Result) -> Result { let r=f(inp.clone()); if r.fail { r } else { ok(inp) } }
fn behind(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    if inp.pos==0 { return fail(inp,"bh"); }
    let t=Input{src:inp.src,pos:inp.pos-1,line:inp.line,col:if inp.col>0{inp.col-1}else{0}};
    let r=f(t); if r.fail { fail(inp,"bh") } else { ok(inp) } }
fn sol(inp: Input) -> Result { if inp.col==0 { ok(inp) } else { fail(inp,"sol") } }
fn eof_ok(inp: Input) -> Result { if inp.at_eof() { ok(inp) } else { fail(inp,"eof") } }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
fn json_text(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { ws(inp) }),
        Box::new(move |inp: Input| -> Result { value(inp) }),
        Box::new(move |inp: Input| -> Result { ws(inp) }),
        Box::new(move |inp: Input| -> Result { eof_ok(inp) })])
}

// [2] VALUE 
fn value(inp: Input) -> Result {
    alt(inp, vec![
        Box::new(move |inp: Input| -> Result { object(inp) }),
        Box::new(move |inp: Input| -> Result { array(inp) }),
        Box::new(move |inp: Input| -> Result { r#string(inp) }),
        Box::new(move |inp: Input| -> Result { number(inp) }),
        Box::new(move |inp: Input| -> Result { match_str(inp, "true") }),
        Box::new(move |inp: Input| -> Result { match_str(inp, "false") }),
        Box::new(move |inp: Input| -> Result { match_str(inp, "null") })])
}

// [3] OBJECT 
fn object(inp: Input) -> Result {
    alt(inp, vec![
        Box::new(move |inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { match_cp(inp, 123) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { members(inp) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 125) })]) }),
        Box::new(move |inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { match_cp(inp, 123) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 125) })]) })])
}

// [4] MEMBERS 
fn members(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { member(inp) }),
        Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 44) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { member(inp) })]) }) })])
}

// [5] MEMBER 
fn member(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { ws(inp) }),
        Box::new(move |inp: Input| -> Result { r#string(inp) }),
        Box::new(move |inp: Input| -> Result { ws(inp) }),
        Box::new(move |inp: Input| -> Result { match_cp(inp, 58) }),
        Box::new(move |inp: Input| -> Result { ws(inp) }),
        Box::new(move |inp: Input| -> Result { value(inp) }),
        Box::new(move |inp: Input| -> Result { ws(inp) })])
}

// [6] ARRAY 
fn array(inp: Input) -> Result {
    alt(inp, vec![
        Box::new(move |inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { match_cp(inp, 91) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { elements(inp) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 93) })]) }),
        Box::new(move |inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { match_cp(inp, 91) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 93) })]) })])
}

// [7] ELEMENTS 
fn elements(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { value(inp) }),
        Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 44) }),
            Box::new(move |inp: Input| -> Result { ws(inp) }),
            Box::new(move |inp: Input| -> Result { value(inp) })]) }) })])
}

// [8] STRING 
fn r#string(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { match_cp(inp, 34) }),
        Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { r#char(inp) }) }),
        Box::new(move |inp: Input| -> Result { match_cp(inp, 34) })])
}

// [9] CHAR 
fn r#char(inp: Input) -> Result {
    alt(inp, vec![
        Box::new(move |inp: Input| -> Result { escaped(inp) }),
        Box::new(move |inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { neg(inp, &|inp: Input| -> Result { match_cp(inp, 34) }) }),
            Box::new(move |inp: Input| -> Result { neg(inp, &|inp: Input| -> Result { match_cp(inp, 92) }) }),
            Box::new(move |inp: Input| -> Result { neg(inp, &|inp: Input| -> Result { match_cp(inp, 0x0) }) }),
            Box::new(move |inp: Input| -> Result { neg(inp, &|inp: Input| -> Result { match_range(inp, 0x0, 0x1F) }) }),
            Box::new(move |inp: Input| -> Result { match_range(inp, 0x20, 0x10FFFF) })]) })])
}

// [10] ESCAPED 
fn escaped(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { match_cp(inp, 92) }),
        Box::new(move |inp: Input| -> Result { alt(inp, vec![
            Box::new(move |inp: Input| -> Result { match_cp(inp, 34) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 92) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 47) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 98) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 102) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 110) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 114) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 116) }),
            Box::new(move |inp: Input| -> Result { seq(inp, vec![
                Box::new(move |inp: Input| -> Result { match_cp(inp, 117) }),
                Box::new(move |inp: Input| -> Result { hex4(inp) })]) })]) })])
}

// [11] HEX4 
fn hex4(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { hexdig(inp) }),
        Box::new(move |inp: Input| -> Result { hexdig(inp) }),
        Box::new(move |inp: Input| -> Result { hexdig(inp) }),
        Box::new(move |inp: Input| -> Result { hexdig(inp) })])
}

// [12] HEXDIG 
fn hexdig(inp: Input) -> Result {
    alt(inp, vec![
        Box::new(move |inp: Input| -> Result { match_range(inp, 48, 57) }),
        Box::new(move |inp: Input| -> Result { match_range(inp, 97, 102) }),
        Box::new(move |inp: Input| -> Result { match_range(inp, 65, 70) })])
}

// [13] NUMBER 
fn number(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { match_cp(inp, 45) }) }),
        Box::new(move |inp: Input| -> Result { integer(inp) }),
        Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { fraction(inp) }) }),
        Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { exponent(inp) }) })])
}

// [14] INTEGER 
fn integer(inp: Input) -> Result {
    alt(inp, vec![
        Box::new(move |inp: Input| -> Result { match_cp(inp, 48) }),
        Box::new(move |inp: Input| -> Result { seq(inp, vec![
            Box::new(move |inp: Input| -> Result { match_range(inp, 49, 57) }),
            Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { match_range(inp, 48, 57) }) })]) })])
}

// [15] FRACTION 
fn fraction(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { match_cp(inp, 46) }),
        Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { match_range(inp, 48, 57) }) })])
}

// [16] EXPONENT 
fn exponent(inp: Input) -> Result {
    seq(inp, vec![
        Box::new(move |inp: Input| -> Result { alt(inp, vec![
            Box::new(move |inp: Input| -> Result { match_cp(inp, 101) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 69) })]) }),
        Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { alt(inp, vec![
            Box::new(move |inp: Input| -> Result { match_cp(inp, 43) }),
            Box::new(move |inp: Input| -> Result { match_cp(inp, 45) })]) }) }),
        Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { match_range(inp, 48, 57) }) })])
}

// [17] WS 
fn ws(inp: Input) -> Result {
    star(inp, &|inp: Input| -> Result { alt(inp, vec![
        Box::new(move |inp: Input| -> Result { match_cp(inp, 0x20) }),
        Box::new(move |inp: Input| -> Result { match_cp(inp, 0x9) }),
        Box::new(move |inp: Input| -> Result { match_cp(inp, 0x0A) }),
        Box::new(move |inp: Input| -> Result { match_cp(inp, 0x0D) })]) })
}

// ── API ──

fn print_ast(node: &Ast, depth: usize) {
    let indent = "  ".repeat(depth);
    match node {
        Ast::Leaf(text) => println!("{indent}SCALAR: \"{text}\""),
        Ast::Branch(tag, children) => {
            println!("{indent}{tag}");
            for c in children { print_ast(c, depth+1); }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let text = if args.len()>1 { fs::read_to_string(&args[1]).expect("Cannot open") }
    else { let mut s=String::new(); io::stdin().read_to_string(&mut s).unwrap(); s };
    let inp = Input::new(&text);
    let r = json_text(inp);
    if !r.fail { println!("OK: {} chars", r.rest.pos); if let Some(a)=&r.ast { print_ast(a,0); } }
    else { eprintln!("FAIL @{}: {}", r.rest.pos, r.err); std::process::exit(1); }
}
