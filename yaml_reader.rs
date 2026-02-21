// ════════════════════════════════════════════════════════════════
// yaml_reader.rs — YAML 1.2 parser, projected from yaml-grammar.scm
// ════════════════════════════════════════════════════════════════
// Generated. DO NOT EDIT — regenerate from the grammar.
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

// ── YAML extensions ──

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
    if i+sp<len&&s[i+sp]!=b'\n' { let mut r=ok(inp); r.tag_int=std::cmp::max(1,sp as i32-n); return r; }
    let mut j=i; while j<len&&s[j]!=b'\n' { j+=1; }
    while j<len { if s[j]==b'\n' { j+=1; } if j>=len { break; }
        sp=0; while j+sp<len&&s[j+sp]==b' ' { sp+=1; }
        let nx=j+sp; if nx>=len||s[nx]==b'\n' { j=nx; continue; }
        let mut r=ok(inp); r.tag_int=std::cmp::max(1,sp as i32-n); return r; }
    let mut r=ok(inp); r.tag_int=1; r }

fn parse_int(inp: Input, f: &dyn Fn(Input)->Result) -> Result {
    let mut r=f(inp); if r.fail { return r; }
    r.tag_int=r.val.chars().filter(|c|c.is_ascii_digit()).fold(0i32,|a,c|a*10+(c as i32-'0' as i32)); r }
fn parse_sym(inp: Input, f: &dyn Fn(Input)->Result, sym: &str) -> Result {
    let mut r=f(inp); if r.fail { return r; } r.tag=sym.to_string(); r }
fn val(inp: Input, v: &str) -> Result { let mut r=ok(inp); r.tag=v.to_string(); r }

// ════════════════════════════════════════════════════════════════
// YAML 1.2 Grammar — 211 rules
// ════════════════════════════════════════════════════════════════

// [1] C-PRINTABLE
fn c_printable(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 0x9) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 0x0A) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 0x0D) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0x20, 0x7E) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 0x85) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0xA0, 0xD7FF) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0xE000, 0xFFFD) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0x10000, 0x10FFFF) })])
}

// [2] NB-JSON
fn nb_json(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 0x9) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0x20, 0x10FFFF) })])
}

// [3] C-BYTE-ORDER-MARK
fn c_byte_order_mark(inp: Input) -> Result {
    match_cp(inp, 0xFEFF)
}

// [4] C-SEQUENCE-ENTRY
fn c_sequence_entry(inp: Input) -> Result {
    match_cp(inp, 45)
}

// [5] C-MAPPING-KEY
fn c_mapping_key(inp: Input) -> Result {
    match_cp(inp, 63)
}

// [6] C-MAPPING-VALUE
fn c_mapping_value(inp: Input) -> Result {
    match_cp(inp, 58)
}

// [7] C-COLLECT-ENTRY
fn c_collect_entry(inp: Input) -> Result {
    match_cp(inp, 44)
}

// [8] C-SEQUENCE-START
fn c_sequence_start(inp: Input) -> Result {
    match_cp(inp, 91)
}

// [9] C-SEQUENCE-END
fn c_sequence_end(inp: Input) -> Result {
    match_cp(inp, 93)
}

// [10] C-MAPPING-START
fn c_mapping_start(inp: Input) -> Result {
    match_cp(inp, 123)
}

// [11] C-MAPPING-END
fn c_mapping_end(inp: Input) -> Result {
    match_cp(inp, 125)
}

// [12] C-COMMENT
fn c_comment(inp: Input) -> Result {
    match_cp(inp, 35)
}

// [13] C-ANCHOR
fn c_anchor(inp: Input) -> Result {
    match_cp(inp, 38)
}

// [14] C-ALIAS
fn c_alias(inp: Input) -> Result {
    match_cp(inp, 42)
}

// [15] C-TAG
fn c_tag(inp: Input) -> Result {
    match_cp(inp, 33)
}

// [16] C-LITERAL
fn c_literal(inp: Input) -> Result {
    match_cp(inp, 124)
}

// [17] C-FOLDED
fn c_folded(inp: Input) -> Result {
    match_cp(inp, 62)
}

// [18] C-SINGLE-QUOTE
fn c_single_quote(inp: Input) -> Result {
    match_cp(inp, 39)
}

// [19] C-DOUBLE-QUOTE
fn c_double_quote(inp: Input) -> Result {
    match_cp(inp, 34)
}

// [20] C-DIRECTIVE
fn c_directive(inp: Input) -> Result {
    match_cp(inp, 37)
}

// [21] C-RESERVED
fn c_reserved(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 64) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 96) })])
}

// [22] C-INDICATOR
fn c_indicator(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_sequence_entry(inp) }), Box::new(move |inp: Input| -> Result { c_mapping_key(inp) }), Box::new(move |inp: Input| -> Result { c_mapping_value(inp) }), Box::new(move |inp: Input| -> Result { c_collect_entry(inp) }), Box::new(move |inp: Input| -> Result { c_sequence_start(inp) }), Box::new(move |inp: Input| -> Result { c_sequence_end(inp) }), Box::new(move |inp: Input| -> Result { c_mapping_start(inp) }), Box::new(move |inp: Input| -> Result { c_mapping_end(inp) }), Box::new(move |inp: Input| -> Result { c_comment(inp) }), Box::new(move |inp: Input| -> Result { c_anchor(inp) }), Box::new(move |inp: Input| -> Result { c_alias(inp) }), Box::new(move |inp: Input| -> Result { c_tag(inp) }), Box::new(move |inp: Input| -> Result { c_literal(inp) }), Box::new(move |inp: Input| -> Result { c_folded(inp) }), Box::new(move |inp: Input| -> Result { c_single_quote(inp) }), Box::new(move |inp: Input| -> Result { c_double_quote(inp) }), Box::new(move |inp: Input| -> Result { c_directive(inp) }), Box::new(move |inp: Input| -> Result { c_reserved(inp) })])
}

// [23] C-FLOW-INDICATOR
fn c_flow_indicator(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_collect_entry(inp) }), Box::new(move |inp: Input| -> Result { c_sequence_start(inp) }), Box::new(move |inp: Input| -> Result { c_sequence_end(inp) }), Box::new(move |inp: Input| -> Result { c_mapping_start(inp) }), Box::new(move |inp: Input| -> Result { c_mapping_end(inp) })])
}

// [24] B-LINE-FEED
fn b_line_feed(inp: Input) -> Result {
    match_cp(inp, 0x0A)
}

// [25] B-CARRIAGE-RETURN
fn b_carriage_return(inp: Input) -> Result {
    match_cp(inp, 0x0D)
}

// [26] B-CHAR
fn b_char(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { b_line_feed(inp) }), Box::new(move |inp: Input| -> Result { b_carriage_return(inp) })])
}

// [27] NB-CHAR
fn nb_char(inp: Input) -> Result {
    minus(inp, &|inp: Input| -> Result { c_printable(inp) }, &|inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { b_char(inp) }), Box::new(move |inp: Input| -> Result { c_byte_order_mark(inp) })]) })
}

// [28] B-BREAK
fn b_break(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { b_carriage_return(inp) }), Box::new(move |inp: Input| -> Result { b_line_feed(inp) })]) }), Box::new(move |inp: Input| -> Result { b_carriage_return(inp) }), Box::new(move |inp: Input| -> Result { b_line_feed(inp) })])
}

// [29] B-AS-LINE-FEED
fn b_as_line_feed(inp: Input) -> Result {
    b_break(inp)
}

// [30] B-NON-CONTENT
fn b_non_content(inp: Input) -> Result {
    b_break(inp)
}

// [31] S-SPACE
fn s_space(inp: Input) -> Result {
    match_cp(inp, 0x20)
}

// [32] S-TAB
fn s_tab(inp: Input) -> Result {
    match_cp(inp, 0x9)
}

// [33] S-WHITE
fn s_white(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { s_space(inp) }), Box::new(move |inp: Input| -> Result { s_tab(inp) })])
}

// [34] NS-CHAR
fn ns_char(inp: Input) -> Result {
    minus(inp, &|inp: Input| -> Result { nb_char(inp) }, &|inp: Input| -> Result { s_white(inp) })
}

// [35] NS-DEC-DIGIT
fn ns_dec_digit(inp: Input) -> Result {
    match_range(inp, 0x30, 0x39)
}

// [36] NS-HEX-DIGIT
fn ns_hex_digit(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { ns_dec_digit(inp) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0x41, 0x46) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0x61, 0x66) })])
}

// [37] NS-ASCII-LETTER
fn ns_ascii_letter(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { match_range(inp, 0x41, 0x5A) }), Box::new(move |inp: Input| -> Result { match_range(inp, 0x61, 0x7A) })])
}

// [38] NS-WORD-CHAR
fn ns_word_char(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { ns_dec_digit(inp) }), Box::new(move |inp: Input| -> Result { ns_ascii_letter(inp) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 45) })])
}

// [39] NS-URI-CHAR
fn ns_uri_char(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 37) }), Box::new(move |inp: Input| -> Result { ns_hex_digit(inp) }), Box::new(move |inp: Input| -> Result { ns_hex_digit(inp) })]) }), Box::new(move |inp: Input| -> Result { ns_word_char(inp) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 35) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 59) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 47) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 63) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 58) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 64) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 38) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 61) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 43) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 36) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 44) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 95) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 46) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 33) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 126) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 42) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 39) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 40) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 41) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 91) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 93) })])
}

// [40] NS-TAG-CHAR
fn ns_tag_char(inp: Input) -> Result {
    minus(inp, &|inp: Input| -> Result { ns_uri_char(inp) }, &|inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { c_tag(inp) }), Box::new(move |inp: Input| -> Result { c_flow_indicator(inp) })]) })
}

// [41] C-ESCAPE
fn c_escape(inp: Input) -> Result {
    match_cp(inp, 92)
}

// [42] NS-ESC-NULL
fn ns_esc_null(inp: Input) -> Result {
    match_cp(inp, 48)
}

// [43] NS-ESC-BELL
fn ns_esc_bell(inp: Input) -> Result {
    match_cp(inp, 97)
}

// [44] NS-ESC-BACKSPACE
fn ns_esc_backspace(inp: Input) -> Result {
    match_cp(inp, 98)
}

// [45] NS-ESC-HORIZONTAL-TAB
fn ns_esc_horizontal_tab(inp: Input) -> Result {
    match_cp(inp, 116)
}

// [46] NS-ESC-LINE-FEED
fn ns_esc_line_feed(inp: Input) -> Result {
    match_cp(inp, 110)
}

// [47] NS-ESC-VERTICAL-TAB
fn ns_esc_vertical_tab(inp: Input) -> Result {
    match_cp(inp, 118)
}

// [48] NS-ESC-FORM-FEED
fn ns_esc_form_feed(inp: Input) -> Result {
    match_cp(inp, 102)
}

// [49] NS-ESC-CARRIAGE-RETURN
fn ns_esc_carriage_return(inp: Input) -> Result {
    match_cp(inp, 114)
}

// [50] NS-ESC-ESCAPE
fn ns_esc_escape(inp: Input) -> Result {
    match_cp(inp, 101)
}

// [51] NS-ESC-SPACE
fn ns_esc_space(inp: Input) -> Result {
    match_cp(inp, 0x20)
}

// [52] NS-ESC-DOUBLE-QUOTE
fn ns_esc_double_quote(inp: Input) -> Result {
    match_cp(inp, 34)
}

// [53] NS-ESC-SLASH
fn ns_esc_slash(inp: Input) -> Result {
    match_cp(inp, 47)
}

// [54] NS-ESC-BACKSLASH
fn ns_esc_backslash(inp: Input) -> Result {
    match_cp(inp, 92)
}

// [55] NS-ESC-NEXT-LINE
fn ns_esc_next_line(inp: Input) -> Result {
    match_cp(inp, 78)
}

// [56] NS-ESC-NON-BREAKING-SPACE
fn ns_esc_non_breaking_space(inp: Input) -> Result {
    match_cp(inp, 95)
}

// [57] NS-ESC-LINE-SEPARATOR
fn ns_esc_line_separator(inp: Input) -> Result {
    match_cp(inp, 76)
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR
fn ns_esc_paragraph_separator(inp: Input) -> Result {
    match_cp(inp, 80)
}

// [59] NS-ESC-8-BIT
fn ns_esc_8_bit(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 120) }), Box::new(move |inp: Input| -> Result { rep(inp, 2, &|inp: Input| -> Result { ns_hex_digit(inp) }) })])
}

// [60] NS-ESC-16-BIT
fn ns_esc_16_bit(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 117) }), Box::new(move |inp: Input| -> Result { rep(inp, 4, &|inp: Input| -> Result { ns_hex_digit(inp) }) })])
}

// [61] NS-ESC-32-BIT
fn ns_esc_32_bit(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 85) }), Box::new(move |inp: Input| -> Result { rep(inp, 8, &|inp: Input| -> Result { ns_hex_digit(inp) }) })])
}

// [62] C-NS-ESC-CHAR
fn c_ns_esc_char(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { c_escape(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { ns_esc_null(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_bell(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_backspace(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_horizontal_tab(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_line_feed(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_vertical_tab(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_form_feed(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_carriage_return(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_escape(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_space(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_double_quote(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_slash(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_backslash(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_next_line(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_non_breaking_space(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_line_separator(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_paragraph_separator(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_8_bit(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_16_bit(inp) }), Box::new(move |inp: Input| -> Result { ns_esc_32_bit(inp) })]) })])
}

// [63] S-INDENT
fn s_indent(inp: Input, n: i32) -> Result {
    rep(inp, n, &|inp: Input| -> Result { s_space(inp) })
}

// [64] S-INDENT-LT
fn s_indent_lt(inp: Input, n: i32) -> Result {
    star(inp, &|inp: Input| -> Result { s_space(inp) })
}

// [65] S-INDENT-LE
fn s_indent_le(inp: Input, n: i32) -> Result {
    star(inp, &|inp: Input| -> Result { s_space(inp) })
}

// [66] S-SEPARATE-IN-LINE
fn s_separate_in_line(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { s_white(inp) }) }), Box::new(move |inp: Input| -> Result { ok(inp) })])
}

// [67] S-LINE-PREFIX
fn s_line_prefix(inp: Input, n: i32, c: String) -> Result {
    (|| -> Result { if c == "BLOCK-IN" { return s_block_line_prefix(inp, n); } if c == "BLOCK-OUT" { return s_block_line_prefix(inp, n); } if c == "FLOW-IN" { return s_flow_line_prefix(inp, n); } if c == "FLOW-OUT" { return s_flow_line_prefix(inp, n); } fail(inp.clone(), "no case") })()
}

// [68] S-BLOCK-LINE-PREFIX
fn s_block_line_prefix(inp: Input, n: i32) -> Result {
    s_indent(inp, n)
}

// [69] S-FLOW-LINE-PREFIX
fn s_flow_line_prefix(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, n) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { s_separate_in_line(inp) }) })])
}

// [70] L-EMPTY
fn l_empty(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_line_prefix(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_indent_lt(inp, n) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); b_as_line_feed(inp) }) }])
}

// [71] B-L-TRIMMED
fn b_l_trimmed(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); b_non_content(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); plus_(inp, &|inp: Input| -> Result { l_empty(inp, n, c.clone()) }) }) }])
}

// [72] B-AS-SPACE
fn b_as_space(inp: Input) -> Result {
    b_break(inp)
}

// [73] B-L-FOLDED
fn b_l_folded(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); b_l_trimmed(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); b_as_space(inp) }) }])
}

// [74] S-FLOW-FOLDED
fn s_flow_folded(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { s_separate_in_line(inp) }) }), Box::new(move |inp: Input| -> Result { b_l_folded(inp, n, "FLOW-IN".to_string()) }), Box::new(move |inp: Input| -> Result { s_flow_line_prefix(inp, n) })])
}

// [75] C-NB-COMMENT-TEXT
fn c_nb_comment_text(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { c_comment(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { nb_char(inp) }) })])
}

// [76] B-COMMENT
fn b_comment(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { b_non_content(inp) }), Box::new(move |inp: Input| -> Result { ok(inp) })])
}

// [77] S-B-COMMENT
fn s_b_comment(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_separate_in_line(inp) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { c_nb_comment_text(inp) }) })]) }) }), Box::new(move |inp: Input| -> Result { b_comment(inp) })])
}

// [78] L-COMMENT
fn l_comment(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_separate_in_line(inp) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { c_nb_comment_text(inp) }) }), Box::new(move |inp: Input| -> Result { b_non_content(inp) })])
}

// [79] S-L-COMMENTS
fn s_l_comments(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { s_b_comment(inp) }), Box::new(move |inp: Input| -> Result { ok(inp) })]) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_comment(inp) }) })])
}

// [80] S-SEPARATE
fn s_separate(inp: Input, n: i32, c: String) -> Result {
    (|| -> Result { if c == "BLOCK-OUT" { return s_separate_lines(inp, n); } if c == "BLOCK-IN" { return s_separate_lines(inp, n); } if c == "FLOW-OUT" { return s_separate_lines(inp, n); } if c == "FLOW-IN" { return s_separate_lines(inp, n); } if c == "BLOCK-KEY" { return s_separate_in_line(inp); } if c == "FLOW-KEY" { return s_separate_in_line(inp); } fail(inp.clone(), "no case") })()
}

// [81] S-SEPARATE-LINES
fn s_separate_lines(inp: Input, n: i32) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_l_comments(inp) }), Box::new(move |inp: Input| -> Result { s_flow_line_prefix(inp, n) })]) }), Box::new(move |inp: Input| -> Result { s_separate_in_line(inp) })])
}

// [82] L-DIRECTIVE
fn l_directive(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { c_directive(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { ns_yaml_directive(inp) }), Box::new(move |inp: Input| -> Result { ns_tag_directive(inp) }), Box::new(move |inp: Input| -> Result { ns_reserved_directive(inp) })]) }), Box::new(move |inp: Input| -> Result { s_l_comments(inp) })])
}

// [83] NS-RESERVED-DIRECTIVE
fn ns_reserved_directive(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { ns_directive_name(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_separate_in_line(inp) }), Box::new(move |inp: Input| -> Result { ns_directive_parameter(inp) })]) }) })])
}

// [84] NS-DIRECTIVE-NAME
fn ns_directive_name(inp: Input) -> Result {
    plus_(inp, &|inp: Input| -> Result { ns_char(inp) })
}

// [85] NS-DIRECTIVE-PARAMETER
fn ns_directive_parameter(inp: Input) -> Result {
    plus_(inp, &|inp: Input| -> Result { ns_char(inp) })
}

// [86] NS-YAML-DIRECTIVE
fn ns_yaml_directive(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_str(inp, "YAML") }), Box::new(move |inp: Input| -> Result { s_separate_in_line(inp) }), Box::new(move |inp: Input| -> Result { ns_yaml_version(inp) })])
}

// [87] NS-YAML-VERSION
fn ns_yaml_version(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { ns_dec_digit(inp) }) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 46) }), Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { ns_dec_digit(inp) }) })])
}

// [88] NS-TAG-DIRECTIVE
fn ns_tag_directive(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_str(inp, "TAG") }), Box::new(move |inp: Input| -> Result { s_separate_in_line(inp) }), Box::new(move |inp: Input| -> Result { c_tag_handle(inp) }), Box::new(move |inp: Input| -> Result { s_separate_in_line(inp) }), Box::new(move |inp: Input| -> Result { ns_tag_prefix(inp) })])
}

// [89] C-TAG-HANDLE
fn c_tag_handle(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_named_tag_handle(inp) }), Box::new(move |inp: Input| -> Result { c_secondary_tag_handle(inp) }), Box::new(move |inp: Input| -> Result { c_primary_tag_handle(inp) })])
}

// [90] C-PRIMARY-TAG-HANDLE
fn c_primary_tag_handle(inp: Input) -> Result {
    match_cp(inp, 33)
}

// [91] C-SECONDARY-TAG-HANDLE
fn c_secondary_tag_handle(inp: Input) -> Result {
    match_str(inp, "!!")
}

// [92] C-NAMED-TAG-HANDLE
fn c_named_tag_handle(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 33) }), Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { ns_word_char(inp) }) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 33) })])
}

// [93] NS-TAG-PREFIX
fn ns_tag_prefix(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_ns_local_tag_prefix(inp) }), Box::new(move |inp: Input| -> Result { ns_global_tag_prefix(inp) })])
}

// [94] C-NS-LOCAL-TAG-PREFIX
fn c_ns_local_tag_prefix(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 33) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { ns_uri_char(inp) }) })])
}

// [95] NS-GLOBAL-TAG-PREFIX
fn ns_global_tag_prefix(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { ns_tag_char(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { ns_uri_char(inp) }) })])
}

// [96] C-NS-PROPERTIES
fn c_ns_properties(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_tag_property(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_anchor_property(inp) }) }]) }) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_anchor_property(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_tag_property(inp) }) }]) }) }) }]) }) }])
}

// [97] C-NS-TAG-PROPERTY
fn c_ns_tag_property(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_verbatim_tag(inp) }), Box::new(move |inp: Input| -> Result { c_ns_shorthand_tag(inp) }), Box::new(move |inp: Input| -> Result { c_non_specific_tag(inp) })])
}

// [98] C-VERBATIM-TAG
fn c_verbatim_tag(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_str(inp, "!<") }), Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { ns_uri_char(inp) }) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 62) })])
}

// [99] C-NS-SHORTHAND-TAG
fn c_ns_shorthand_tag(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { c_tag_handle(inp) }), Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { ns_tag_char(inp) }) })])
}

// [100] C-NON-SPECIFIC-TAG
fn c_non_specific_tag(inp: Input) -> Result {
    match_cp(inp, 33)
}

// [101] C-NS-ANCHOR-PROPERTY
fn c_ns_anchor_property(inp: Input) -> Result {
    build(inp, "ANCHOR", &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { c_anchor(inp) }), Box::new(move |inp: Input| -> Result { scalar(inp, &|inp: Input| -> Result { ns_anchor_name(inp) }) })]) })
}

// [102] NS-ANCHOR-CHAR
fn ns_anchor_char(inp: Input) -> Result {
    minus(inp, &|inp: Input| -> Result { ns_char(inp) }, &|inp: Input| -> Result { c_flow_indicator(inp) })
}

// [103] NS-ANCHOR-NAME
fn ns_anchor_name(inp: Input) -> Result {
    plus_(inp, &|inp: Input| -> Result { ns_anchor_char(inp) })
}

// [104] C-NS-ALIAS-NODE
fn c_ns_alias_node(inp: Input) -> Result {
    build(inp, "ALIAS", &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { c_alias(inp) }), Box::new(move |inp: Input| -> Result { scalar(inp, &|inp: Input| -> Result { ns_anchor_name(inp) }) })]) })
}

// [105] E-SCALAR
fn e_scalar(inp: Input) -> Result {
    ok(inp)
}

// [106] E-NODE
fn e_node(inp: Input) -> Result {
    e_scalar(inp)
}

// [107] NB-DOUBLE-CHAR
fn nb_double_char(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_ns_esc_char(inp) }), Box::new(move |inp: Input| -> Result { minus(inp, &|inp: Input| -> Result { nb_json(inp) }, &|inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 92) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 34) })]) }) })])
}

// [108] NS-DOUBLE-CHAR
fn ns_double_char(inp: Input) -> Result {
    minus(inp, &|inp: Input| -> Result { nb_double_char(inp) }, &|inp: Input| -> Result { s_white(inp) })
}

// [109] C-DOUBLE-QUOTED
fn c_double_quoted(inp: Input, n: i32, c: String) -> Result {
    scalar(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 34) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); nb_double_text(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 34) }) }]) })
}

// [110] NB-DOUBLE-TEXT
fn nb_double_text(inp: Input, n: i32, c: String) -> Result {
    (|| -> Result { if c == "FLOW-OUT" { return nb_double_multi_line(inp, n); } if c == "FLOW-IN" { return nb_double_multi_line(inp, n); } if c == "BLOCK-KEY" { return nb_double_one_line(inp); } if c == "FLOW-KEY" { return nb_double_one_line(inp); } fail(inp.clone(), "no case") })()
}

// [111] NB-DOUBLE-ONE-LINE
fn nb_double_one_line(inp: Input) -> Result {
    star(inp, &|inp: Input| -> Result { nb_double_char(inp) })
}

// [112] S-DOUBLE-ESCAPED
fn s_double_escaped(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { s_white(inp) }) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 92) }), Box::new(move |inp: Input| -> Result { b_non_content(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_empty(inp, n, "FLOW-IN".to_string()) }) }), Box::new(move |inp: Input| -> Result { s_flow_line_prefix(inp, n) })])
}

// [113] S-DOUBLE-BREAK
fn s_double_break(inp: Input, n: i32) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { s_double_escaped(inp, n) }), Box::new(move |inp: Input| -> Result { s_flow_folded(inp, n) })])
}

// [114] NB-NS-DOUBLE-IN-LINE
fn nb_ns_double_in_line(inp: Input) -> Result {
    star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { s_white(inp) }) }), Box::new(move |inp: Input| -> Result { ns_double_char(inp) })]) })
}

// [115] S-DOUBLE-NEXT-LINE
fn s_double_next_line(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_double_break(inp, n) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { ns_double_char(inp) }), Box::new(move |inp: Input| -> Result { nb_ns_double_in_line(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { s_double_next_line(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { s_white(inp) }) })]) })]) }) })])
}

// [116] NB-DOUBLE-MULTI-LINE
fn nb_double_multi_line(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { nb_ns_double_in_line(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { s_double_next_line(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { s_white(inp) }) })]) })])
}

// [117] C-QUOTED-QUOTE
fn c_quoted_quote(inp: Input) -> Result {
    match_str(inp, "''")
}

// [118] NB-SINGLE-CHAR
fn nb_single_char(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_quoted_quote(inp) }), Box::new(move |inp: Input| -> Result { minus(inp, &|inp: Input| -> Result { nb_json(inp) }, &|inp: Input| -> Result { match_cp(inp, 39) }) })])
}

// [119] NS-SINGLE-CHAR
fn ns_single_char(inp: Input) -> Result {
    minus(inp, &|inp: Input| -> Result { nb_single_char(inp) }, &|inp: Input| -> Result { s_white(inp) })
}

// [120] C-SINGLE-QUOTED
fn c_single_quoted(inp: Input, n: i32, c: String) -> Result {
    scalar(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 39) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); nb_single_text(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 39) }) }]) })
}

// [121] NB-SINGLE-TEXT
fn nb_single_text(inp: Input, n: i32, c: String) -> Result {
    (|| -> Result { if c == "FLOW-OUT" { return nb_single_multi_line(inp, n); } if c == "FLOW-IN" { return nb_single_multi_line(inp, n); } if c == "BLOCK-KEY" { return nb_single_one_line(inp); } if c == "FLOW-KEY" { return nb_single_one_line(inp); } fail(inp.clone(), "no case") })()
}

// [122] NB-SINGLE-ONE-LINE
fn nb_single_one_line(inp: Input) -> Result {
    star(inp, &|inp: Input| -> Result { nb_single_char(inp) })
}

// [123] NS-SINGLE-IN-LINE
fn ns_single_in_line(inp: Input) -> Result {
    star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { s_white(inp) }) }), Box::new(move |inp: Input| -> Result { ns_single_char(inp) })]) })
}

// [124] S-SINGLE-NEXT-LINE
fn s_single_next_line(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_flow_folded(inp, n) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { ns_single_char(inp) }), Box::new(move |inp: Input| -> Result { ns_single_in_line(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { s_single_next_line(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { s_white(inp) }) })]) })]) }) })])
}

// [125] NB-SINGLE-MULTI-LINE
fn nb_single_multi_line(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { ns_single_in_line(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { s_single_next_line(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { s_white(inp) }) })]) })])
}

// [126] NS-PLAIN-FIRST
fn ns_plain_first(inp: Input, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); minus(inp, &|inp: Input| -> Result { ns_char(inp) }, &|inp: Input| -> Result { c_indicator(inp) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 63) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 58) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 45) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ahead(inp, &|inp: Input| -> Result { ns_plain_safe(inp, c.clone()) }) }) }]) }) }])
}

// [127] NS-PLAIN-SAFE
fn ns_plain_safe(inp: Input, c: String) -> Result {
    (|| -> Result { if c == "FLOW-OUT" { return ns_plain_safe_out(inp); } if c == "FLOW-IN" { return ns_plain_safe_in(inp); } if c == "BLOCK-KEY" { return ns_plain_safe_out(inp); } if c == "FLOW-KEY" { return ns_plain_safe_in(inp); } fail(inp.clone(), "no case") })()
}

// [128] NS-PLAIN-SAFE-OUT
fn ns_plain_safe_out(inp: Input) -> Result {
    ns_char(inp)
}

// [129] NS-PLAIN-SAFE-IN
fn ns_plain_safe_in(inp: Input) -> Result {
    minus(inp, &|inp: Input| -> Result { ns_char(inp) }, &|inp: Input| -> Result { c_flow_indicator(inp) })
}

// [130] NS-PLAIN-CHAR
fn ns_plain_char(inp: Input, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); minus(inp, &|inp: Input| -> Result { ns_plain_safe(inp, c.clone()) }, &|inp: Input| -> Result { alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 58) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 35) }) }]) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); behind(inp, &|inp: Input| -> Result { ns_char(inp) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 35) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 58) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ahead(inp, &|inp: Input| -> Result { ns_plain_safe(inp, c.clone()) }) }) }]) }) }])
}

// [131] NS-PLAIN
fn ns_plain(inp: Input, n: i32, c: String) -> Result {
    scalar(inp, &|inp: Input| -> Result { (|| -> Result { if c == "FLOW-OUT" { return ns_plain_multi_line(inp, n, c.clone()); } if c == "FLOW-IN" { return ns_plain_multi_line(inp, n, c.clone()); } if c == "BLOCK-KEY" { return ns_plain_one_line(inp, c.clone()); } if c == "FLOW-KEY" { return ns_plain_one_line(inp, c.clone()); } fail(inp.clone(), "no case") })() })
}

// [132] NB-NS-PLAIN-IN-LINE
fn nb_ns_plain_in_line(inp: Input, c: String) -> Result {
    star(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); star(inp, &|inp: Input| -> Result { s_white(inp) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_plain_char(inp, c.clone()) }) }]) })
}

// [133] NS-PLAIN-ONE-LINE
fn ns_plain_one_line(inp: Input, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_plain_first(inp, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); nb_ns_plain_in_line(inp, c.clone()) }) }])
}

// [134] S-NS-PLAIN-NEXT-LINE
fn s_ns_plain_next_line(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_flow_folded(inp, n) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); neg(inp, &|inp: Input| -> Result { c_forbidden(inp) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_plain_char(inp, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); nb_ns_plain_in_line(inp, c.clone()) }) }])
}

// [135] NS-PLAIN-MULTI-LINE
fn ns_plain_multi_line(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_plain_one_line(inp, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); star(inp, &|inp: Input| -> Result { s_ns_plain_next_line(inp, n, c.clone()) }) }) }])
}

// [137] C-FLOW-SEQUENCE
fn c_flow_sequence(inp: Input, n: i32, c: String) -> Result {
    build(inp, "SEQUENCE", &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 91) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { collect(inp, &|inp: Input| -> Result { ns_s_flow_seq_entries(inp, n, in_flow(&c.clone())) }) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 93) }) }]) })
}

// [138] NS-S-FLOW-SEQ-ENTRIES
fn ns_s_flow_seq_entries(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_seq_entry(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 44) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { ns_s_flow_seq_entries(inp, n, c.clone()) }) }) }]) }) }) }])
}

// [139] NS-FLOW-SEQ-ENTRY
fn ns_flow_seq_entry(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_pair(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_node(inp, n, c.clone()) }) }])
}

// [140] C-FLOW-MAPPING
fn c_flow_mapping(inp: Input, n: i32, c: String) -> Result {
    build(inp, "MAPPING", &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 123) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { collect(inp, &|inp: Input| -> Result { ns_s_flow_map_entries(inp, n, in_flow(&c.clone())) }) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 125) }) }]) })
}

// [141] NS-S-FLOW-MAP-ENTRIES
fn ns_s_flow_map_entries(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_map_entry(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 44) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { ns_s_flow_map_entries(inp, n, c.clone()) }) }) }]) }) }) }])
}

// [142] NS-FLOW-MAP-ENTRY
fn ns_flow_map_entry(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 63) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_map_explicit_entry(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_map_implicit_entry(inp, n, c.clone()) }) }])
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY
fn ns_flow_map_explicit_entry(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_map_implicit_entry(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }]) }) }])
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY
fn ns_flow_map_implicit_entry(inp: Input, n: i32, c: String) -> Result {
    build(inp, "PAIR", &|inp: Input| -> Result { alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_map_yaml_key_entry(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_empty_key_entry(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_json_key_entry(inp, n, c.clone()) }) }]) })
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY
fn ns_flow_map_yaml_key_entry(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_yaml_node(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_separate_value(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }]) }) }])
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY
fn c_ns_flow_map_empty_key_entry(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_separate_value(inp, n, c.clone()) }) }])
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE
fn c_ns_flow_map_separate_value(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 58) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); neg(inp, &|inp: Input| -> Result { ns_plain_safe(inp, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_node(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }]) }) }])
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY
fn c_ns_flow_map_json_key_entry(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_flow_json_node(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_adjacent_value(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }]) }) }])
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE
fn c_ns_flow_map_adjacent_value(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 58) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate(inp, n, c.clone()) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_node(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }]) }) }])
}

// [150] NS-FLOW-PAIR
fn ns_flow_pair(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); match_cp(inp, 63) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_map_explicit_entry(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_pair_entry(inp, n, c.clone()) }) }])
}

// [151] NS-FLOW-PAIR-ENTRY
fn ns_flow_pair_entry(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_pair_yaml_key_entry(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_empty_key_entry(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_pair_json_key_entry(inp, n, c.clone()) }) }])
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY
fn ns_flow_pair_yaml_key_entry(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_s_implicit_yaml_key(inp, "FLOW-KEY".to_string()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_separate_value(inp, n, c.clone()) }) }])
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY
fn c_ns_flow_pair_json_key_entry(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_s_implicit_json_key(inp, "FLOW-KEY".to_string()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_flow_map_adjacent_value(inp, n, c.clone()) }) }])
}

// [154] NS-S-IMPLICIT-YAML-KEY
fn ns_s_implicit_yaml_key(inp: Input, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_yaml_node(inp, 0, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate_in_line(inp) }) }) }])
}

// [155] C-S-IMPLICIT-JSON-KEY
fn c_s_implicit_json_key(inp: Input, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_flow_json_node(inp, 0, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { s_separate_in_line(inp) }) }) }])
}

// [156] NS-FLOW-YAML-CONTENT
fn ns_flow_yaml_content(inp: Input, n: i32, c: String) -> Result {
    ns_plain(inp, n, c.clone())
}

// [157] C-FLOW-JSON-CONTENT
fn c_flow_json_content(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_flow_sequence(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_flow_mapping(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_single_quoted(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_double_quoted(inp, n, c.clone()) }) }])
}

// [158] NS-FLOW-CONTENT
fn ns_flow_content(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_yaml_content(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_flow_json_content(inp, n, c.clone()) }) }])
}

// [159] NS-FLOW-YAML-NODE
fn ns_flow_yaml_node(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_alias_node(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_yaml_content(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_properties(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_yaml_content(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_scalar(inp) }) }]) }) }]) }) }])
}

// [160] C-FLOW-JSON-NODE
fn c_flow_json_node(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_properties(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }]) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_flow_json_content(inp, n, c.clone()) }) }])
}

// [161] NS-FLOW-NODE
fn ns_flow_node(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_alias_node(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_content(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_properties(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_flow_content(inp, n, c.clone()) }) }]) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_scalar(inp) }) }]) }) }]) }) }])
}

// [162] C-B-BLOCK-HEADER
fn c_b_block_header(inp: Input, n: i32) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { (|| -> Result { let r = alt(inp, vec![Box::new(move |inp: Input| -> Result { parse_int(inp, &|inp: Input| -> Result { ns_dec_digit(inp) }) }), Box::new(move |inp: Input| -> Result { detect_indent(inp, n) })]); if r.fail { return r; } let m: i32 = r.tag_int; let inp = r.rest; (|| -> Result { let r = alt(inp, vec![Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 45) }, "STRIP") }), Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 43) }, "KEEP") }), Box::new(move |inp: Input| -> Result { val(inp, "CLIP") })]); if r.fail { return r; } let t: String = r.tag.clone(); let inp = r.rest; s_b_comment(inp) })() })() }), Box::new(move |inp: Input| -> Result { (|| -> Result { let r = alt(inp, vec![Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 45) }, "STRIP") }), Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 43) }, "KEEP") }), Box::new(move |inp: Input| -> Result { val(inp, "CLIP") })]); if r.fail { return r; } let t: String = r.tag.clone(); let inp = r.rest; (|| -> Result { let r = alt(inp, vec![{ let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); parse_int(inp, &|inp: Input| -> Result { ns_dec_digit(inp) }) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); detect_indent(inp, n) }) }]); if r.fail { return r; } let m: i32 = r.tag_int; let inp = r.rest; s_b_comment(inp) })() })() })])
}

// [163] C-INDENTATION-INDICATOR
fn c_indentation_indicator(inp: Input, n: i32) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { ns_dec_digit(inp) }), Box::new(move |inp: Input| -> Result { ok(inp) })])
}

// [164] C-CHOMPING-INDICATOR
fn c_chomping_indicator(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 45) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 43) }), Box::new(move |inp: Input| -> Result { ok(inp) })])
}

// [165] B-CHOMPED-LAST
fn b_chomped_last(inp: Input, t: String) -> Result {
    (|| -> Result { if t == "STRIP" { return b_non_content(inp); } if t == "CLIP" { return b_as_line_feed(inp); } if t == "KEEP" { return b_as_line_feed(inp); } fail(inp.clone(), "no case") })()
}

// [166] L-CHOMPED-EMPTY
fn l_chomped_empty(inp: Input, n: i32, t: String) -> Result {
    (|| -> Result { if t == "STRIP" { return l_strip_empty(inp, n); } if t == "CLIP" { return l_strip_empty(inp, n); } if t == "KEEP" { return l_keep_empty(inp, n); } fail(inp.clone(), "no case") })()
}

// [167] L-STRIP-EMPTY
fn l_strip_empty(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent_le(inp, n) }), Box::new(move |inp: Input| -> Result { b_non_content(inp) })]) }) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { l_trail_comments(inp, n) }) })])
}

// [168] L-KEEP-EMPTY
fn l_keep_empty(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_empty(inp, n, "BLOCK-IN".to_string()) }) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { l_trail_comments(inp, n) }) })])
}

// [169] L-TRAIL-COMMENTS
fn l_trail_comments(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent_lt(inp, n) }), Box::new(move |inp: Input| -> Result { c_nb_comment_text(inp) }), Box::new(move |inp: Input| -> Result { b_comment(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_comment(inp) }) })])
}

// [170] C-L+LITERAL
fn c_lliteral(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 124) }), Box::new(move |inp: Input| -> Result { (|| -> Result { let r = alt(inp, vec![Box::new(move |inp: Input| -> Result { parse_int(inp, &|inp: Input| -> Result { ns_dec_digit(inp) }) }), Box::new(move |inp: Input| -> Result { detect_indent(inp, n) })]); if r.fail { return r; } let m: i32 = r.tag_int; let inp = r.rest; (|| -> Result { let r = alt(inp, vec![Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 45) }, "STRIP") }), Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 43) }, "KEEP") }), Box::new(move |inp: Input| -> Result { val(inp, "CLIP") })]); if r.fail { return r; } let t: String = r.tag.clone(); let inp = r.rest; seq(inp, vec![{ let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); s_b_comment(inp) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); l_literal_content(inp, (n + m), t.clone()) }) }]) })() })() })])
}

// [171] L-NB-LITERAL-TEXT
fn l_nb_literal_text(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_empty(inp, n, "BLOCK-IN".to_string()) }) }), Box::new(move |inp: Input| -> Result { s_indent(inp, n) }), Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { nb_char(inp) }) })])
}

// [172] B-NB-LITERAL-NEXT
fn b_nb_literal_next(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { b_as_line_feed(inp) }), Box::new(move |inp: Input| -> Result { l_nb_literal_text(inp, n) })])
}

// [173] L-LITERAL-CONTENT
fn l_literal_content(inp: Input, n: i32, t: String) -> Result {
    scalar(inp, &|inp: Input| -> Result { seq(inp, vec![{ let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); l_nb_literal_text(inp, n) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); star(inp, &|inp: Input| -> Result { b_nb_literal_next(inp, n) }) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); b_chomped_last(inp, t.clone()) }) }]) }) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); l_chomped_empty(inp, n, t.clone()) }) }]) })
}

// [174] C-L+FOLDED
fn c_lfolded(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 62) }), Box::new(move |inp: Input| -> Result { (|| -> Result { let r = alt(inp, vec![Box::new(move |inp: Input| -> Result { parse_int(inp, &|inp: Input| -> Result { ns_dec_digit(inp) }) }), Box::new(move |inp: Input| -> Result { detect_indent(inp, n) })]); if r.fail { return r; } let m: i32 = r.tag_int; let inp = r.rest; (|| -> Result { let r = alt(inp, vec![Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 45) }, "STRIP") }), Box::new(move |inp: Input| -> Result { parse_sym(inp, &|inp: Input| -> Result { match_cp(inp, 43) }, "KEEP") }), Box::new(move |inp: Input| -> Result { val(inp, "CLIP") })]); if r.fail { return r; } let t: String = r.tag.clone(); let inp = r.rest; seq(inp, vec![{ let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); s_b_comment(inp) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); l_folded_content(inp, (n + m), t.clone()) }) }]) })() })() })])
}

// [175] S-NB-FOLDED-TEXT
fn s_nb_folded_text(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, n) }), Box::new(move |inp: Input| -> Result { ns_char(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { nb_char(inp) }) })])
}

// [176] L-NB-FOLDED-LINES
fn l_nb_folded_lines(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_nb_folded_text(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { b_l_folded(inp, n, "BLOCK-IN".to_string()) }), Box::new(move |inp: Input| -> Result { s_nb_folded_text(inp, n) })]) }) })])
}

// [177] S-NB-SPACED-TEXT
fn s_nb_spaced_text(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, n) }), Box::new(move |inp: Input| -> Result { s_white(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { nb_char(inp) }) })])
}

// [178] B-L-SPACED
fn b_l_spaced(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { b_as_line_feed(inp) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_empty(inp, n, "BLOCK-IN".to_string()) }) })])
}

// [179] L-NB-SPACED-LINES
fn l_nb_spaced_lines(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_nb_spaced_text(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { b_l_spaced(inp, n) }), Box::new(move |inp: Input| -> Result { s_nb_spaced_text(inp, n) })]) }) })])
}

// [180] L-NB-SAME-LINES
fn l_nb_same_lines(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_empty(inp, n, "BLOCK-IN".to_string()) }) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { l_nb_folded_lines(inp, n) }), Box::new(move |inp: Input| -> Result { l_nb_spaced_lines(inp, n) })]) })])
}

// [181] L-NB-DIFF-LINES
fn l_nb_diff_lines(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { l_nb_same_lines(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { b_as_line_feed(inp) }), Box::new(move |inp: Input| -> Result { l_nb_same_lines(inp, n) })]) }) })])
}

// [182] L-FOLDED-CONTENT
fn l_folded_content(inp: Input, n: i32, t: String) -> Result {
    scalar(inp, &|inp: Input| -> Result { seq(inp, vec![{ let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); l_nb_diff_lines(inp, n) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); b_chomped_last(inp, t.clone()) }) }]) }) }) }, { let t_c = t.clone(); Box::new(move |inp: Input| -> Result { let t = t_c.clone(); l_chomped_empty(inp, n, t.clone()) }) }]) })
}

// [183] L+BLOCK-SEQUENCE
fn lblock_sequence(inp: Input, n: i32) -> Result {
    build(inp, "SEQUENCE", &|inp: Input| -> Result { (|| -> Result { let r = detect_indent(inp, n); if r.fail { return r; } let m: i32 = r.tag_int; let inp = r.rest; collect(inp, &|inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, (n + m)) }), Box::new(move |inp: Input| -> Result { c_l_block_seq_entry(inp, (n + m)) })]) }) }) })() })
}

// [184] C-L-BLOCK-SEQ-ENTRY
fn c_l_block_seq_entry(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 45) }), Box::new(move |inp: Input| -> Result { neg(inp, &|inp: Input| -> Result { ns_char(inp) }) }), Box::new(move |inp: Input| -> Result { s_lblock_indented(inp, n, "BLOCK-IN".to_string()) })])
}

// [185] S-L+BLOCK-INDENTED
fn s_lblock_indented(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); (|| -> Result { let r = detect_indent(inp, 0); if r.fail { return r; } let m: i32 = r.tag_int; let inp = r.rest; seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_indent(inp, m) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_l_compact_sequence(inp, (n + 1 + m)) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); ns_l_compact_mapping(inp, (n + 1 + m)) }) }]) }) }]) })() }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_lblock_node(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); e_node(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_l_comments(inp) }) }]) }) }])
}

// [186] NS-L-COMPACT-SEQUENCE
fn ns_l_compact_sequence(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { c_l_block_seq_entry(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, n) }), Box::new(move |inp: Input| -> Result { c_l_block_seq_entry(inp, n) })]) }) })])
}

// [187] L+BLOCK-MAPPING
fn lblock_mapping(inp: Input, n: i32) -> Result {
    build(inp, "MAPPING", &|inp: Input| -> Result { (|| -> Result { let r = detect_indent(inp, n); if r.fail { return r; } let m: i32 = r.tag_int; let inp = r.rest; collect(inp, &|inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, (n + m)) }), Box::new(move |inp: Input| -> Result { ns_l_block_map_entry(inp, (n + m)) })]) }) }) })() })
}

// [188] NS-L-BLOCK-MAP-ENTRY
fn ns_l_block_map_entry(inp: Input, n: i32) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_l_block_map_explicit_entry(inp, n) }), Box::new(move |inp: Input| -> Result { ns_l_block_map_implicit_entry(inp, n) })])
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY
fn c_l_block_map_explicit_entry(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { c_l_block_map_explicit_key(inp, n) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { l_block_map_explicit_value(inp, n) }), Box::new(move |inp: Input| -> Result { e_node(inp) })]) })])
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY
fn c_l_block_map_explicit_key(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 63) }), Box::new(move |inp: Input| -> Result { s_lblock_indented(inp, n, "BLOCK-OUT".to_string()) })])
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE
fn l_block_map_explicit_value(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, n) }), Box::new(move |inp: Input| -> Result { match_cp(inp, 58) }), Box::new(move |inp: Input| -> Result { s_lblock_indented(inp, n, "BLOCK-OUT".to_string()) })])
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY
fn ns_l_block_map_implicit_entry(inp: Input, n: i32) -> Result {
    build(inp, "PAIR", &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { scalar(inp, &|inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { ns_s_block_map_implicit_key(inp) }), Box::new(move |inp: Input| -> Result { e_node(inp) })]) }) }), Box::new(move |inp: Input| -> Result { c_l_block_map_implicit_value(inp, n) })]) })
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY
fn ns_s_block_map_implicit_key(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { c_s_implicit_json_key(inp, "BLOCK-KEY".to_string()) }), Box::new(move |inp: Input| -> Result { ns_s_implicit_yaml_key(inp, "BLOCK-KEY".to_string()) })])
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE
fn c_l_block_map_implicit_value(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { match_cp(inp, 58) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { s_lblock_node(inp, n, "BLOCK-OUT".to_string()) }), Box::new(move |inp: Input| -> Result { scalar(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { e_node(inp) }), Box::new(move |inp: Input| -> Result { s_l_comments(inp) })]) }) })]) })])
}

// [195] NS-L-COMPACT-MAPPING
fn ns_l_compact_mapping(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { ns_l_block_map_entry(inp, n) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { s_indent(inp, n) }), Box::new(move |inp: Input| -> Result { ns_l_block_map_entry(inp, n) })]) }) })])
}

// [196] S-L+BLOCK-NODE
fn s_lblock_node(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_lblock_in_block(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_lflow_in_block(inp, n) }) }])
}

// [197] S-L+FLOW-IN-BLOCK
fn s_lflow_in_block(inp: Input, n: i32) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { s_separate(inp, (n + 1), "FLOW-OUT".to_string()) }), Box::new(move |inp: Input| -> Result { ns_flow_node(inp, (n + 1), "FLOW-OUT".to_string()) }), Box::new(move |inp: Input| -> Result { s_l_comments(inp) })])
}

// [198] S-L+BLOCK-IN-BLOCK
fn s_lblock_in_block(inp: Input, n: i32, c: String) -> Result {
    alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_lblock_scalar(inp, n, c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_lblock_collection(inp, n, c.clone()) }) }])
}

// [199] S-L+BLOCK-SCALAR
fn s_lblock_scalar(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, (n + 1), c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_properties(inp, (n + 1), c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, (n + 1), c.clone()) }) }]) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_lliteral(inp, n) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_lfolded(inp, n) }) }]) }) }])
}

// [200] S-L+BLOCK-COLLECTION
fn s_lblock_collection(inp: Input, n: i32, c: String) -> Result {
    seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); opt(inp, &|inp: Input| -> Result { seq(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_separate(inp, (n + 1), c.clone()) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); c_ns_properties(inp, (n + 1), c.clone()) }) }]) }) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); s_l_comments(inp) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); alt(inp, vec![{ let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); lblock_sequence(inp, seq_spaces(n, &c.clone())) }) }, { let c_c = c.clone(); Box::new(move |inp: Input| -> Result { let c = c_c.clone(); lblock_mapping(inp, n) }) }]) }) }])
}

// [202] L-DOCUMENT-PREFIX
fn l_document_prefix(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { c_byte_order_mark(inp) }) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_comment(inp) }) })])
}

// [203] C-DIRECTIVES-END
fn c_directives_end(inp: Input) -> Result {
    match_str(inp, "---")
}

// [204] C-DOCUMENT-END
fn c_document_end(inp: Input) -> Result {
    match_str(inp, "...")
}

// [205] L-DOCUMENT-SUFFIX
fn l_document_suffix(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { c_document_end(inp) }), Box::new(move |inp: Input| -> Result { s_l_comments(inp) })])
}

// [206] C-FORBIDDEN
fn c_forbidden(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { sol(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { c_directives_end(inp) }), Box::new(move |inp: Input| -> Result { c_document_end(inp) })]) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { b_char(inp) }), Box::new(move |inp: Input| -> Result { s_white(inp) }), Box::new(move |inp: Input| -> Result { eof_ok(inp) })]) })])
}

// [207] L-BARE-DOCUMENT
fn l_bare_document(inp: Input) -> Result {
    build(inp, "DOC", &|inp: Input| -> Result { s_lblock_node(inp, -1, "BLOCK-IN".to_string()) })
}

// [208] L-EXPLICIT-DOCUMENT
fn l_explicit_document(inp: Input) -> Result {
    build(inp, "DOC", &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { c_directives_end(inp) }), Box::new(move |inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { l_bare_document(inp) }), Box::new(move |inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { e_node(inp) }), Box::new(move |inp: Input| -> Result { s_l_comments(inp) })]) })]) })]) })
}

// [209] L-DIRECTIVE-DOCUMENT
fn l_directive_document(inp: Input) -> Result {
    seq(inp, vec![Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { l_directive(inp) }) }), Box::new(move |inp: Input| -> Result { l_explicit_document(inp) })])
}

// [210] L-ANY-DOCUMENT
fn l_any_document(inp: Input) -> Result {
    alt(inp, vec![Box::new(move |inp: Input| -> Result { l_directive_document(inp) }), Box::new(move |inp: Input| -> Result { l_explicit_document(inp) }), Box::new(move |inp: Input| -> Result { l_bare_document(inp) })])
}

// [211] L-YAML-STREAM
fn l_yaml_stream(inp: Input) -> Result {
    build(inp, "STREAM", &|inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_document_prefix(inp) }) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { l_any_document(inp) }) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { alt(inp, vec![Box::new(move |inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { plus_(inp, &|inp: Input| -> Result { l_document_suffix(inp) }) }), Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_document_prefix(inp) }) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { l_any_document(inp) }) })]) }), Box::new(move |inp: Input| -> Result { seq(inp, vec![Box::new(move |inp: Input| -> Result { star(inp, &|inp: Input| -> Result { l_document_prefix(inp) }) }), Box::new(move |inp: Input| -> Result { opt(inp, &|inp: Input| -> Result { l_explicit_document(inp) }) })]) })]) }) })]) })
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
    let r = l_yaml_stream(inp);
    if !r.fail { println!("OK: {} chars", r.rest.pos); if let Some(a)=&r.ast { print_ast(a,0); } }
    else { eprintln!("FAIL @{}: {}", r.rest.pos, r.err); std::process::exit(1); }
}
