// ════════════════════════════════════════════════════════════════
import Foundation

typealias PFn = (Input) -> Result

// ── Input ──

struct Input {
    let src: String
    let chars: [Character]
    let pos: Int
    let line: Int
    let col: Int
    init(_ s: String, _ p: Int = 0, _ l: Int = 1, _ c: Int = 0) {
        src = s; chars = Array(s); pos = p; line = l; col = c
    }
    init(chars cs: [Character], src s: String, pos p: Int, line l: Int, col c: Int) {
        self.chars = cs; self.src = s; self.pos = p; self.line = l; self.col = c
    }
    func atEof() -> Bool { return pos >= chars.count }
    func peek() -> Int {
        if atEof() { return -1 }
        return Int(chars[pos].unicodeScalars.first!.value)
    }
    func adv() -> Input {
        if atEof() { return self }
        let c = chars[pos]
        let nl = c == "\n"
        return Input(chars: chars, src: src, pos: pos + 1, line: nl ? line + 1 : line, col: nl ? 0 : col + 1)
    }
}

// ── AST ──

class Ast {
    let tag: String
    let text: String
    var children: [Ast]
    let isLeaf: Bool
    init(tag: String) { self.tag = tag; self.text = ""; self.children = []; self.isLeaf = false }
    init(text: String) { self.tag = ""; self.text = text; self.children = []; self.isLeaf = true }
    static func branch(_ tag: String) -> Ast { return Ast(tag: tag) }
    static func leaf(_ text: String) -> Ast { return Ast(text: text) }
}

// ── Result ──

struct Result {
    var fail: Bool = false
    var val_: String = ""
    var rest: Input
    var tag: String = ""
    var tagInt: Int = 0
    var ast: Ast? = nil
    var astList: [Ast]? = nil
    var err: String = ""
}

func ok(_ inp: Input) -> Result { return Result(rest: inp) }
func okV(_ inp: Input, _ v: String) -> Result { return Result(val_: v, rest: inp) }
func fail(_ inp: Input, _ msg: String) -> Result { return Result(fail: true, rest: inp, err: msg) }

// ── Context ──

func inFlow(_ c: String) -> String { return (c == "FLOW-OUT" || c == "FLOW-IN") ? "FLOW-IN" : "FLOW-KEY" }
func seqSpaces(_ n: Int, _ c: String) -> Int { return c == "BLOCK-OUT" ? n - 1 : n }

// ── Combinators ──

func match_cp(_ inp: Input, _ cp: Int) -> Result {
    let c = inp.peek()
    if c == cp {
        let s = String(UnicodeScalar(c)!)
        var cur = inp; for _ in s.indices { cur = cur.adv() }
        return okV(cur, s)
    }
    return fail(inp, "cp")
}

func match_range(_ inp: Input, _ lo: Int, _ hi: Int) -> Result {
    let c = inp.peek()
    if c >= lo && c <= hi {
        let s = String(UnicodeScalar(c)!)
        var cur = inp; for _ in s.indices { cur = cur.adv() }
        return okV(cur, s)
    }
    return fail(inp, "rng")
}

func match_str(_ inp: Input, _ t: String) -> Result {
    let n = t.count
    if inp.pos + n > inp.chars.count { return fail(inp, "str") }
    let sub = String(inp.chars[inp.pos..<inp.pos+n])
    if sub != t { return fail(inp, "str") }
    var cur = inp; for _ in 0..<n { cur = cur.adv() }
    return okV(cur, t)
}

func mergeAsts(_ dst: inout [Ast], _ r: Result) {
    if let a = r.ast { dst.append(a) }
    if let al = r.astList { dst.append(contentsOf: al) }
}

func seq(_ inp: Input, _ fns: [PFn]) -> Result {
    var cur = inp; var acc = ""; var asts: [Ast] = []
    for f in fns { var r = f(cur); if r.fail { return r }; acc += r.val_; mergeAsts(&asts, r); cur = r.rest }
    var res = okV(cur, acc)
    if asts.count == 1 { res.ast = asts[0] } else if asts.count > 1 { res.astList = asts }
    return res
}

func alt(_ inp: Input, _ fns: [PFn]) -> Result {
    for f in fns { let r = f(inp); if !r.fail { return r } }
    return fail(inp, "alt")
}

func star(_ inp: Input, _ f: PFn) -> Result {
    var cur = inp; var acc = ""; var asts: [Ast] = []
    while true { let r = f(cur); if r.fail || r.rest.pos <= cur.pos { break }; acc += r.val_; mergeAsts(&asts, r); cur = r.rest }
    var res = okV(cur, acc)
    if !asts.isEmpty { res.astList = asts }
    return res
}

func plus_(_ inp: Input, _ f: PFn) -> Result {
    let first = f(inp); if first.fail { return first }
    let rest = star(first.rest, f)
    var res = okV(rest.rest, first.val_ + rest.val_)
    var asts: [Ast] = []; mergeAsts(&asts, first); mergeAsts(&asts, rest)
    if !asts.isEmpty { res.astList = asts }
    return res
}

func opt(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return ok(inp) }; return r }
func neg(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return ok(inp) }; return fail(inp, "neg") }
func minus(_ inp: Input, _ fa: PFn, _ fb: PFn) -> Result {
    let ra = fa(inp); if ra.fail { return ra }
    let rb = fb(inp); if !rb.fail && rb.rest.pos == ra.rest.pos { return fail(inp, "excl") }; return ra
}
func rep(_ inp: Input, _ n: Int, _ f: PFn) -> Result {
    var cur = inp; var acc = ""
    for _ in 0..<n { let r = f(cur); if r.fail { return r }; acc += r.val_; cur = r.rest }
    return okV(cur, acc)
}
func ahead(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return r }; return ok(inp) }
func behind(_ inp: Input, _ f: PFn) -> Result {
    if inp.pos == 0 { return fail(inp, "bh") }
    let t = Input(chars: inp.chars, src: inp.src, pos: inp.pos - 1, line: inp.line, col: max(0, inp.col - 1))
    let r = f(t); if r.fail { return fail(inp, "bh") }; return ok(inp)
}
func sol(_ inp: Input) -> Result { if inp.col == 0 { return ok(inp) }; return fail(inp, "sol") }
func eof_ok(_ inp: Input) -> Result { if inp.atEof() { return ok(inp) }; return fail(inp, "eof") }

// ── YAML extensions ──

func bindInt(_ r: Result, _ k: (Int, Input) -> Result) -> Result {
    if r.fail { return r }
    return k(r.tagInt, r.rest)
}

func bindCtx(_ r: Result, _ k: (String, Input) -> Result) -> Result {
    if r.fail { return r }
    return k(r.tag, r.rest)
}

func build(_ inp: Input, _ typ: String, _ f: PFn) -> Result {
    var r = f(inp); if r.fail { return r }
    let node = Ast.branch(typ)
    if let a = r.ast { node.children.append(a) }
    if let al = r.astList { node.children.append(contentsOf: al) }
    r.ast = node; r.astList = nil; return r
}

func scalar(_ inp: Input, _ f: PFn) -> Result {
    var r = f(inp); if r.fail { return r }
    r.ast = Ast.leaf(r.val_); return r
}

func collect(_ inp: Input, _ f: PFn) -> Result { return f(inp) }

func detect_indent(_ inp: Input, _ n: Int) -> Result {
    let s = inp.chars; let l = s.count; let i = inp.pos
    var sp = 0; while i + sp < l && s[i + sp] == " " { sp += 1 }
    if i + sp < l && s[i + sp] != "\n" { var r = ok(inp); r.tagInt = max(1, sp - n); return r }
    var j = i; while j < l && s[j] != "\n" { j += 1 }
    while j < l {
        if s[j] == "\n" { j += 1 }; if j >= l { break }
        sp = 0; while j + sp < l && s[j + sp] == " " { sp += 1 }
        let nx = j + sp; if nx >= l || s[nx] == "\n" { j = nx; continue }
        var r = ok(inp); r.tagInt = max(1, sp - n); return r
    }
    var r = ok(inp); r.tagInt = 1; return r
}

func parse_int(_ inp: Input, _ f: PFn) -> Result {
    var r = f(inp); if r.fail { return r }
    var v = 0; for ch in r.val_ { if ch >= "0" && ch <= "9" { v = v * 10 + Int(ch.asciiValue! - 48) } }
    r.tagInt = v; return r
}

func parse_sym(_ inp: Input, _ f: PFn, _ sym: String) -> Result {
    var r = f(inp); if r.fail { return r }; r.tag = sym; return r
}

func val(_ inp: Input, _ v: String) -> Result { var r = ok(inp); r.tag = v; return r }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] C-PRINTABLE 
func c_printable(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 0x9) },
        { (inp: Input) -> Result in return match_cp(inp, 0x0A) },
        { (inp: Input) -> Result in return match_cp(inp, 0x0D) },
        { (inp: Input) -> Result in return match_range(inp, 0x20, 0x7E) },
        { (inp: Input) -> Result in return match_cp(inp, 0x85) },
        { (inp: Input) -> Result in return match_range(inp, 0xA0, 0xD7FF) },
        { (inp: Input) -> Result in return match_range(inp, 0xE000, 0xFFFD) },
        { (inp: Input) -> Result in return match_range(inp, 0x10000, 0x10FFFF) }])
}

// [2] NB-JSON 
func nb_json(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 0x9) },
        { (inp: Input) -> Result in return match_range(inp, 0x20, 0x10FFFF) }])
}

// [3] C-BYTE-ORDER-MARK 
func c_byte_order_mark(_ inp: Input) -> Result {
    return match_cp(inp, 0xFEFF)
}

// [4] C-SEQUENCE-ENTRY 
func c_sequence_entry(_ inp: Input) -> Result {
    return match_cp(inp, 45)
}

// [5] C-MAPPING-KEY 
func c_mapping_key(_ inp: Input) -> Result {
    return match_cp(inp, 63)
}

// [6] C-MAPPING-VALUE 
func c_mapping_value(_ inp: Input) -> Result {
    return match_cp(inp, 58)
}

// [7] C-COLLECT-ENTRY 
func c_collect_entry(_ inp: Input) -> Result {
    return match_cp(inp, 44)
}

// [8] C-SEQUENCE-START 
func c_sequence_start(_ inp: Input) -> Result {
    return match_cp(inp, 91)
}

// [9] C-SEQUENCE-END 
func c_sequence_end(_ inp: Input) -> Result {
    return match_cp(inp, 93)
}

// [10] C-MAPPING-START 
func c_mapping_start(_ inp: Input) -> Result {
    return match_cp(inp, 123)
}

// [11] C-MAPPING-END 
func c_mapping_end(_ inp: Input) -> Result {
    return match_cp(inp, 125)
}

// [12] C-COMMENT 
func c_comment(_ inp: Input) -> Result {
    return match_cp(inp, 35)
}

// [13] C-ANCHOR 
func c_anchor(_ inp: Input) -> Result {
    return match_cp(inp, 38)
}

// [14] C-ALIAS 
func c_alias(_ inp: Input) -> Result {
    return match_cp(inp, 42)
}

// [15] C-TAG 
func c_tag(_ inp: Input) -> Result {
    return match_cp(inp, 33)
}

// [16] C-LITERAL 
func c_literal(_ inp: Input) -> Result {
    return match_cp(inp, 124)
}

// [17] C-FOLDED 
func c_folded(_ inp: Input) -> Result {
    return match_cp(inp, 62)
}

// [18] C-SINGLE-QUOTE 
func c_single_quote(_ inp: Input) -> Result {
    return match_cp(inp, 39)
}

// [19] C-DOUBLE-QUOTE 
func c_double_quote(_ inp: Input) -> Result {
    return match_cp(inp, 34)
}

// [20] C-DIRECTIVE 
func c_directive(_ inp: Input) -> Result {
    return match_cp(inp, 37)
}

// [21] C-RESERVED 
func c_reserved(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 64) },
        { (inp: Input) -> Result in return match_cp(inp, 96) }])
}

// [22] C-INDICATOR 
func c_indicator(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_sequence_entry(inp) },
        { (inp: Input) -> Result in return c_mapping_key(inp) },
        { (inp: Input) -> Result in return c_mapping_value(inp) },
        { (inp: Input) -> Result in return c_collect_entry(inp) },
        { (inp: Input) -> Result in return c_sequence_start(inp) },
        { (inp: Input) -> Result in return c_sequence_end(inp) },
        { (inp: Input) -> Result in return c_mapping_start(inp) },
        { (inp: Input) -> Result in return c_mapping_end(inp) },
        { (inp: Input) -> Result in return c_comment(inp) },
        { (inp: Input) -> Result in return c_anchor(inp) },
        { (inp: Input) -> Result in return c_alias(inp) },
        { (inp: Input) -> Result in return c_tag(inp) },
        { (inp: Input) -> Result in return c_literal(inp) },
        { (inp: Input) -> Result in return c_folded(inp) },
        { (inp: Input) -> Result in return c_single_quote(inp) },
        { (inp: Input) -> Result in return c_double_quote(inp) },
        { (inp: Input) -> Result in return c_directive(inp) },
        { (inp: Input) -> Result in return c_reserved(inp) }])
}

// [23] C-FLOW-INDICATOR 
func c_flow_indicator(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_collect_entry(inp) },
        { (inp: Input) -> Result in return c_sequence_start(inp) },
        { (inp: Input) -> Result in return c_sequence_end(inp) },
        { (inp: Input) -> Result in return c_mapping_start(inp) },
        { (inp: Input) -> Result in return c_mapping_end(inp) }])
}

// [24] B-LINE-FEED 
func b_line_feed(_ inp: Input) -> Result {
    return match_cp(inp, 0x0A)
}

// [25] B-CARRIAGE-RETURN 
func b_carriage_return(_ inp: Input) -> Result {
    return match_cp(inp, 0x0D)
}

// [26] B-CHAR 
func b_char(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return b_line_feed(inp) },
        { (inp: Input) -> Result in return b_carriage_return(inp) }])
}

// [27] NB-CHAR 
func nb_char(_ inp: Input) -> Result {
    return minus(inp, { (inp: Input) -> Result in return c_printable(inp) }, { (inp: Input) -> Result in return alt(inp, [
        { (inp: Input) -> Result in return b_char(inp) },
        { (inp: Input) -> Result in return c_byte_order_mark(inp) }]) })
}

// [28] B-BREAK 
func b_break(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return b_carriage_return(inp) },
            { (inp: Input) -> Result in return b_line_feed(inp) }]) },
        { (inp: Input) -> Result in return b_carriage_return(inp) },
        { (inp: Input) -> Result in return b_line_feed(inp) }])
}

// [29] B-AS-LINE-FEED 
func b_as_line_feed(_ inp: Input) -> Result {
    return b_break(inp)
}

// [30] B-NON-CONTENT 
func b_non_content(_ inp: Input) -> Result {
    return b_break(inp)
}

// [31] S-SPACE 
func s_space(_ inp: Input) -> Result {
    return match_cp(inp, 0x20)
}

// [32] S-TAB 
func s_tab(_ inp: Input) -> Result {
    return match_cp(inp, 0x9)
}

// [33] S-WHITE 
func s_white(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return s_space(inp) },
        { (inp: Input) -> Result in return s_tab(inp) }])
}

// [34] NS-CHAR 
func ns_char(_ inp: Input) -> Result {
    return minus(inp, { (inp: Input) -> Result in return nb_char(inp) }, { (inp: Input) -> Result in return s_white(inp) })
}

// [35] NS-DEC-DIGIT 
func ns_dec_digit(_ inp: Input) -> Result {
    return match_range(inp, 0x30, 0x39)
}

// [36] NS-HEX-DIGIT 
func ns_hex_digit(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return ns_dec_digit(inp) },
        { (inp: Input) -> Result in return match_range(inp, 0x41, 0x46) },
        { (inp: Input) -> Result in return match_range(inp, 0x61, 0x66) }])
}

// [37] NS-ASCII-LETTER 
func ns_ascii_letter(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return match_range(inp, 0x41, 0x5A) },
        { (inp: Input) -> Result in return match_range(inp, 0x61, 0x7A) }])
}

// [38] NS-WORD-CHAR 
func ns_word_char(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return ns_dec_digit(inp) },
        { (inp: Input) -> Result in return ns_ascii_letter(inp) },
        { (inp: Input) -> Result in return match_cp(inp, 45) }])
}

// [39] NS-URI-CHAR 
func ns_uri_char(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 37) },
            { (inp: Input) -> Result in return ns_hex_digit(inp) },
            { (inp: Input) -> Result in return ns_hex_digit(inp) }]) },
        { (inp: Input) -> Result in return ns_word_char(inp) },
        { (inp: Input) -> Result in return match_cp(inp, 35) },
        { (inp: Input) -> Result in return match_cp(inp, 59) },
        { (inp: Input) -> Result in return match_cp(inp, 47) },
        { (inp: Input) -> Result in return match_cp(inp, 63) },
        { (inp: Input) -> Result in return match_cp(inp, 58) },
        { (inp: Input) -> Result in return match_cp(inp, 64) },
        { (inp: Input) -> Result in return match_cp(inp, 38) },
        { (inp: Input) -> Result in return match_cp(inp, 61) },
        { (inp: Input) -> Result in return match_cp(inp, 43) },
        { (inp: Input) -> Result in return match_cp(inp, 36) },
        { (inp: Input) -> Result in return match_cp(inp, 44) },
        { (inp: Input) -> Result in return match_cp(inp, 95) },
        { (inp: Input) -> Result in return match_cp(inp, 46) },
        { (inp: Input) -> Result in return match_cp(inp, 33) },
        { (inp: Input) -> Result in return match_cp(inp, 126) },
        { (inp: Input) -> Result in return match_cp(inp, 42) },
        { (inp: Input) -> Result in return match_cp(inp, 39) },
        { (inp: Input) -> Result in return match_cp(inp, 40) },
        { (inp: Input) -> Result in return match_cp(inp, 41) },
        { (inp: Input) -> Result in return match_cp(inp, 91) },
        { (inp: Input) -> Result in return match_cp(inp, 93) }])
}

// [40] NS-TAG-CHAR 
func ns_tag_char(_ inp: Input) -> Result {
    return minus(inp, { (inp: Input) -> Result in return ns_uri_char(inp) }, { (inp: Input) -> Result in return alt(inp, [
        { (inp: Input) -> Result in return c_tag(inp) },
        { (inp: Input) -> Result in return c_flow_indicator(inp) }]) })
}

// [41] C-ESCAPE 
func c_escape(_ inp: Input) -> Result {
    return match_cp(inp, 92)
}

// [42] NS-ESC-NULL 
func ns_esc_null(_ inp: Input) -> Result {
    return match_cp(inp, 48)
}

// [43] NS-ESC-BELL 
func ns_esc_bell(_ inp: Input) -> Result {
    return match_cp(inp, 97)
}

// [44] NS-ESC-BACKSPACE 
func ns_esc_backspace(_ inp: Input) -> Result {
    return match_cp(inp, 98)
}

// [45] NS-ESC-HORIZONTAL-TAB 
func ns_esc_horizontal_tab(_ inp: Input) -> Result {
    return match_cp(inp, 116)
}

// [46] NS-ESC-LINE-FEED 
func ns_esc_line_feed(_ inp: Input) -> Result {
    return match_cp(inp, 110)
}

// [47] NS-ESC-VERTICAL-TAB 
func ns_esc_vertical_tab(_ inp: Input) -> Result {
    return match_cp(inp, 118)
}

// [48] NS-ESC-FORM-FEED 
func ns_esc_form_feed(_ inp: Input) -> Result {
    return match_cp(inp, 102)
}

// [49] NS-ESC-CARRIAGE-RETURN 
func ns_esc_carriage_return(_ inp: Input) -> Result {
    return match_cp(inp, 114)
}

// [50] NS-ESC-ESCAPE 
func ns_esc_escape(_ inp: Input) -> Result {
    return match_cp(inp, 101)
}

// [51] NS-ESC-SPACE 
func ns_esc_space(_ inp: Input) -> Result {
    return match_cp(inp, 0x20)
}

// [52] NS-ESC-DOUBLE-QUOTE 
func ns_esc_double_quote(_ inp: Input) -> Result {
    return match_cp(inp, 34)
}

// [53] NS-ESC-SLASH 
func ns_esc_slash(_ inp: Input) -> Result {
    return match_cp(inp, 47)
}

// [54] NS-ESC-BACKSLASH 
func ns_esc_backslash(_ inp: Input) -> Result {
    return match_cp(inp, 92)
}

// [55] NS-ESC-NEXT-LINE 
func ns_esc_next_line(_ inp: Input) -> Result {
    return match_cp(inp, 78)
}

// [56] NS-ESC-NON-BREAKING-SPACE 
func ns_esc_non_breaking_space(_ inp: Input) -> Result {
    return match_cp(inp, 95)
}

// [57] NS-ESC-LINE-SEPARATOR 
func ns_esc_line_separator(_ inp: Input) -> Result {
    return match_cp(inp, 76)
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR 
func ns_esc_paragraph_separator(_ inp: Input) -> Result {
    return match_cp(inp, 80)
}

// [59] NS-ESC-8-BIT 
func ns_esc_8_bit(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 120) },
        { (inp: Input) -> Result in return rep(inp, 2, { (inp: Input) -> Result in return ns_hex_digit(inp) }) }])
}

// [60] NS-ESC-16-BIT 
func ns_esc_16_bit(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 117) },
        { (inp: Input) -> Result in return rep(inp, 4, { (inp: Input) -> Result in return ns_hex_digit(inp) }) }])
}

// [61] NS-ESC-32-BIT 
func ns_esc_32_bit(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 85) },
        { (inp: Input) -> Result in return rep(inp, 8, { (inp: Input) -> Result in return ns_hex_digit(inp) }) }])
}

// [62] C-NS-ESC-CHAR 
func c_ns_esc_char(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_escape(inp) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return ns_esc_null(inp) },
            { (inp: Input) -> Result in return ns_esc_bell(inp) },
            { (inp: Input) -> Result in return ns_esc_backspace(inp) },
            { (inp: Input) -> Result in return ns_esc_horizontal_tab(inp) },
            { (inp: Input) -> Result in return ns_esc_line_feed(inp) },
            { (inp: Input) -> Result in return ns_esc_vertical_tab(inp) },
            { (inp: Input) -> Result in return ns_esc_form_feed(inp) },
            { (inp: Input) -> Result in return ns_esc_carriage_return(inp) },
            { (inp: Input) -> Result in return ns_esc_escape(inp) },
            { (inp: Input) -> Result in return ns_esc_space(inp) },
            { (inp: Input) -> Result in return ns_esc_double_quote(inp) },
            { (inp: Input) -> Result in return ns_esc_slash(inp) },
            { (inp: Input) -> Result in return ns_esc_backslash(inp) },
            { (inp: Input) -> Result in return ns_esc_next_line(inp) },
            { (inp: Input) -> Result in return ns_esc_non_breaking_space(inp) },
            { (inp: Input) -> Result in return ns_esc_line_separator(inp) },
            { (inp: Input) -> Result in return ns_esc_paragraph_separator(inp) },
            { (inp: Input) -> Result in return ns_esc_8_bit(inp) },
            { (inp: Input) -> Result in return ns_esc_16_bit(inp) },
            { (inp: Input) -> Result in return ns_esc_32_bit(inp) }]) }])
}

// [63] S-INDENT 
func s_indent(_ inp: Input, _ n: Int) -> Result {
    return rep(inp, n, { (inp: Input) -> Result in return s_space(inp) })
}

// [64] S-INDENT-LT 
func s_indent_lt(_ inp: Input, _ n: Int) -> Result {
    return star(inp, { (inp: Input) -> Result in return s_space(inp) })
}

// [65] S-INDENT-LE 
func s_indent_le(_ inp: Input, _ n: Int) -> Result {
    return star(inp, { (inp: Input) -> Result in return s_space(inp) })
}

// [66] S-SEPARATE-IN-LINE 
func s_separate_in_line(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return s_white(inp) }) },
        { (inp: Input) -> Result in return ok(inp) }])
}

// [67] S-LINE-PREFIX 
func s_line_prefix(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return { () -> Result in switch c {
        case "BLOCK-IN": return s_block_line_prefix(inp, n)
        case "BLOCK-OUT": return s_block_line_prefix(inp, n)
        case "FLOW-IN": return s_flow_line_prefix(inp, n)
        case "FLOW-OUT": return s_flow_line_prefix(inp, n)
        default: return fail(inp, "no case")
    } }()
}

// [68] S-BLOCK-LINE-PREFIX 
func s_block_line_prefix(_ inp: Input, _ n: Int) -> Result {
    return s_indent(inp, n)
}

// [69] S-FLOW-LINE-PREFIX 
func s_flow_line_prefix(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_indent(inp, n) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate_in_line(inp) }) }])
}

// [70] L-EMPTY 
func l_empty(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return s_line_prefix(inp, n, c) },
            { (inp: Input) -> Result in return s_indent_lt(inp, n) }]) },
        { (inp: Input) -> Result in return b_as_line_feed(inp) }])
}

// [71] B-L-TRIMMED 
func b_l_trimmed(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return b_non_content(inp) },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return l_empty(inp, n, c) }) }])
}

// [72] B-AS-SPACE 
func b_as_space(_ inp: Input) -> Result {
    return b_break(inp)
}

// [73] B-L-FOLDED 
func b_l_folded(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return b_l_trimmed(inp, n, c) },
        { (inp: Input) -> Result in return b_as_space(inp) }])
}

// [74] S-FLOW-FOLDED 
func s_flow_folded(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate_in_line(inp) }) },
        { (inp: Input) -> Result in return b_l_folded(inp, n, "FLOW-IN") },
        { (inp: Input) -> Result in return s_flow_line_prefix(inp, n) }])
}

// [75] C-NB-COMMENT-TEXT 
func c_nb_comment_text(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_comment(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return nb_char(inp) }) }])
}

// [76] B-COMMENT 
func b_comment(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return b_non_content(inp) },
        { (inp: Input) -> Result in return ok(inp) }])
}

// [77] S-B-COMMENT 
func s_b_comment(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return s_separate_in_line(inp) },
            { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return c_nb_comment_text(inp) }) }]) }) },
        { (inp: Input) -> Result in return b_comment(inp) }])
}

// [78] L-COMMENT 
func l_comment(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_separate_in_line(inp) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return c_nb_comment_text(inp) }) },
        { (inp: Input) -> Result in return b_non_content(inp) }])
}

// [79] S-L-COMMENTS 
func s_l_comments(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return s_b_comment(inp) },
            { (inp: Input) -> Result in return ok(inp) }]) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_comment(inp) }) }])
}

// [80] S-SEPARATE 
func s_separate(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return { () -> Result in switch c {
        case "BLOCK-OUT": return s_separate_lines(inp, n)
        case "BLOCK-IN": return s_separate_lines(inp, n)
        case "FLOW-OUT": return s_separate_lines(inp, n)
        case "FLOW-IN": return s_separate_lines(inp, n)
        case "BLOCK-KEY": return s_separate_in_line(inp)
        case "FLOW-KEY": return s_separate_in_line(inp)
        default: return fail(inp, "no case")
    } }()
}

// [81] S-SEPARATE-LINES 
func s_separate_lines(_ inp: Input, _ n: Int) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return s_l_comments(inp) },
            { (inp: Input) -> Result in return s_flow_line_prefix(inp, n) }]) },
        { (inp: Input) -> Result in return s_separate_in_line(inp) }])
}

// [82] L-DIRECTIVE 
func l_directive(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_directive(inp) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return ns_yaml_directive(inp) },
            { (inp: Input) -> Result in return ns_tag_directive(inp) },
            { (inp: Input) -> Result in return ns_reserved_directive(inp) }]) },
        { (inp: Input) -> Result in return s_l_comments(inp) }])
}

// [83] NS-RESERVED-DIRECTIVE 
func ns_reserved_directive(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_directive_name(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return s_separate_in_line(inp) },
            { (inp: Input) -> Result in return ns_directive_parameter(inp) }]) }) }])
}

// [84] NS-DIRECTIVE-NAME 
func ns_directive_name(_ inp: Input) -> Result {
    return plus_(inp, { (inp: Input) -> Result in return ns_char(inp) })
}

// [85] NS-DIRECTIVE-PARAMETER 
func ns_directive_parameter(_ inp: Input) -> Result {
    return plus_(inp, { (inp: Input) -> Result in return ns_char(inp) })
}

// [86] NS-YAML-DIRECTIVE 
func ns_yaml_directive(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_str(inp, "YAML") },
        { (inp: Input) -> Result in return s_separate_in_line(inp) },
        { (inp: Input) -> Result in return ns_yaml_version(inp) }])
}

// [87] NS-YAML-VERSION 
func ns_yaml_version(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return ns_dec_digit(inp) }) },
        { (inp: Input) -> Result in return match_cp(inp, 46) },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return ns_dec_digit(inp) }) }])
}

// [88] NS-TAG-DIRECTIVE 
func ns_tag_directive(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_str(inp, "TAG") },
        { (inp: Input) -> Result in return s_separate_in_line(inp) },
        { (inp: Input) -> Result in return c_tag_handle(inp) },
        { (inp: Input) -> Result in return s_separate_in_line(inp) },
        { (inp: Input) -> Result in return ns_tag_prefix(inp) }])
}

// [89] C-TAG-HANDLE 
func c_tag_handle(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_named_tag_handle(inp) },
        { (inp: Input) -> Result in return c_secondary_tag_handle(inp) },
        { (inp: Input) -> Result in return c_primary_tag_handle(inp) }])
}

// [90] C-PRIMARY-TAG-HANDLE 
func c_primary_tag_handle(_ inp: Input) -> Result {
    return match_cp(inp, 33)
}

// [91] C-SECONDARY-TAG-HANDLE 
func c_secondary_tag_handle(_ inp: Input) -> Result {
    return match_str(inp, "!!")
}

// [92] C-NAMED-TAG-HANDLE 
func c_named_tag_handle(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 33) },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return ns_word_char(inp) }) },
        { (inp: Input) -> Result in return match_cp(inp, 33) }])
}

// [93] NS-TAG-PREFIX 
func ns_tag_prefix(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_ns_local_tag_prefix(inp) },
        { (inp: Input) -> Result in return ns_global_tag_prefix(inp) }])
}

// [94] C-NS-LOCAL-TAG-PREFIX 
func c_ns_local_tag_prefix(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 33) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return ns_uri_char(inp) }) }])
}

// [95] NS-GLOBAL-TAG-PREFIX 
func ns_global_tag_prefix(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_tag_char(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return ns_uri_char(inp) }) }])
}

// [96] C-NS-PROPERTIES 
func c_ns_properties(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return c_ns_tag_property(inp) },
            { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return s_separate(inp, n, c) },
                { (inp: Input) -> Result in return c_ns_anchor_property(inp) }]) }) }]) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return c_ns_anchor_property(inp) },
            { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return s_separate(inp, n, c) },
                { (inp: Input) -> Result in return c_ns_tag_property(inp) }]) }) }]) }])
}

// [97] C-NS-TAG-PROPERTY 
func c_ns_tag_property(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_verbatim_tag(inp) },
        { (inp: Input) -> Result in return c_ns_shorthand_tag(inp) },
        { (inp: Input) -> Result in return c_non_specific_tag(inp) }])
}

// [98] C-VERBATIM-TAG 
func c_verbatim_tag(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_str(inp, "!<") },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return ns_uri_char(inp) }) },
        { (inp: Input) -> Result in return match_cp(inp, 62) }])
}

// [99] C-NS-SHORTHAND-TAG 
func c_ns_shorthand_tag(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_tag_handle(inp) },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return ns_tag_char(inp) }) }])
}

// [100] C-NON-SPECIFIC-TAG 
func c_non_specific_tag(_ inp: Input) -> Result {
    return match_cp(inp, 33)
}

// [101] C-NS-ANCHOR-PROPERTY 
func c_ns_anchor_property(_ inp: Input) -> Result {
    return build(inp, "ANCHOR", { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return c_anchor(inp) },
        { (inp: Input) -> Result in return scalar(inp, { (inp: Input) -> Result in return ns_anchor_name(inp) }) }]) })
}

// [102] NS-ANCHOR-CHAR 
func ns_anchor_char(_ inp: Input) -> Result {
    return minus(inp, { (inp: Input) -> Result in return ns_char(inp) }, { (inp: Input) -> Result in return c_flow_indicator(inp) })
}

// [103] NS-ANCHOR-NAME 
func ns_anchor_name(_ inp: Input) -> Result {
    return plus_(inp, { (inp: Input) -> Result in return ns_anchor_char(inp) })
}

// [104] C-NS-ALIAS-NODE 
func c_ns_alias_node(_ inp: Input) -> Result {
    return build(inp, "ALIAS", { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return c_alias(inp) },
        { (inp: Input) -> Result in return scalar(inp, { (inp: Input) -> Result in return ns_anchor_name(inp) }) }]) })
}

// [105] E-SCALAR 
func e_scalar(_ inp: Input) -> Result {
    return ok(inp)
}

// [106] E-NODE 
func e_node(_ inp: Input) -> Result {
    return e_scalar(inp)
}

// [107] NB-DOUBLE-CHAR 
func nb_double_char(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_ns_esc_char(inp) },
        { (inp: Input) -> Result in return minus(inp, { (inp: Input) -> Result in return nb_json(inp) }, { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 92) },
            { (inp: Input) -> Result in return match_cp(inp, 34) }]) }) }])
}

// [108] NS-DOUBLE-CHAR 
func ns_double_char(_ inp: Input) -> Result {
    return minus(inp, { (inp: Input) -> Result in return nb_double_char(inp) }, { (inp: Input) -> Result in return s_white(inp) })
}

// [109] C-DOUBLE-QUOTED 
func c_double_quoted(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return scalar(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 34) },
        { (inp: Input) -> Result in return nb_double_text(inp, n, c) },
        { (inp: Input) -> Result in return match_cp(inp, 34) }]) })
}

// [110] NB-DOUBLE-TEXT 
func nb_double_text(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return { () -> Result in switch c {
        case "FLOW-OUT": return nb_double_multi_line(inp, n)
        case "FLOW-IN": return nb_double_multi_line(inp, n)
        case "BLOCK-KEY": return nb_double_one_line(inp)
        case "FLOW-KEY": return nb_double_one_line(inp)
        default: return fail(inp, "no case")
    } }()
}

// [111] NB-DOUBLE-ONE-LINE 
func nb_double_one_line(_ inp: Input) -> Result {
    return star(inp, { (inp: Input) -> Result in return nb_double_char(inp) })
}

// [112] S-DOUBLE-ESCAPED 
func s_double_escaped(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) },
        { (inp: Input) -> Result in return match_cp(inp, 92) },
        { (inp: Input) -> Result in return b_non_content(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_empty(inp, n, "FLOW-IN") }) },
        { (inp: Input) -> Result in return s_flow_line_prefix(inp, n) }])
}

// [113] S-DOUBLE-BREAK 
func s_double_break(_ inp: Input, _ n: Int) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return s_double_escaped(inp, n) },
        { (inp: Input) -> Result in return s_flow_folded(inp, n) }])
}

// [114] NB-NS-DOUBLE-IN-LINE 
func nb_ns_double_in_line(_ inp: Input) -> Result {
    return star(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) },
        { (inp: Input) -> Result in return ns_double_char(inp) }]) })
}

// [115] S-DOUBLE-NEXT-LINE 
func s_double_next_line(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_double_break(inp, n) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return ns_double_char(inp) },
            { (inp: Input) -> Result in return nb_ns_double_in_line(inp) },
            { (inp: Input) -> Result in return alt(inp, [
                { (inp: Input) -> Result in return s_double_next_line(inp, n) },
                { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) }]) }]) }) }])
}

// [116] NB-DOUBLE-MULTI-LINE 
func nb_double_multi_line(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return nb_ns_double_in_line(inp) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return s_double_next_line(inp, n) },
            { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) }]) }])
}

// [117] C-QUOTED-QUOTE 
func c_quoted_quote(_ inp: Input) -> Result {
    return match_str(inp, "''")
}

// [118] NB-SINGLE-CHAR 
func nb_single_char(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_quoted_quote(inp) },
        { (inp: Input) -> Result in return minus(inp, { (inp: Input) -> Result in return nb_json(inp) }, { (inp: Input) -> Result in return match_cp(inp, 39) }) }])
}

// [119] NS-SINGLE-CHAR 
func ns_single_char(_ inp: Input) -> Result {
    return minus(inp, { (inp: Input) -> Result in return nb_single_char(inp) }, { (inp: Input) -> Result in return s_white(inp) })
}

// [120] C-SINGLE-QUOTED 
func c_single_quoted(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return scalar(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 39) },
        { (inp: Input) -> Result in return nb_single_text(inp, n, c) },
        { (inp: Input) -> Result in return match_cp(inp, 39) }]) })
}

// [121] NB-SINGLE-TEXT 
func nb_single_text(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return { () -> Result in switch c {
        case "FLOW-OUT": return nb_single_multi_line(inp, n)
        case "FLOW-IN": return nb_single_multi_line(inp, n)
        case "BLOCK-KEY": return nb_single_one_line(inp)
        case "FLOW-KEY": return nb_single_one_line(inp)
        default: return fail(inp, "no case")
    } }()
}

// [122] NB-SINGLE-ONE-LINE 
func nb_single_one_line(_ inp: Input) -> Result {
    return star(inp, { (inp: Input) -> Result in return nb_single_char(inp) })
}

// [123] NS-SINGLE-IN-LINE 
func ns_single_in_line(_ inp: Input) -> Result {
    return star(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) },
        { (inp: Input) -> Result in return ns_single_char(inp) }]) })
}

// [124] S-SINGLE-NEXT-LINE 
func s_single_next_line(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_flow_folded(inp, n) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return ns_single_char(inp) },
            { (inp: Input) -> Result in return ns_single_in_line(inp) },
            { (inp: Input) -> Result in return alt(inp, [
                { (inp: Input) -> Result in return s_single_next_line(inp, n) },
                { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) }]) }]) }) }])
}

// [125] NB-SINGLE-MULTI-LINE 
func nb_single_multi_line(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_single_in_line(inp) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return s_single_next_line(inp, n) },
            { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) }]) }])
}

// [126] NS-PLAIN-FIRST 
func ns_plain_first(_ inp: Input, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return minus(inp, { (inp: Input) -> Result in return ns_char(inp) }, { (inp: Input) -> Result in return c_indicator(inp) }) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return alt(inp, [
                { (inp: Input) -> Result in return match_cp(inp, 63) },
                { (inp: Input) -> Result in return match_cp(inp, 58) },
                { (inp: Input) -> Result in return match_cp(inp, 45) }]) },
            { (inp: Input) -> Result in return ahead(inp, { (inp: Input) -> Result in return ns_plain_safe(inp, c) }) }]) }])
}

// [127] NS-PLAIN-SAFE 
func ns_plain_safe(_ inp: Input, _ c: String) -> Result {
    return { () -> Result in switch c {
        case "FLOW-OUT": return ns_plain_safe_out(inp)
        case "FLOW-IN": return ns_plain_safe_in(inp)
        case "BLOCK-KEY": return ns_plain_safe_out(inp)
        case "FLOW-KEY": return ns_plain_safe_in(inp)
        default: return fail(inp, "no case")
    } }()
}

// [128] NS-PLAIN-SAFE-OUT 
func ns_plain_safe_out(_ inp: Input) -> Result {
    return ns_char(inp)
}

// [129] NS-PLAIN-SAFE-IN 
func ns_plain_safe_in(_ inp: Input) -> Result {
    return minus(inp, { (inp: Input) -> Result in return ns_char(inp) }, { (inp: Input) -> Result in return c_flow_indicator(inp) })
}

// [130] NS-PLAIN-CHAR 
func ns_plain_char(_ inp: Input, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return minus(inp, { (inp: Input) -> Result in return ns_plain_safe(inp, c) }, { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 58) },
            { (inp: Input) -> Result in return match_cp(inp, 35) }]) }) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return behind(inp, { (inp: Input) -> Result in return ns_char(inp) }) },
            { (inp: Input) -> Result in return match_cp(inp, 35) }]) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 58) },
            { (inp: Input) -> Result in return ahead(inp, { (inp: Input) -> Result in return ns_plain_safe(inp, c) }) }]) }])
}

// [131] NS-PLAIN 
func ns_plain(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return scalar(inp, { (inp: Input) -> Result in return { () -> Result in switch c {
        case "FLOW-OUT": return ns_plain_multi_line(inp, n, c)
        case "FLOW-IN": return ns_plain_multi_line(inp, n, c)
        case "BLOCK-KEY": return ns_plain_one_line(inp, c)
        case "FLOW-KEY": return ns_plain_one_line(inp, c)
        default: return fail(inp, "no case")
    } }() })
}

// [132] NB-NS-PLAIN-IN-LINE 
func nb_ns_plain_in_line(_ inp: Input, _ c: String) -> Result {
    return star(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_white(inp) }) },
        { (inp: Input) -> Result in return ns_plain_char(inp, c) }]) })
}

// [133] NS-PLAIN-ONE-LINE 
func ns_plain_one_line(_ inp: Input, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_plain_first(inp, c) },
        { (inp: Input) -> Result in return nb_ns_plain_in_line(inp, c) }])
}

// [134] S-NS-PLAIN-NEXT-LINE 
func s_ns_plain_next_line(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_flow_folded(inp, n) },
        { (inp: Input) -> Result in return neg(inp, { (inp: Input) -> Result in return c_forbidden(inp) }) },
        { (inp: Input) -> Result in return ns_plain_char(inp, c) },
        { (inp: Input) -> Result in return nb_ns_plain_in_line(inp, c) }])
}

// [135] NS-PLAIN-MULTI-LINE 
func ns_plain_multi_line(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_plain_one_line(inp, c) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return s_ns_plain_next_line(inp, n, c) }) }])
}

// [137] C-FLOW-SEQUENCE 
func c_flow_sequence(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return build(inp, "SEQUENCE", { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 91) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return collect(inp, { (inp: Input) -> Result in return ns_s_flow_seq_entries(inp, n, inFlow(c)) }) }) },
        { (inp: Input) -> Result in return match_cp(inp, 93) }]) })
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
func ns_s_flow_seq_entries(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_flow_seq_entry(inp, n, c) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 44) },
            { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
            { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return ns_s_flow_seq_entries(inp, n, c) }) }]) }) }])
}

// [139] NS-FLOW-SEQ-ENTRY 
func ns_flow_seq_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return ns_flow_pair(inp, n, c) },
        { (inp: Input) -> Result in return ns_flow_node(inp, n, c) }])
}

// [140] C-FLOW-MAPPING 
func c_flow_mapping(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return build(inp, "MAPPING", { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 123) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return collect(inp, { (inp: Input) -> Result in return ns_s_flow_map_entries(inp, n, inFlow(c)) }) }) },
        { (inp: Input) -> Result in return match_cp(inp, 125) }]) })
}

// [141] NS-S-FLOW-MAP-ENTRIES 
func ns_s_flow_map_entries(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_flow_map_entry(inp, n, c) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 44) },
            { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
            { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return ns_s_flow_map_entries(inp, n, c) }) }]) }) }])
}

// [142] NS-FLOW-MAP-ENTRY 
func ns_flow_map_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 63) },
            { (inp: Input) -> Result in return s_separate(inp, n, c) },
            { (inp: Input) -> Result in return ns_flow_map_explicit_entry(inp, n, c) }]) },
        { (inp: Input) -> Result in return ns_flow_map_implicit_entry(inp, n, c) }])
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
func ns_flow_map_explicit_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return ns_flow_map_implicit_entry(inp, n, c) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return e_node(inp) },
            { (inp: Input) -> Result in return e_node(inp) }]) }])
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
func ns_flow_map_implicit_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return build(inp, "PAIR", { (inp: Input) -> Result in return alt(inp, [
        { (inp: Input) -> Result in return ns_flow_map_yaml_key_entry(inp, n, c) },
        { (inp: Input) -> Result in return c_ns_flow_map_empty_key_entry(inp, n, c) },
        { (inp: Input) -> Result in return c_ns_flow_map_json_key_entry(inp, n, c) }]) })
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
func ns_flow_map_yaml_key_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_flow_yaml_node(inp, n, c) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
                { (inp: Input) -> Result in return c_ns_flow_map_separate_value(inp, n, c) }]) },
            { (inp: Input) -> Result in return e_node(inp) }]) }])
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
func c_ns_flow_map_empty_key_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return e_node(inp) },
        { (inp: Input) -> Result in return c_ns_flow_map_separate_value(inp, n, c) }])
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
func c_ns_flow_map_separate_value(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 58) },
        { (inp: Input) -> Result in return neg(inp, { (inp: Input) -> Result in return ns_plain_safe(inp, c) }) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return s_separate(inp, n, c) },
                { (inp: Input) -> Result in return ns_flow_node(inp, n, c) }]) },
            { (inp: Input) -> Result in return e_node(inp) }]) }])
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
func c_ns_flow_map_json_key_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_flow_json_node(inp, n, c) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
                { (inp: Input) -> Result in return c_ns_flow_map_adjacent_value(inp, n, c) }]) },
            { (inp: Input) -> Result in return e_node(inp) }]) }])
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
func c_ns_flow_map_adjacent_value(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 58) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate(inp, n, c) }) },
                { (inp: Input) -> Result in return ns_flow_node(inp, n, c) }]) },
            { (inp: Input) -> Result in return e_node(inp) }]) }])
}

// [150] NS-FLOW-PAIR 
func ns_flow_pair(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 63) },
            { (inp: Input) -> Result in return s_separate(inp, n, c) },
            { (inp: Input) -> Result in return ns_flow_map_explicit_entry(inp, n, c) }]) },
        { (inp: Input) -> Result in return ns_flow_pair_entry(inp, n, c) }])
}

// [151] NS-FLOW-PAIR-ENTRY 
func ns_flow_pair_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return ns_flow_pair_yaml_key_entry(inp, n, c) },
        { (inp: Input) -> Result in return c_ns_flow_map_empty_key_entry(inp, n, c) },
        { (inp: Input) -> Result in return c_ns_flow_pair_json_key_entry(inp, n, c) }])
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
func ns_flow_pair_yaml_key_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_s_implicit_yaml_key(inp, "FLOW-KEY") },
        { (inp: Input) -> Result in return c_ns_flow_map_separate_value(inp, n, c) }])
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
func c_ns_flow_pair_json_key_entry(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_s_implicit_json_key(inp, "FLOW-KEY") },
        { (inp: Input) -> Result in return c_ns_flow_map_adjacent_value(inp, n, c) }])
}

// [154] NS-S-IMPLICIT-YAML-KEY 
func ns_s_implicit_yaml_key(_ inp: Input, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_flow_yaml_node(inp, 0, c) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate_in_line(inp) }) }])
}

// [155] C-S-IMPLICIT-JSON-KEY 
func c_s_implicit_json_key(_ inp: Input, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_flow_json_node(inp, 0, c) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return s_separate_in_line(inp) }) }])
}

// [156] NS-FLOW-YAML-CONTENT 
func ns_flow_yaml_content(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return ns_plain(inp, n, c)
}

// [157] C-FLOW-JSON-CONTENT 
func c_flow_json_content(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_flow_sequence(inp, n, c) },
        { (inp: Input) -> Result in return c_flow_mapping(inp, n, c) },
        { (inp: Input) -> Result in return c_single_quoted(inp, n, c) },
        { (inp: Input) -> Result in return c_double_quoted(inp, n, c) }])
}

// [158] NS-FLOW-CONTENT 
func ns_flow_content(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return ns_flow_yaml_content(inp, n, c) },
        { (inp: Input) -> Result in return c_flow_json_content(inp, n, c) }])
}

// [159] NS-FLOW-YAML-NODE 
func ns_flow_yaml_node(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_ns_alias_node(inp) },
        { (inp: Input) -> Result in return ns_flow_yaml_content(inp, n, c) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return c_ns_properties(inp, n, c) },
            { (inp: Input) -> Result in return alt(inp, [
                { (inp: Input) -> Result in return seq(inp, [
                    { (inp: Input) -> Result in return s_separate(inp, n, c) },
                    { (inp: Input) -> Result in return ns_flow_yaml_content(inp, n, c) }]) },
                { (inp: Input) -> Result in return e_scalar(inp) }]) }]) }])
}

// [160] C-FLOW-JSON-NODE 
func c_flow_json_node(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return c_ns_properties(inp, n, c) },
            { (inp: Input) -> Result in return s_separate(inp, n, c) }]) }) },
        { (inp: Input) -> Result in return c_flow_json_content(inp, n, c) }])
}

// [161] NS-FLOW-NODE 
func ns_flow_node(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_ns_alias_node(inp) },
        { (inp: Input) -> Result in return ns_flow_content(inp, n, c) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return c_ns_properties(inp, n, c) },
            { (inp: Input) -> Result in return alt(inp, [
                { (inp: Input) -> Result in return seq(inp, [
                    { (inp: Input) -> Result in return s_separate(inp, n, c) },
                    { (inp: Input) -> Result in return ns_flow_content(inp, n, c) }]) },
                { (inp: Input) -> Result in return e_scalar(inp) }]) }]) }])
}

// [162] C-B-BLOCK-HEADER 
func c_b_block_header(_ inp: Input, _ n: Int) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return bindInt(alt(inp, [
            { (inp: Input) -> Result in return parse_int(inp, { (inp: Input) -> Result in return ns_dec_digit(inp) }) },
            { (inp: Input) -> Result in return detect_indent(inp, n) }]), { m, inp in bindCtx(alt(inp, [
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 45) }, "STRIP") },
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 43) }, "KEEP") },
            { (inp: Input) -> Result in return val(inp, "CLIP") }]), { t, inp in s_b_comment(inp) }) }) },
        { (inp: Input) -> Result in return bindCtx(alt(inp, [
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 45) }, "STRIP") },
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 43) }, "KEEP") },
            { (inp: Input) -> Result in return val(inp, "CLIP") }]), { t, inp in bindInt(alt(inp, [
            { (inp: Input) -> Result in return parse_int(inp, { (inp: Input) -> Result in return ns_dec_digit(inp) }) },
            { (inp: Input) -> Result in return detect_indent(inp, n) }]), { m, inp in s_b_comment(inp) }) }) }])
}

// [163] C-INDENTATION-INDICATOR 
func c_indentation_indicator(_ inp: Input, _ n: Int) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return ns_dec_digit(inp) },
        { (inp: Input) -> Result in return ok(inp) }])
}

// [164] C-CHOMPING-INDICATOR 
func c_chomping_indicator(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 45) },
        { (inp: Input) -> Result in return match_cp(inp, 43) },
        { (inp: Input) -> Result in return ok(inp) }])
}

// [165] B-CHOMPED-LAST 
func b_chomped_last(_ inp: Input, _ t: String) -> Result {
    return { () -> Result in switch t {
        case "STRIP": return b_non_content(inp)
        case "CLIP": return b_as_line_feed(inp)
        case "KEEP": return b_as_line_feed(inp)
        default: return fail(inp, "no case")
    } }()
}

// [166] L-CHOMPED-EMPTY 
func l_chomped_empty(_ inp: Input, _ n: Int, _ t: String) -> Result {
    return { () -> Result in switch t {
        case "STRIP": return l_strip_empty(inp, n)
        case "CLIP": return l_strip_empty(inp, n)
        case "KEEP": return l_keep_empty(inp, n)
        default: return fail(inp, "no case")
    } }()
}

// [167] L-STRIP-EMPTY 
func l_strip_empty(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return s_indent_le(inp, n) },
            { (inp: Input) -> Result in return b_non_content(inp) }]) }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return l_trail_comments(inp, n) }) }])
}

// [168] L-KEEP-EMPTY 
func l_keep_empty(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_empty(inp, n, "BLOCK-IN") }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return l_trail_comments(inp, n) }) }])
}

// [169] L-TRAIL-COMMENTS 
func l_trail_comments(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_indent_lt(inp, n) },
        { (inp: Input) -> Result in return c_nb_comment_text(inp) },
        { (inp: Input) -> Result in return b_comment(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_comment(inp) }) }])
}

// [170] C-L+LITERAL 
func c_lliteral(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 124) },
        { (inp: Input) -> Result in return bindInt(alt(inp, [
            { (inp: Input) -> Result in return parse_int(inp, { (inp: Input) -> Result in return ns_dec_digit(inp) }) },
            { (inp: Input) -> Result in return detect_indent(inp, n) }]), { m, inp in bindCtx(alt(inp, [
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 45) }, "STRIP") },
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 43) }, "KEEP") },
            { (inp: Input) -> Result in return val(inp, "CLIP") }]), { t, inp in seq(inp, [
            { (inp: Input) -> Result in return s_b_comment(inp) },
            { (inp: Input) -> Result in return l_literal_content(inp, (n + m), t) }]) }) }) }])
}

// [171] L-NB-LITERAL-TEXT 
func l_nb_literal_text(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_empty(inp, n, "BLOCK-IN") }) },
        { (inp: Input) -> Result in return s_indent(inp, n) },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return nb_char(inp) }) }])
}

// [172] B-NB-LITERAL-NEXT 
func b_nb_literal_next(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return b_as_line_feed(inp) },
        { (inp: Input) -> Result in return l_nb_literal_text(inp, n) }])
}

// [173] L-LITERAL-CONTENT 
func l_literal_content(_ inp: Input, _ n: Int, _ t: String) -> Result {
    return scalar(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return l_nb_literal_text(inp, n) },
            { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return b_nb_literal_next(inp, n) }) },
            { (inp: Input) -> Result in return b_chomped_last(inp, t) }]) }) },
        { (inp: Input) -> Result in return l_chomped_empty(inp, n, t) }]) })
}

// [174] C-L+FOLDED 
func c_lfolded(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 62) },
        { (inp: Input) -> Result in return bindInt(alt(inp, [
            { (inp: Input) -> Result in return parse_int(inp, { (inp: Input) -> Result in return ns_dec_digit(inp) }) },
            { (inp: Input) -> Result in return detect_indent(inp, n) }]), { m, inp in bindCtx(alt(inp, [
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 45) }, "STRIP") },
            { (inp: Input) -> Result in return parse_sym(inp, { (inp: Input) -> Result in return match_cp(inp, 43) }, "KEEP") },
            { (inp: Input) -> Result in return val(inp, "CLIP") }]), { t, inp in seq(inp, [
            { (inp: Input) -> Result in return s_b_comment(inp) },
            { (inp: Input) -> Result in return l_folded_content(inp, (n + m), t) }]) }) }) }])
}

// [175] S-NB-FOLDED-TEXT 
func s_nb_folded_text(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_indent(inp, n) },
        { (inp: Input) -> Result in return ns_char(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return nb_char(inp) }) }])
}

// [176] L-NB-FOLDED-LINES 
func l_nb_folded_lines(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_nb_folded_text(inp, n) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return b_l_folded(inp, n, "BLOCK-IN") },
            { (inp: Input) -> Result in return s_nb_folded_text(inp, n) }]) }) }])
}

// [177] S-NB-SPACED-TEXT 
func s_nb_spaced_text(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_indent(inp, n) },
        { (inp: Input) -> Result in return s_white(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return nb_char(inp) }) }])
}

// [178] B-L-SPACED 
func b_l_spaced(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return b_as_line_feed(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_empty(inp, n, "BLOCK-IN") }) }])
}

// [179] L-NB-SPACED-LINES 
func l_nb_spaced_lines(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_nb_spaced_text(inp, n) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return b_l_spaced(inp, n) },
            { (inp: Input) -> Result in return s_nb_spaced_text(inp, n) }]) }) }])
}

// [180] L-NB-SAME-LINES 
func l_nb_same_lines(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_empty(inp, n, "BLOCK-IN") }) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return l_nb_folded_lines(inp, n) },
            { (inp: Input) -> Result in return l_nb_spaced_lines(inp, n) }]) }])
}

// [181] L-NB-DIFF-LINES 
func l_nb_diff_lines(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return l_nb_same_lines(inp, n) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return b_as_line_feed(inp) },
            { (inp: Input) -> Result in return l_nb_same_lines(inp, n) }]) }) }])
}

// [182] L-FOLDED-CONTENT 
func l_folded_content(_ inp: Input, _ n: Int, _ t: String) -> Result {
    return scalar(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return l_nb_diff_lines(inp, n) },
            { (inp: Input) -> Result in return b_chomped_last(inp, t) }]) }) },
        { (inp: Input) -> Result in return l_chomped_empty(inp, n, t) }]) })
}

// [183] L+BLOCK-SEQUENCE 
func lblock_sequence(_ inp: Input, _ n: Int) -> Result {
    return build(inp, "SEQUENCE", { (inp: Input) -> Result in return bindInt(detect_indent(inp, n), { m, inp in collect(inp, { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return s_indent(inp, (n + m)) },
        { (inp: Input) -> Result in return c_l_block_seq_entry(inp, (n + m)) }]) }) }) }) })
}

// [184] C-L-BLOCK-SEQ-ENTRY 
func c_l_block_seq_entry(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 45) },
        { (inp: Input) -> Result in return neg(inp, { (inp: Input) -> Result in return ns_char(inp) }) },
        { (inp: Input) -> Result in return s_lblock_indented(inp, n, "BLOCK-IN") }])
}

// [185] S-L+BLOCK-INDENTED 
func s_lblock_indented(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return bindInt(detect_indent(inp, 0), { m, inp in seq(inp, [
            { (inp: Input) -> Result in return s_indent(inp, m) },
            { (inp: Input) -> Result in return alt(inp, [
                { (inp: Input) -> Result in return ns_l_compact_sequence(inp, (n + 1 + m)) },
                { (inp: Input) -> Result in return ns_l_compact_mapping(inp, (n + 1 + m)) }]) }]) }) },
        { (inp: Input) -> Result in return s_lblock_node(inp, n, c) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return e_node(inp) },
            { (inp: Input) -> Result in return s_l_comments(inp) }]) }])
}

// [186] NS-L-COMPACT-SEQUENCE 
func ns_l_compact_sequence(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_l_block_seq_entry(inp, n) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return s_indent(inp, n) },
            { (inp: Input) -> Result in return c_l_block_seq_entry(inp, n) }]) }) }])
}

// [187] L+BLOCK-MAPPING 
func lblock_mapping(_ inp: Input, _ n: Int) -> Result {
    return build(inp, "MAPPING", { (inp: Input) -> Result in return bindInt(detect_indent(inp, n), { m, inp in collect(inp, { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return s_indent(inp, (n + m)) },
        { (inp: Input) -> Result in return ns_l_block_map_entry(inp, (n + m)) }]) }) }) }) })
}

// [188] NS-L-BLOCK-MAP-ENTRY 
func ns_l_block_map_entry(_ inp: Input, _ n: Int) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_l_block_map_explicit_entry(inp, n) },
        { (inp: Input) -> Result in return ns_l_block_map_implicit_entry(inp, n) }])
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
func c_l_block_map_explicit_entry(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_l_block_map_explicit_key(inp, n) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return l_block_map_explicit_value(inp, n) },
            { (inp: Input) -> Result in return e_node(inp) }]) }])
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
func c_l_block_map_explicit_key(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 63) },
        { (inp: Input) -> Result in return s_lblock_indented(inp, n, "BLOCK-OUT") }])
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
func l_block_map_explicit_value(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_indent(inp, n) },
        { (inp: Input) -> Result in return match_cp(inp, 58) },
        { (inp: Input) -> Result in return s_lblock_indented(inp, n, "BLOCK-OUT") }])
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
func ns_l_block_map_implicit_entry(_ inp: Input, _ n: Int) -> Result {
    return build(inp, "PAIR", { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return scalar(inp, { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return ns_s_block_map_implicit_key(inp) },
            { (inp: Input) -> Result in return e_node(inp) }]) }) },
        { (inp: Input) -> Result in return c_l_block_map_implicit_value(inp, n) }]) })
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
func ns_s_block_map_implicit_key(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return c_s_implicit_json_key(inp, "BLOCK-KEY") },
        { (inp: Input) -> Result in return ns_s_implicit_yaml_key(inp, "BLOCK-KEY") }])
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
func c_l_block_map_implicit_value(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 58) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return s_lblock_node(inp, n, "BLOCK-OUT") },
            { (inp: Input) -> Result in return scalar(inp, { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return e_node(inp) },
                { (inp: Input) -> Result in return s_l_comments(inp) }]) }) }]) }])
}

// [195] NS-L-COMPACT-MAPPING 
func ns_l_compact_mapping(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ns_l_block_map_entry(inp, n) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return s_indent(inp, n) },
            { (inp: Input) -> Result in return ns_l_block_map_entry(inp, n) }]) }) }])
}

// [196] S-L+BLOCK-NODE 
func s_lblock_node(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return s_lblock_in_block(inp, n, c) },
        { (inp: Input) -> Result in return s_lflow_in_block(inp, n) }])
}

// [197] S-L+FLOW-IN-BLOCK 
func s_lflow_in_block(_ inp: Input, _ n: Int) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_separate(inp, (n + 1), "FLOW-OUT") },
        { (inp: Input) -> Result in return ns_flow_node(inp, (n + 1), "FLOW-OUT") },
        { (inp: Input) -> Result in return s_l_comments(inp) }])
}

// [198] S-L+BLOCK-IN-BLOCK 
func s_lblock_in_block(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return s_lblock_scalar(inp, n, c) },
        { (inp: Input) -> Result in return s_lblock_collection(inp, n, c) }])
}

// [199] S-L+BLOCK-SCALAR 
func s_lblock_scalar(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return s_separate(inp, (n + 1), c) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return c_ns_properties(inp, (n + 1), c) },
            { (inp: Input) -> Result in return s_separate(inp, (n + 1), c) }]) }) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return c_lliteral(inp, n) },
            { (inp: Input) -> Result in return c_lfolded(inp, n) }]) }])
}

// [200] S-L+BLOCK-COLLECTION 
func s_lblock_collection(_ inp: Input, _ n: Int, _ c: String) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return s_separate(inp, (n + 1), c) },
            { (inp: Input) -> Result in return c_ns_properties(inp, (n + 1), c) }]) }) },
        { (inp: Input) -> Result in return s_l_comments(inp) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return lblock_sequence(inp, seqSpaces(n, c)) },
            { (inp: Input) -> Result in return lblock_mapping(inp, n) }]) }])
}

// [202] L-DOCUMENT-PREFIX 
func l_document_prefix(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return c_byte_order_mark(inp) }) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_comment(inp) }) }])
}

// [203] C-DIRECTIVES-END 
func c_directives_end(_ inp: Input) -> Result {
    return match_str(inp, "---")
}

// [204] C-DOCUMENT-END 
func c_document_end(_ inp: Input) -> Result {
    return match_str(inp, "...")
}

// [205] L-DOCUMENT-SUFFIX 
func l_document_suffix(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return c_document_end(inp) },
        { (inp: Input) -> Result in return s_l_comments(inp) }])
}

// [206] C-FORBIDDEN 
func c_forbidden(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return sol(inp) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return c_directives_end(inp) },
            { (inp: Input) -> Result in return c_document_end(inp) }]) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return b_char(inp) },
            { (inp: Input) -> Result in return s_white(inp) },
            { (inp: Input) -> Result in return eof_ok(inp) }]) }])
}

// [207] L-BARE-DOCUMENT 
func l_bare_document(_ inp: Input) -> Result {
    return build(inp, "DOC", { (inp: Input) -> Result in return s_lblock_node(inp, -1, "BLOCK-IN") })
}

// [208] L-EXPLICIT-DOCUMENT 
func l_explicit_document(_ inp: Input) -> Result {
    return build(inp, "DOC", { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return c_directives_end(inp) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return l_bare_document(inp) },
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return e_node(inp) },
                { (inp: Input) -> Result in return s_l_comments(inp) }]) }]) }]) })
}

// [209] L-DIRECTIVE-DOCUMENT 
func l_directive_document(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return l_directive(inp) }) },
        { (inp: Input) -> Result in return l_explicit_document(inp) }])
}

// [210] L-ANY-DOCUMENT 
func l_any_document(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return l_directive_document(inp) },
        { (inp: Input) -> Result in return l_explicit_document(inp) },
        { (inp: Input) -> Result in return l_bare_document(inp) }])
}

// [211] L-YAML-STREAM 
func l_yaml_stream(_ inp: Input) -> Result {
    return build(inp, "STREAM", { (inp: Input) -> Result in return seq(inp, [
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_document_prefix(inp) }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return l_any_document(inp) }) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return l_document_suffix(inp) }) },
                { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_document_prefix(inp) }) },
                { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return l_any_document(inp) }) }]) },
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return l_document_prefix(inp) }) },
                { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return l_explicit_document(inp) }) }]) }]) }) }]) })
}

// ── API ──

func printAst(_ node: Ast, _ depth: Int) {
    let indent = String(repeating: "  ", count: depth)
    if node.isLeaf {
        print("\(indent)SCALAR: \"\(node.text)\"")
    } else {
        print("\(indent)\(node.tag)")
        for c in node.children { printAst(c, depth + 1) }
    }
}

// ── Main ──

func runMain() {
    let text: String
    if CommandLine.arguments.count > 1 {
        text = try! String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    } else {
        text = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
    }
    let inp = Input(text)
    let r = l_yaml_stream(inp)
    if !r.fail {
        print("OK: \(r.rest.pos) chars")
        if let ast = r.ast { printAst(ast, 0) }
    } else {
        FileHandle.standardError.write("FAIL @\(r.rest.pos): \(r.err)\n".data(using: .utf8)!)
        _exit(1)
    }
    _exit(0)
}

// Run with 64MB stack to handle deep PEG recursion
let thread = Thread { runMain() }
thread.stackSize = 64 * 1024 * 1024
thread.start()
dispatchMain()
