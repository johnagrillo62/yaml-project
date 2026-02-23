// ════════════════════════════════════════════════════════════════
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

public class YamlReader {

// ── Input ──

struct Input {
    public string src;
    public int pos, line, col;
    public Input(string s, int p, int l, int c) { src=s; pos=p; line=l; col=c; }
}

static bool AtEof(Input i) => i.pos >= i.src.Length;
static int PeekCp(Input i) {
    if (AtEof(i)) return -1;
    return char.ConvertToUtf32(i.src, i.pos);
}
static Input Adv(Input i) {
    if (AtEof(i)) return i;
    char c = i.src[i.pos];
    bool nl = c == '\n';
    int len = char.IsHighSurrogate(c) ? 2 : 1;
    return new Input(i.src, i.pos + len, nl ? i.line + 1 : i.line, nl ? 0 : i.col + 1);
}

// ── AST ──

class Ast {
    public string tag;
    public string text;
    public List<Ast> children = new List<Ast>();
    public bool isLeaf;
    public static Ast Branch(string t) => new Ast { tag = t };
    public static Ast Leaf(string t) => new Ast { text = t, isLeaf = true, tag = "SCALAR" };
}

// ── Result ──

class Result {
    public bool failed;
    public string val = "";
    public Input rest;
    public string tag = "";
    public int tagInt;
    public Ast ast;
    public List<Ast> astList;
    public string err = "";
}

static Result Ok(Input inp) => new Result { rest = inp };
static Result OkV(Input inp, string v) => new Result { val = v, rest = inp };
static Result Fail(Input inp, string m) => new Result { failed = true, rest = inp, err = m };

// ── Context ──

static string InFlow(string c) => (c == "FLOW-OUT" || c == "FLOW-IN") ? "FLOW-IN" : "FLOW-KEY";
static int SeqSpaces(int n, string c) => c == "BLOCK-OUT" ? n - 1 : n;

// ── Combinators ──

delegate Result PFn(Input inp);

static Result match_cp(Input inp, int cp) {
    int c = PeekCp(inp);
    if (c == cp) {
        string s = char.ConvertFromUtf32(c);
        Input cur = inp;
        for (int i = 0; i < s.Length; i++) cur = Adv(cur);
        return OkV(cur, s);
    }
    return Fail(inp, "cp");
}

static Result match_range(Input inp, int lo, int hi) {
    int c = PeekCp(inp);
    if (c >= lo && c <= hi) {
        string s = char.ConvertFromUtf32(c);
        Input cur = inp;
        for (int i = 0; i < s.Length; i++) cur = Adv(cur);
        return OkV(cur, s);
    }
    return Fail(inp, "rng");
}

static Result match_str(Input inp, string t) {
    if (inp.pos + t.Length > inp.src.Length) return Fail(inp, "str");
    if (inp.src.Substring(inp.pos, t.Length) != t) return Fail(inp, "str");
    Input cur = inp;
    for (int i = 0; i < t.Length; i++) cur = Adv(cur);
    return OkV(cur, t);
}

static void MergeAsts(List<Ast> dst, Result r) {
    if (r.ast != null) dst.Add(r.ast);
    if (r.astList != null) dst.AddRange(r.astList);
}

static Result Seq(Input inp, PFn[] fns) {
    Input cur = inp; var acc = new StringBuilder(); var asts = new List<Ast>();
    foreach (var f in fns) {
        var r = f(cur); if (r.failed) return r;
        acc.Append(r.val); MergeAsts(asts, r); cur = r.rest;
    }
    var res = OkV(cur, acc.ToString());
    if (asts.Count == 1) res.ast = asts[0]; else if (asts.Count > 1) res.astList = asts;
    return res;
}

static Result Alt(Input inp, PFn[] fns) {
    foreach (var f in fns) { var r = f(inp); if (!r.failed) return r; }
    return Fail(inp, "alt");
}

static Result star(Input inp, PFn f) {
    Input cur = inp; var acc = new StringBuilder(); var asts = new List<Ast>();
    for (;;) {
        var r = f(cur); if (r.failed || r.rest.pos <= cur.pos) break;
        acc.Append(r.val); MergeAsts(asts, r); cur = r.rest;
    }
    var res = OkV(cur, acc.ToString());
    if (asts.Count > 0) res.astList = asts;
    return res;
}

static Result plus_(Input inp, PFn f) {
    var first = f(inp); if (first.failed) return first;
    var rest_ = star(first.rest, f);
    var res = OkV(rest_.rest, first.val + rest_.val);
    var asts = new List<Ast>(); MergeAsts(asts, first); MergeAsts(asts, rest_);
    if (asts.Count > 0) res.astList = asts;
    return res;
}

static Result opt(Input inp, PFn f) { var r = f(inp); return r.failed ? Ok(inp) : r; }
static Result neg(Input inp, PFn f) { var r = f(inp); return r.failed ? Ok(inp) : Fail(inp, "neg"); }
static Result minus(Input inp, PFn fa, PFn fb) {
    var ra = fa(inp); if (ra.failed) return ra;
    var rb = fb(inp); if (!rb.failed && rb.rest.pos == ra.rest.pos) return Fail(inp, "excl"); return ra;
}
static Result rep(Input inp, int n, PFn f) {
    Input cur = inp; var acc = new StringBuilder();
    for (int i = 0; i < n; i++) { var r = f(cur); if (r.failed) return r; acc.Append(r.val); cur = r.rest; }
    return OkV(cur, acc.ToString());
}
static Result ahead(Input inp, PFn f) { var r = f(inp); return r.failed ? r : Ok(inp); }
static Result behind(Input inp, PFn f) {
    if (inp.pos == 0) return Fail(inp, "bh");
    var t = new Input(inp.src, inp.pos - 1, inp.line, Math.Max(0, inp.col - 1));
    var r = f(t); return r.failed ? Fail(inp, "bh") : Ok(inp);
}
static Result sol(Input inp) => inp.col == 0 ? Ok(inp) : Fail(inp, "sol");
static Result eof_ok(Input inp) => AtEof(inp) ? Ok(inp) : Fail(inp, "eof");
static Result ok(Input inp) => Ok(inp);

// ── YAML extensions ──

static Result build(Input inp, string typ, PFn f) {
    var r = f(inp); if (r.failed) return r;
    var children = new List<Ast>();
    if (r.ast != null) children.Add(r.ast);
    if (r.astList != null) children.AddRange(r.astList);
    var node = Ast.Branch(typ); node.children = children;
    r.ast = node; r.astList = null; return r;
}

static Result scalar(Input inp, PFn f) {
    var r = f(inp); if (r.failed) return r;
    r.ast = Ast.Leaf(r.val); return r;
}

static Result collect(Input inp, PFn f) => f(inp);

static Result detect_indent(Input inp, int n) {
    string s = inp.src; int len = s.Length; int i = inp.pos;
    int sp = 0; while (i + sp < len && s[i + sp] == ' ') sp++;
    if (i + sp < len && s[i + sp] != '\n') {
        var r = Ok(inp); r.tagInt = Math.Max(1, sp - n); return r;
    }
    int j = i + sp;
    while (j < len) {
        if (j >= len) { var r = Ok(inp); r.tagInt = 1; return r; }
        if (s[j] == '\n') {
            j++;
            if (j >= len) { var r = Ok(inp); r.tagInt = 1; return r; }
            sp = 0; while (j + sp < len && s[j + sp] == ' ') sp++;
            int nx = j + sp;
            if (nx >= len || s[nx] == '\n') { j = nx; continue; }
            var r2 = Ok(inp); r2.tagInt = Math.Max(1, sp - n); return r2;
        }
        break;
    }
    var rf = Ok(inp); rf.tagInt = 1; return rf;
}

static Result parse_int(Input inp, PFn f) {
    var r = f(inp); if (r.failed) return r;
    int v = 0; foreach (char c in r.val) { if (c >= '0' && c <= '9') v = v * 10 + (c - '0'); }
    r.tagInt = v; return r;
}

static Result parse_sym(Input inp, PFn f, string sym) {
    var r = f(inp); if (r.failed) return r;
    r.tag = sym; return r;
}

static Result val(Input inp, string v) { var r = Ok(inp); r.tag = v; return r; }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] C-PRINTABLE 
static Result c_printable(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 0x9),
        (Input _i0) => match_cp(_i0, 0x0A),
        (Input _i0) => match_cp(_i0, 0x0D),
        (Input _i0) => match_range(_i0, 0x20, 0x7E),
        (Input _i0) => match_cp(_i0, 0x85),
        (Input _i0) => match_range(_i0, 0xA0, 0xD7FF),
        (Input _i0) => match_range(_i0, 0xE000, 0xFFFD),
        (Input _i0) => match_range(_i0, 0x10000, 0x10FFFF)});
}

// [2] NB-JSON 
static Result nb_json(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 0x9),
        (Input _i0) => match_range(_i0, 0x20, 0x10FFFF)});
}

// [3] C-BYTE-ORDER-MARK 
static Result c_byte_order_mark(Input inp) {
    return match_cp(inp, 0xFEFF);
}

// [4] C-SEQUENCE-ENTRY 
static Result c_sequence_entry(Input inp) {
    return match_cp(inp, 45);
}

// [5] C-MAPPING-KEY 
static Result c_mapping_key(Input inp) {
    return match_cp(inp, 63);
}

// [6] C-MAPPING-VALUE 
static Result c_mapping_value(Input inp) {
    return match_cp(inp, 58);
}

// [7] C-COLLECT-ENTRY 
static Result c_collect_entry(Input inp) {
    return match_cp(inp, 44);
}

// [8] C-SEQUENCE-START 
static Result c_sequence_start(Input inp) {
    return match_cp(inp, 91);
}

// [9] C-SEQUENCE-END 
static Result c_sequence_end(Input inp) {
    return match_cp(inp, 93);
}

// [10] C-MAPPING-START 
static Result c_mapping_start(Input inp) {
    return match_cp(inp, 123);
}

// [11] C-MAPPING-END 
static Result c_mapping_end(Input inp) {
    return match_cp(inp, 125);
}

// [12] C-COMMENT 
static Result c_comment(Input inp) {
    return match_cp(inp, 35);
}

// [13] C-ANCHOR 
static Result c_anchor(Input inp) {
    return match_cp(inp, 38);
}

// [14] C-ALIAS 
static Result c_alias(Input inp) {
    return match_cp(inp, 42);
}

// [15] C-TAG 
static Result c_tag(Input inp) {
    return match_cp(inp, 33);
}

// [16] C-LITERAL 
static Result c_literal(Input inp) {
    return match_cp(inp, 124);
}

// [17] C-FOLDED 
static Result c_folded(Input inp) {
    return match_cp(inp, 62);
}

// [18] C-SINGLE-QUOTE 
static Result c_single_quote(Input inp) {
    return match_cp(inp, 39);
}

// [19] C-DOUBLE-QUOTE 
static Result c_double_quote(Input inp) {
    return match_cp(inp, 34);
}

// [20] C-DIRECTIVE 
static Result c_directive(Input inp) {
    return match_cp(inp, 37);
}

// [21] C-RESERVED 
static Result c_reserved(Input inp) {
    return Alt(inp, new PFn[]{(Input _i0) => match_cp(_i0, 64), (Input _i0) => match_cp(_i0, 96)});
}

// [22] C-INDICATOR 
static Result c_indicator(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_sequence_entry(_i0),
        (Input _i0) => c_mapping_key(_i0),
        (Input _i0) => c_mapping_value(_i0),
        (Input _i0) => c_collect_entry(_i0),
        (Input _i0) => c_sequence_start(_i0),
        (Input _i0) => c_sequence_end(_i0),
        (Input _i0) => c_mapping_start(_i0),
        (Input _i0) => c_mapping_end(_i0),
        (Input _i0) => c_comment(_i0),
        (Input _i0) => c_anchor(_i0),
        (Input _i0) => c_alias(_i0),
        (Input _i0) => c_tag(_i0),
        (Input _i0) => c_literal(_i0),
        (Input _i0) => c_folded(_i0),
        (Input _i0) => c_single_quote(_i0),
        (Input _i0) => c_double_quote(_i0),
        (Input _i0) => c_directive(_i0),
        (Input _i0) => c_reserved(_i0)});
}

// [23] C-FLOW-INDICATOR 
static Result c_flow_indicator(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_collect_entry(_i0),
        (Input _i0) => c_sequence_start(_i0),
        (Input _i0) => c_sequence_end(_i0),
        (Input _i0) => c_mapping_start(_i0),
        (Input _i0) => c_mapping_end(_i0)});
}

// [24] B-LINE-FEED 
static Result b_line_feed(Input inp) {
    return match_cp(inp, 0x0A);
}

// [25] B-CARRIAGE-RETURN 
static Result b_carriage_return(Input inp) {
    return match_cp(inp, 0x0D);
}

// [26] B-CHAR 
static Result b_char(Input inp) {
    return Alt(inp, new PFn[]{(Input _i0) => b_line_feed(_i0), (Input _i0) => b_carriage_return(_i0)});
}

// [27] NB-CHAR 
static Result nb_char(Input inp) {
    return minus(inp, (Input _i0) => c_printable(_i0), (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => b_char(_i0), (Input _i0) => c_byte_order_mark(_i0)}));
}

// [28] B-BREAK 
static Result b_break(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => b_carriage_return(_i0), (Input _i0) => b_line_feed(_i0)}),
        (Input _i0) => b_carriage_return(_i0),
        (Input _i0) => b_line_feed(_i0)});
}

// [29] B-AS-LINE-FEED 
static Result b_as_line_feed(Input inp) {
    return b_break(inp);
}

// [30] B-NON-CONTENT 
static Result b_non_content(Input inp) {
    return b_break(inp);
}

// [31] S-SPACE 
static Result s_space(Input inp) {
    return match_cp(inp, 0x20);
}

// [32] S-TAB 
static Result s_tab(Input inp) {
    return match_cp(inp, 0x9);
}

// [33] S-WHITE 
static Result s_white(Input inp) {
    return Alt(inp, new PFn[]{(Input _i0) => s_space(_i0), (Input _i0) => s_tab(_i0)});
}

// [34] NS-CHAR 
static Result ns_char(Input inp) {
    return minus(inp, (Input _i0) => nb_char(_i0), (Input _i0) => s_white(_i0));
}

// [35] NS-DEC-DIGIT 
static Result ns_dec_digit(Input inp) {
    return match_range(inp, 0x30, 0x39);
}

// [36] NS-HEX-DIGIT 
static Result ns_hex_digit(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => ns_dec_digit(_i0),
        (Input _i0) => match_range(_i0, 0x41, 0x46),
        (Input _i0) => match_range(_i0, 0x61, 0x66)});
}

// [37] NS-ASCII-LETTER 
static Result ns_ascii_letter(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => match_range(_i0, 0x41, 0x5A),
        (Input _i0) => match_range(_i0, 0x61, 0x7A)});
}

// [38] NS-WORD-CHAR 
static Result ns_word_char(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => ns_dec_digit(_i0),
        (Input _i0) => ns_ascii_letter(_i0),
        (Input _i0) => match_cp(_i0, 45)});
}

// [39] NS-URI-CHAR 
static Result ns_uri_char(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => match_cp(_i0, 37),
            (Input _i0) => ns_hex_digit(_i0),
            (Input _i0) => ns_hex_digit(_i0)}),
        (Input _i0) => ns_word_char(_i0),
        (Input _i0) => match_cp(_i0, 35),
        (Input _i0) => match_cp(_i0, 59),
        (Input _i0) => match_cp(_i0, 47),
        (Input _i0) => match_cp(_i0, 63),
        (Input _i0) => match_cp(_i0, 58),
        (Input _i0) => match_cp(_i0, 64),
        (Input _i0) => match_cp(_i0, 38),
        (Input _i0) => match_cp(_i0, 61),
        (Input _i0) => match_cp(_i0, 43),
        (Input _i0) => match_cp(_i0, 36),
        (Input _i0) => match_cp(_i0, 44),
        (Input _i0) => match_cp(_i0, 95),
        (Input _i0) => match_cp(_i0, 46),
        (Input _i0) => match_cp(_i0, 33),
        (Input _i0) => match_cp(_i0, 126),
        (Input _i0) => match_cp(_i0, 42),
        (Input _i0) => match_cp(_i0, 39),
        (Input _i0) => match_cp(_i0, 40),
        (Input _i0) => match_cp(_i0, 41),
        (Input _i0) => match_cp(_i0, 91),
        (Input _i0) => match_cp(_i0, 93)});
}

// [40] NS-TAG-CHAR 
static Result ns_tag_char(Input inp) {
    return minus(inp, (Input _i0) => ns_uri_char(_i0), (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => c_tag(_i0), (Input _i0) => c_flow_indicator(_i0)}));
}

// [41] C-ESCAPE 
static Result c_escape(Input inp) {
    return match_cp(inp, 92);
}

// [42] NS-ESC-NULL 
static Result ns_esc_null(Input inp) {
    return match_cp(inp, 48);
}

// [43] NS-ESC-BELL 
static Result ns_esc_bell(Input inp) {
    return match_cp(inp, 97);
}

// [44] NS-ESC-BACKSPACE 
static Result ns_esc_backspace(Input inp) {
    return match_cp(inp, 98);
}

// [45] NS-ESC-HORIZONTAL-TAB 
static Result ns_esc_horizontal_tab(Input inp) {
    return match_cp(inp, 116);
}

// [46] NS-ESC-LINE-FEED 
static Result ns_esc_line_feed(Input inp) {
    return match_cp(inp, 110);
}

// [47] NS-ESC-VERTICAL-TAB 
static Result ns_esc_vertical_tab(Input inp) {
    return match_cp(inp, 118);
}

// [48] NS-ESC-FORM-FEED 
static Result ns_esc_form_feed(Input inp) {
    return match_cp(inp, 102);
}

// [49] NS-ESC-CARRIAGE-RETURN 
static Result ns_esc_carriage_return(Input inp) {
    return match_cp(inp, 114);
}

// [50] NS-ESC-ESCAPE 
static Result ns_esc_escape(Input inp) {
    return match_cp(inp, 101);
}

// [51] NS-ESC-SPACE 
static Result ns_esc_space(Input inp) {
    return match_cp(inp, 0x20);
}

// [52] NS-ESC-DOUBLE-QUOTE 
static Result ns_esc_double_quote(Input inp) {
    return match_cp(inp, 34);
}

// [53] NS-ESC-SLASH 
static Result ns_esc_slash(Input inp) {
    return match_cp(inp, 47);
}

// [54] NS-ESC-BACKSLASH 
static Result ns_esc_backslash(Input inp) {
    return match_cp(inp, 92);
}

// [55] NS-ESC-NEXT-LINE 
static Result ns_esc_next_line(Input inp) {
    return match_cp(inp, 78);
}

// [56] NS-ESC-NON-BREAKING-SPACE 
static Result ns_esc_non_breaking_space(Input inp) {
    return match_cp(inp, 95);
}

// [57] NS-ESC-LINE-SEPARATOR 
static Result ns_esc_line_separator(Input inp) {
    return match_cp(inp, 76);
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR 
static Result ns_esc_paragraph_separator(Input inp) {
    return match_cp(inp, 80);
}

// [59] NS-ESC-8-BIT 
static Result ns_esc_8_bit(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 120),
        (Input _i1) => rep(_i1, 2, (Input _i0) => ns_hex_digit(_i0))});
}

// [60] NS-ESC-16-BIT 
static Result ns_esc_16_bit(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 117),
        (Input _i1) => rep(_i1, 4, (Input _i0) => ns_hex_digit(_i0))});
}

// [61] NS-ESC-32-BIT 
static Result ns_esc_32_bit(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 85),
        (Input _i1) => rep(_i1, 8, (Input _i0) => ns_hex_digit(_i0))});
}

// [62] C-NS-ESC-CHAR 
static Result c_ns_esc_char(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_escape(_i0),
        (Input _i1) => Alt(_i1, new PFn[]{
            (Input _i0) => ns_esc_null(_i0),
            (Input _i0) => ns_esc_bell(_i0),
            (Input _i0) => ns_esc_backspace(_i0),
            (Input _i0) => ns_esc_horizontal_tab(_i0),
            (Input _i0) => ns_esc_line_feed(_i0),
            (Input _i0) => ns_esc_vertical_tab(_i0),
            (Input _i0) => ns_esc_form_feed(_i0),
            (Input _i0) => ns_esc_carriage_return(_i0),
            (Input _i0) => ns_esc_escape(_i0),
            (Input _i0) => ns_esc_space(_i0),
            (Input _i0) => ns_esc_double_quote(_i0),
            (Input _i0) => ns_esc_slash(_i0),
            (Input _i0) => ns_esc_backslash(_i0),
            (Input _i0) => ns_esc_next_line(_i0),
            (Input _i0) => ns_esc_non_breaking_space(_i0),
            (Input _i0) => ns_esc_line_separator(_i0),
            (Input _i0) => ns_esc_paragraph_separator(_i0),
            (Input _i0) => ns_esc_8_bit(_i0),
            (Input _i0) => ns_esc_16_bit(_i0),
            (Input _i0) => ns_esc_32_bit(_i0)})});
}

// [63] S-INDENT 
static Result s_indent(Input inp, int n) {
    return rep(inp, n, (Input _i0) => s_space(_i0));
}

// [64] S-INDENT-LT 
static Result s_indent_lt(Input inp, int n) {
    return star(inp, (Input _i0) => s_space(_i0));
}

// [65] S-INDENT-LE 
static Result s_indent_le(Input inp, int n) {
    return star(inp, (Input _i0) => s_space(_i0));
}

// [66] S-SEPARATE-IN-LINE 
static Result s_separate_in_line(Input inp) {
    return Alt(inp, new PFn[]{(Input _i1) => plus_(_i1, (Input _i0) => s_white(_i0)), (Input _i0) => ok(_i0)});
}

// [67] S-LINE-PREFIX 
static Result s_line_prefix(Input inp, int n, string c) {
    return ((Func<Result>)(() => { if (c == "BLOCK-IN") return s_block_line_prefix(inp, n); if (c == "BLOCK-OUT") return s_block_line_prefix(inp, n); if (c == "FLOW-IN") return s_flow_line_prefix(inp, n); if (c == "FLOW-OUT") return s_flow_line_prefix(inp, n); return Fail(inp, "no case"); }))();
}

// [68] S-BLOCK-LINE-PREFIX 
static Result s_block_line_prefix(Input inp, int n) {
    return s_indent(inp, n);
}

// [69] S-FLOW-LINE-PREFIX 
static Result s_flow_line_prefix(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_indent(_i0, n),
        (Input _i1) => opt(_i1, (Input _i0) => s_separate_in_line(_i0))});
}

// [70] L-EMPTY 
static Result l_empty(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => s_line_prefix(_i0, n, c), (Input _i0) => s_indent_lt(_i0, n)}),
        (Input _i0) => b_as_line_feed(_i0)});
}

// [71] B-L-TRIMMED 
static Result b_l_trimmed(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => b_non_content(_i0),
        (Input _i1) => plus_(_i1, (Input _i0) => l_empty(_i0, n, c))});
}

// [72] B-AS-SPACE 
static Result b_as_space(Input inp) {
    return b_break(inp);
}

// [73] B-L-FOLDED 
static Result b_l_folded(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{(Input _i0) => b_l_trimmed(_i0, n, c), (Input _i0) => b_as_space(_i0)});
}

// [74] S-FLOW-FOLDED 
static Result s_flow_folded(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i1) => opt(_i1, (Input _i0) => s_separate_in_line(_i0)),
        (Input _i0) => b_l_folded(_i0, n, "FLOW-IN"),
        (Input _i0) => s_flow_line_prefix(_i0, n)});
}

// [75] C-NB-COMMENT-TEXT 
static Result c_nb_comment_text(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_comment(_i0),
        (Input _i1) => star(_i1, (Input _i0) => nb_char(_i0))});
}

// [76] B-COMMENT 
static Result b_comment(Input inp) {
    return Alt(inp, new PFn[]{(Input _i0) => b_non_content(_i0), (Input _i0) => ok(_i0)});
}

// [77] S-B-COMMENT 
static Result s_b_comment(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i3) => opt(_i3, (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i0) => s_separate_in_line(_i0),
            (Input _i1) => opt(_i1, (Input _i0) => c_nb_comment_text(_i0))})),
        (Input _i0) => b_comment(_i0)});
}

// [78] L-COMMENT 
static Result l_comment(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_separate_in_line(_i0),
        (Input _i1) => opt(_i1, (Input _i0) => c_nb_comment_text(_i0)),
        (Input _i0) => b_non_content(_i0)});
}

// [79] S-L-COMMENTS 
static Result s_l_comments(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => s_b_comment(_i0), (Input _i0) => ok(_i0)}),
        (Input _i1) => star(_i1, (Input _i0) => l_comment(_i0))});
}

// [80] S-SEPARATE 
static Result s_separate(Input inp, int n, string c) {
    return ((Func<Result>)(() => { if (c == "BLOCK-OUT") return s_separate_lines(inp, n); if (c == "BLOCK-IN") return s_separate_lines(inp, n); if (c == "FLOW-OUT") return s_separate_lines(inp, n); if (c == "FLOW-IN") return s_separate_lines(inp, n); if (c == "BLOCK-KEY") return s_separate_in_line(inp); if (c == "FLOW-KEY") return s_separate_in_line(inp); return Fail(inp, "no case"); }))();
}

// [81] S-SEPARATE-LINES 
static Result s_separate_lines(Input inp, int n) {
    return Alt(inp, new PFn[]{
        (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_l_comments(_i0), (Input _i0) => s_flow_line_prefix(_i0, n)}),
        (Input _i0) => s_separate_in_line(_i0)});
}

// [82] L-DIRECTIVE 
static Result l_directive(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_directive(_i0),
        (Input _i1) => Alt(_i1, new PFn[]{
            (Input _i0) => ns_yaml_directive(_i0),
            (Input _i0) => ns_tag_directive(_i0),
            (Input _i0) => ns_reserved_directive(_i0)}),
        (Input _i0) => s_l_comments(_i0)});
}

// [83] NS-RESERVED-DIRECTIVE 
static Result ns_reserved_directive(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_directive_name(_i0),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => s_separate_in_line(_i0),
            (Input _i0) => ns_directive_parameter(_i0)}))});
}

// [84] NS-DIRECTIVE-NAME 
static Result ns_directive_name(Input inp) {
    return plus_(inp, (Input _i0) => ns_char(_i0));
}

// [85] NS-DIRECTIVE-PARAMETER 
static Result ns_directive_parameter(Input inp) {
    return plus_(inp, (Input _i0) => ns_char(_i0));
}

// [86] NS-YAML-DIRECTIVE 
static Result ns_yaml_directive(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_str(_i0, "YAML"),
        (Input _i0) => s_separate_in_line(_i0),
        (Input _i0) => ns_yaml_version(_i0)});
}

// [87] NS-YAML-VERSION 
static Result ns_yaml_version(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i1) => plus_(_i1, (Input _i0) => ns_dec_digit(_i0)),
        (Input _i0) => match_cp(_i0, 46),
        (Input _i1) => plus_(_i1, (Input _i0) => ns_dec_digit(_i0))});
}

// [88] NS-TAG-DIRECTIVE 
static Result ns_tag_directive(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_str(_i0, "TAG"),
        (Input _i0) => s_separate_in_line(_i0),
        (Input _i0) => c_tag_handle(_i0),
        (Input _i0) => s_separate_in_line(_i0),
        (Input _i0) => ns_tag_prefix(_i0)});
}

// [89] C-TAG-HANDLE 
static Result c_tag_handle(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_named_tag_handle(_i0),
        (Input _i0) => c_secondary_tag_handle(_i0),
        (Input _i0) => c_primary_tag_handle(_i0)});
}

// [90] C-PRIMARY-TAG-HANDLE 
static Result c_primary_tag_handle(Input inp) {
    return match_cp(inp, 33);
}

// [91] C-SECONDARY-TAG-HANDLE 
static Result c_secondary_tag_handle(Input inp) {
    return match_str(inp, "!!");
}

// [92] C-NAMED-TAG-HANDLE 
static Result c_named_tag_handle(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 33),
        (Input _i1) => plus_(_i1, (Input _i0) => ns_word_char(_i0)),
        (Input _i0) => match_cp(_i0, 33)});
}

// [93] NS-TAG-PREFIX 
static Result ns_tag_prefix(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_ns_local_tag_prefix(_i0),
        (Input _i0) => ns_global_tag_prefix(_i0)});
}

// [94] C-NS-LOCAL-TAG-PREFIX 
static Result c_ns_local_tag_prefix(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 33),
        (Input _i1) => star(_i1, (Input _i0) => ns_uri_char(_i0))});
}

// [95] NS-GLOBAL-TAG-PREFIX 
static Result ns_global_tag_prefix(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_tag_char(_i0),
        (Input _i1) => star(_i1, (Input _i0) => ns_uri_char(_i0))});
}

// [96] C-NS-PROPERTIES 
static Result c_ns_properties(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i3) => Seq(_i3, new PFn[]{
            (Input _i0) => c_ns_tag_property(_i0),
            (Input _i2) => opt(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_separate(_i0, n, c), (Input _i0) => c_ns_anchor_property(_i0)}))}),
        (Input _i3) => Seq(_i3, new PFn[]{
            (Input _i0) => c_ns_anchor_property(_i0),
            (Input _i2) => opt(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_separate(_i0, n, c), (Input _i0) => c_ns_tag_property(_i0)}))})});
}

// [97] C-NS-TAG-PROPERTY 
static Result c_ns_tag_property(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_verbatim_tag(_i0),
        (Input _i0) => c_ns_shorthand_tag(_i0),
        (Input _i0) => c_non_specific_tag(_i0)});
}

// [98] C-VERBATIM-TAG 
static Result c_verbatim_tag(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_str(_i0, "!<"),
        (Input _i1) => plus_(_i1, (Input _i0) => ns_uri_char(_i0)),
        (Input _i0) => match_cp(_i0, 62)});
}

// [99] C-NS-SHORTHAND-TAG 
static Result c_ns_shorthand_tag(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_tag_handle(_i0),
        (Input _i1) => plus_(_i1, (Input _i0) => ns_tag_char(_i0))});
}

// [100] C-NON-SPECIFIC-TAG 
static Result c_non_specific_tag(Input inp) {
    return match_cp(inp, 33);
}

// [101] C-NS-ANCHOR-PROPERTY 
static Result c_ns_anchor_property(Input inp) {
    return build(inp, "ANCHOR", (Input _i2) => Seq(_i2, new PFn[]{
        (Input _i0) => c_anchor(_i0),
        (Input _i1) => scalar(_i1, (Input _i0) => ns_anchor_name(_i0))}));
}

// [102] NS-ANCHOR-CHAR 
static Result ns_anchor_char(Input inp) {
    return minus(inp, (Input _i0) => ns_char(_i0), (Input _i0) => c_flow_indicator(_i0));
}

// [103] NS-ANCHOR-NAME 
static Result ns_anchor_name(Input inp) {
    return plus_(inp, (Input _i0) => ns_anchor_char(_i0));
}

// [104] C-NS-ALIAS-NODE 
static Result c_ns_alias_node(Input inp) {
    return build(inp, "ALIAS", (Input _i2) => Seq(_i2, new PFn[]{
        (Input _i0) => c_alias(_i0),
        (Input _i1) => scalar(_i1, (Input _i0) => ns_anchor_name(_i0))}));
}

// [105] E-SCALAR 
static Result e_scalar(Input inp) {
    return ok(inp);
}

// [106] E-NODE 
static Result e_node(Input inp) {
    return e_scalar(inp);
}

// [107] NB-DOUBLE-CHAR 
static Result nb_double_char(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_ns_esc_char(_i0),
        (Input _i2) => minus(_i2, (Input _i0) => nb_json(_i0), (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => match_cp(_i0, 92), (Input _i0) => match_cp(_i0, 34)}))});
}

// [108] NS-DOUBLE-CHAR 
static Result ns_double_char(Input inp) {
    return minus(inp, (Input _i0) => nb_double_char(_i0), (Input _i0) => s_white(_i0));
}

// [109] C-DOUBLE-QUOTED 
static Result c_double_quoted(Input inp, int n, string c) {
    return scalar(inp, (Input _i1) => Seq(_i1, new PFn[]{
        (Input _i0) => match_cp(_i0, 34),
        (Input _i0) => nb_double_text(_i0, n, c),
        (Input _i0) => match_cp(_i0, 34)}));
}

// [110] NB-DOUBLE-TEXT 
static Result nb_double_text(Input inp, int n, string c) {
    return ((Func<Result>)(() => { if (c == "FLOW-OUT") return nb_double_multi_line(inp, n); if (c == "FLOW-IN") return nb_double_multi_line(inp, n); if (c == "BLOCK-KEY") return nb_double_one_line(inp); if (c == "FLOW-KEY") return nb_double_one_line(inp); return Fail(inp, "no case"); }))();
}

// [111] NB-DOUBLE-ONE-LINE 
static Result nb_double_one_line(Input inp) {
    return star(inp, (Input _i0) => nb_double_char(_i0));
}

// [112] S-DOUBLE-ESCAPED 
static Result s_double_escaped(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => s_white(_i0)),
        (Input _i0) => match_cp(_i0, 92),
        (Input _i0) => b_non_content(_i0),
        (Input _i1) => star(_i1, (Input _i0) => l_empty(_i0, n, "FLOW-IN")),
        (Input _i0) => s_flow_line_prefix(_i0, n)});
}

// [113] S-DOUBLE-BREAK 
static Result s_double_break(Input inp, int n) {
    return Alt(inp, new PFn[]{(Input _i0) => s_double_escaped(_i0, n), (Input _i0) => s_flow_folded(_i0, n)});
}

// [114] NB-NS-DOUBLE-IN-LINE 
static Result nb_ns_double_in_line(Input inp) {
    return star(inp, (Input _i2) => Seq(_i2, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => s_white(_i0)),
        (Input _i0) => ns_double_char(_i0)}));
}

// [115] S-DOUBLE-NEXT-LINE 
static Result s_double_next_line(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_double_break(_i0, n),
        (Input _i4) => opt(_i4, (Input _i3) => Seq(_i3, new PFn[]{
            (Input _i0) => ns_double_char(_i0),
            (Input _i0) => nb_ns_double_in_line(_i0),
            (Input _i2) => Alt(_i2, new PFn[]{
                (Input _i0) => s_double_next_line(_i0, n),
                (Input _i1) => star(_i1, (Input _i0) => s_white(_i0))})}))});
}

// [116] NB-DOUBLE-MULTI-LINE 
static Result nb_double_multi_line(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => nb_ns_double_in_line(_i0),
        (Input _i2) => Alt(_i2, new PFn[]{
            (Input _i0) => s_double_next_line(_i0, n),
            (Input _i1) => star(_i1, (Input _i0) => s_white(_i0))})});
}

// [117] C-QUOTED-QUOTE 
static Result c_quoted_quote(Input inp) {
    return match_str(inp, "''");
}

// [118] NB-SINGLE-CHAR 
static Result nb_single_char(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_quoted_quote(_i0),
        (Input _i1) => minus(_i1, (Input _i0) => nb_json(_i0), (Input _i0) => match_cp(_i0, 39))});
}

// [119] NS-SINGLE-CHAR 
static Result ns_single_char(Input inp) {
    return minus(inp, (Input _i0) => nb_single_char(_i0), (Input _i0) => s_white(_i0));
}

// [120] C-SINGLE-QUOTED 
static Result c_single_quoted(Input inp, int n, string c) {
    return scalar(inp, (Input _i1) => Seq(_i1, new PFn[]{
        (Input _i0) => match_cp(_i0, 39),
        (Input _i0) => nb_single_text(_i0, n, c),
        (Input _i0) => match_cp(_i0, 39)}));
}

// [121] NB-SINGLE-TEXT 
static Result nb_single_text(Input inp, int n, string c) {
    return ((Func<Result>)(() => { if (c == "FLOW-OUT") return nb_single_multi_line(inp, n); if (c == "FLOW-IN") return nb_single_multi_line(inp, n); if (c == "BLOCK-KEY") return nb_single_one_line(inp); if (c == "FLOW-KEY") return nb_single_one_line(inp); return Fail(inp, "no case"); }))();
}

// [122] NB-SINGLE-ONE-LINE 
static Result nb_single_one_line(Input inp) {
    return star(inp, (Input _i0) => nb_single_char(_i0));
}

// [123] NS-SINGLE-IN-LINE 
static Result ns_single_in_line(Input inp) {
    return star(inp, (Input _i2) => Seq(_i2, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => s_white(_i0)),
        (Input _i0) => ns_single_char(_i0)}));
}

// [124] S-SINGLE-NEXT-LINE 
static Result s_single_next_line(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_flow_folded(_i0, n),
        (Input _i4) => opt(_i4, (Input _i3) => Seq(_i3, new PFn[]{
            (Input _i0) => ns_single_char(_i0),
            (Input _i0) => ns_single_in_line(_i0),
            (Input _i2) => Alt(_i2, new PFn[]{
                (Input _i0) => s_single_next_line(_i0, n),
                (Input _i1) => star(_i1, (Input _i0) => s_white(_i0))})}))});
}

// [125] NB-SINGLE-MULTI-LINE 
static Result nb_single_multi_line(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_single_in_line(_i0),
        (Input _i2) => Alt(_i2, new PFn[]{
            (Input _i0) => s_single_next_line(_i0, n),
            (Input _i1) => star(_i1, (Input _i0) => s_white(_i0))})});
}

// [126] NS-PLAIN-FIRST 
static Result ns_plain_first(Input inp, string c) {
    return Alt(inp, new PFn[]{
        (Input _i1) => minus(_i1, (Input _i0) => ns_char(_i0), (Input _i0) => c_indicator(_i0)),
        (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i1) => Alt(_i1, new PFn[]{
                (Input _i0) => match_cp(_i0, 63),
                (Input _i0) => match_cp(_i0, 58),
                (Input _i0) => match_cp(_i0, 45)}),
            (Input _i1) => ahead(_i1, (Input _i0) => ns_plain_safe(_i0, c))})});
}

// [127] NS-PLAIN-SAFE 
static Result ns_plain_safe(Input inp, string c) {
    return ((Func<Result>)(() => { if (c == "FLOW-OUT") return ns_plain_safe_out(inp); if (c == "FLOW-IN") return ns_plain_safe_in(inp); if (c == "BLOCK-KEY") return ns_plain_safe_out(inp); if (c == "FLOW-KEY") return ns_plain_safe_in(inp); return Fail(inp, "no case"); }))();
}

// [128] NS-PLAIN-SAFE-OUT 
static Result ns_plain_safe_out(Input inp) {
    return ns_char(inp);
}

// [129] NS-PLAIN-SAFE-IN 
static Result ns_plain_safe_in(Input inp) {
    return minus(inp, (Input _i0) => ns_char(_i0), (Input _i0) => c_flow_indicator(_i0));
}

// [130] NS-PLAIN-CHAR 
static Result ns_plain_char(Input inp, string c) {
    return Alt(inp, new PFn[]{
        (Input _i2) => minus(_i2, (Input _i0) => ns_plain_safe(_i0, c), (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => match_cp(_i0, 58), (Input _i0) => match_cp(_i0, 35)})),
        (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i1) => behind(_i1, (Input _i0) => ns_char(_i0)),
            (Input _i0) => match_cp(_i0, 35)}),
        (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i0) => match_cp(_i0, 58),
            (Input _i1) => ahead(_i1, (Input _i0) => ns_plain_safe(_i0, c))})});
}

// [131] NS-PLAIN 
static Result ns_plain(Input inp, int n, string c) {
    return scalar(inp, (Input _i0) => ((Func<Result>)(() => { if (c == "FLOW-OUT") return ns_plain_multi_line(_i0, n, c); if (c == "FLOW-IN") return ns_plain_multi_line(_i0, n, c); if (c == "BLOCK-KEY") return ns_plain_one_line(_i0, c); if (c == "FLOW-KEY") return ns_plain_one_line(_i0, c); return Fail(_i0, "no case"); }))());
}

// [132] NB-NS-PLAIN-IN-LINE 
static Result nb_ns_plain_in_line(Input inp, string c) {
    return star(inp, (Input _i2) => Seq(_i2, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => s_white(_i0)),
        (Input _i0) => ns_plain_char(_i0, c)}));
}

// [133] NS-PLAIN-ONE-LINE 
static Result ns_plain_one_line(Input inp, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_plain_first(_i0, c),
        (Input _i0) => nb_ns_plain_in_line(_i0, c)});
}

// [134] S-NS-PLAIN-NEXT-LINE 
static Result s_ns_plain_next_line(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_flow_folded(_i0, n),
        (Input _i1) => neg(_i1, (Input _i0) => c_forbidden(_i0)),
        (Input _i0) => ns_plain_char(_i0, c),
        (Input _i0) => nb_ns_plain_in_line(_i0, c)});
}

// [135] NS-PLAIN-MULTI-LINE 
static Result ns_plain_multi_line(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_plain_one_line(_i0, c),
        (Input _i1) => star(_i1, (Input _i0) => s_ns_plain_next_line(_i0, n, c))});
}

// [137] C-FLOW-SEQUENCE 
static Result c_flow_sequence(Input inp, int n, string c) {
    return build(inp, "SEQUENCE", (Input _i3) => Seq(_i3, new PFn[]{
        (Input _i0) => match_cp(_i0, 91),
        (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
        (Input _i2) => opt(_i2, (Input _i1) => collect(_i1, (Input _i0) => ns_s_flow_seq_entries(_i0, n, InFlow(c)))),
        (Input _i0) => match_cp(_i0, 93)}));
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
static Result ns_s_flow_seq_entries(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_flow_seq_entry(_i0, n, c),
        (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
        (Input _i3) => opt(_i3, (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i0) => match_cp(_i0, 44),
            (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
            (Input _i1) => opt(_i1, (Input _i0) => ns_s_flow_seq_entries(_i0, n, c))}))});
}

// [139] NS-FLOW-SEQ-ENTRY 
static Result ns_flow_seq_entry(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{(Input _i0) => ns_flow_pair(_i0, n, c), (Input _i0) => ns_flow_node(_i0, n, c)});
}

// [140] C-FLOW-MAPPING 
static Result c_flow_mapping(Input inp, int n, string c) {
    return build(inp, "MAPPING", (Input _i3) => Seq(_i3, new PFn[]{
        (Input _i0) => match_cp(_i0, 123),
        (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
        (Input _i2) => opt(_i2, (Input _i1) => collect(_i1, (Input _i0) => ns_s_flow_map_entries(_i0, n, InFlow(c)))),
        (Input _i0) => match_cp(_i0, 125)}));
}

// [141] NS-S-FLOW-MAP-ENTRIES 
static Result ns_s_flow_map_entries(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_flow_map_entry(_i0, n, c),
        (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
        (Input _i3) => opt(_i3, (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i0) => match_cp(_i0, 44),
            (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
            (Input _i1) => opt(_i1, (Input _i0) => ns_s_flow_map_entries(_i0, n, c))}))});
}

// [142] NS-FLOW-MAP-ENTRY 
static Result ns_flow_map_entry(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => match_cp(_i0, 63),
            (Input _i0) => s_separate(_i0, n, c),
            (Input _i0) => ns_flow_map_explicit_entry(_i0, n, c)}),
        (Input _i0) => ns_flow_map_implicit_entry(_i0, n, c)});
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
static Result ns_flow_map_explicit_entry(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => ns_flow_map_implicit_entry(_i0, n, c),
        (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => e_node(_i0), (Input _i0) => e_node(_i0)})});
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
static Result ns_flow_map_implicit_entry(Input inp, int n, string c) {
    return build(inp, "PAIR", (Input _i1) => Alt(_i1, new PFn[]{
        (Input _i0) => ns_flow_map_yaml_key_entry(_i0, n, c),
        (Input _i0) => c_ns_flow_map_empty_key_entry(_i0, n, c),
        (Input _i0) => c_ns_flow_map_json_key_entry(_i0, n, c)}));
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
static Result ns_flow_map_yaml_key_entry(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_flow_yaml_node(_i0, n, c),
        (Input _i3) => Alt(_i3, new PFn[]{
            (Input _i2) => Seq(_i2, new PFn[]{
                (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
                (Input _i0) => c_ns_flow_map_separate_value(_i0, n, c)}),
            (Input _i0) => e_node(_i0)})});
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
static Result c_ns_flow_map_empty_key_entry(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => e_node(_i0),
        (Input _i0) => c_ns_flow_map_separate_value(_i0, n, c)});
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
static Result c_ns_flow_map_separate_value(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 58),
        (Input _i1) => neg(_i1, (Input _i0) => ns_plain_safe(_i0, c)),
        (Input _i2) => Alt(_i2, new PFn[]{
            (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_separate(_i0, n, c), (Input _i0) => ns_flow_node(_i0, n, c)}),
            (Input _i0) => e_node(_i0)})});
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
static Result c_ns_flow_map_json_key_entry(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_flow_json_node(_i0, n, c),
        (Input _i3) => Alt(_i3, new PFn[]{
            (Input _i2) => Seq(_i2, new PFn[]{
                (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
                (Input _i0) => c_ns_flow_map_adjacent_value(_i0, n, c)}),
            (Input _i0) => e_node(_i0)})});
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
static Result c_ns_flow_map_adjacent_value(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 58),
        (Input _i3) => Alt(_i3, new PFn[]{
            (Input _i2) => Seq(_i2, new PFn[]{
                (Input _i1) => opt(_i1, (Input _i0) => s_separate(_i0, n, c)),
                (Input _i0) => ns_flow_node(_i0, n, c)}),
            (Input _i0) => e_node(_i0)})});
}

// [150] NS-FLOW-PAIR 
static Result ns_flow_pair(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => match_cp(_i0, 63),
            (Input _i0) => s_separate(_i0, n, c),
            (Input _i0) => ns_flow_map_explicit_entry(_i0, n, c)}),
        (Input _i0) => ns_flow_pair_entry(_i0, n, c)});
}

// [151] NS-FLOW-PAIR-ENTRY 
static Result ns_flow_pair_entry(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => ns_flow_pair_yaml_key_entry(_i0, n, c),
        (Input _i0) => c_ns_flow_map_empty_key_entry(_i0, n, c),
        (Input _i0) => c_ns_flow_pair_json_key_entry(_i0, n, c)});
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
static Result ns_flow_pair_yaml_key_entry(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_s_implicit_yaml_key(_i0, "FLOW-KEY"),
        (Input _i0) => c_ns_flow_map_separate_value(_i0, n, c)});
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
static Result c_ns_flow_pair_json_key_entry(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_s_implicit_json_key(_i0, "FLOW-KEY"),
        (Input _i0) => c_ns_flow_map_adjacent_value(_i0, n, c)});
}

// [154] NS-S-IMPLICIT-YAML-KEY 
static Result ns_s_implicit_yaml_key(Input inp, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_flow_yaml_node(_i0, 0, c),
        (Input _i1) => opt(_i1, (Input _i0) => s_separate_in_line(_i0))});
}

// [155] C-S-IMPLICIT-JSON-KEY 
static Result c_s_implicit_json_key(Input inp, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_flow_json_node(_i0, 0, c),
        (Input _i1) => opt(_i1, (Input _i0) => s_separate_in_line(_i0))});
}

// [156] NS-FLOW-YAML-CONTENT 
static Result ns_flow_yaml_content(Input inp, int n, string c) {
    return ns_plain(inp, n, c);
}

// [157] C-FLOW-JSON-CONTENT 
static Result c_flow_json_content(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_flow_sequence(_i0, n, c),
        (Input _i0) => c_flow_mapping(_i0, n, c),
        (Input _i0) => c_single_quoted(_i0, n, c),
        (Input _i0) => c_double_quoted(_i0, n, c)});
}

// [158] NS-FLOW-CONTENT 
static Result ns_flow_content(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => ns_flow_yaml_content(_i0, n, c),
        (Input _i0) => c_flow_json_content(_i0, n, c)});
}

// [159] NS-FLOW-YAML-NODE 
static Result ns_flow_yaml_node(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_ns_alias_node(_i0),
        (Input _i0) => ns_flow_yaml_content(_i0, n, c),
        (Input _i3) => Seq(_i3, new PFn[]{
            (Input _i0) => c_ns_properties(_i0, n, c),
            (Input _i2) => Alt(_i2, new PFn[]{
                (Input _i1) => Seq(_i1, new PFn[]{
                    (Input _i0) => s_separate(_i0, n, c),
                    (Input _i0) => ns_flow_yaml_content(_i0, n, c)}),
                (Input _i0) => e_scalar(_i0)})})});
}

// [160] C-FLOW-JSON-NODE 
static Result c_flow_json_node(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i2) => opt(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => c_ns_properties(_i0, n, c), (Input _i0) => s_separate(_i0, n, c)})),
        (Input _i0) => c_flow_json_content(_i0, n, c)});
}

// [161] NS-FLOW-NODE 
static Result ns_flow_node(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_ns_alias_node(_i0),
        (Input _i0) => ns_flow_content(_i0, n, c),
        (Input _i3) => Seq(_i3, new PFn[]{
            (Input _i0) => c_ns_properties(_i0, n, c),
            (Input _i2) => Alt(_i2, new PFn[]{
                (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_separate(_i0, n, c), (Input _i0) => ns_flow_content(_i0, n, c)}),
                (Input _i0) => e_scalar(_i0)})})});
}

// [162] C-B-BLOCK-HEADER 
static Result c_b_block_header(Input inp, int n) {
    return Alt(inp, new PFn[]{
        (Input _i2) => ((Func<Result>)(() => { var r6_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_int(_i1, (Input _i0) => ns_dec_digit(_i0)),
            (Input _i0) => detect_indent(_i0, n)}); if (r6_.failed) return r6_; int m = r6_.tagInt; _i2 = r6_.rest; return ((Func<Result>)(() => { var r5_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 45), "STRIP"),
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 43), "KEEP"),
            (Input _i0) => val(_i0, "CLIP")}); if (r5_.failed) return r5_; string t = r5_.tag; _i2 = r5_.rest; return s_b_comment(_i2); }))(); }))(),
        (Input _i2) => ((Func<Result>)(() => { var r8_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 45), "STRIP"),
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 43), "KEEP"),
            (Input _i0) => val(_i0, "CLIP")}); if (r8_.failed) return r8_; string t = r8_.tag; _i2 = r8_.rest; return ((Func<Result>)(() => { var r7_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_int(_i1, (Input _i0) => ns_dec_digit(_i0)),
            (Input _i0) => detect_indent(_i0, n)}); if (r7_.failed) return r7_; int m = r7_.tagInt; _i2 = r7_.rest; return s_b_comment(_i2); }))(); }))()});
}

// [163] C-INDENTATION-INDICATOR 
static Result c_indentation_indicator(Input inp, int n) {
    return Alt(inp, new PFn[]{(Input _i0) => ns_dec_digit(_i0), (Input _i0) => ok(_i0)});
}

// [164] C-CHOMPING-INDICATOR 
static Result c_chomping_indicator(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 45),
        (Input _i0) => match_cp(_i0, 43),
        (Input _i0) => ok(_i0)});
}

// [165] B-CHOMPED-LAST 
static Result b_chomped_last(Input inp, string t) {
    return ((Func<Result>)(() => { if (t == "STRIP") return b_non_content(inp); if (t == "CLIP") return b_as_line_feed(inp); if (t == "KEEP") return b_as_line_feed(inp); return Fail(inp, "no case"); }))();
}

// [166] L-CHOMPED-EMPTY 
static Result l_chomped_empty(Input inp, int n, string t) {
    return ((Func<Result>)(() => { if (t == "STRIP") return l_strip_empty(inp, n); if (t == "CLIP") return l_strip_empty(inp, n); if (t == "KEEP") return l_keep_empty(inp, n); return Fail(inp, "no case"); }))();
}

// [167] L-STRIP-EMPTY 
static Result l_strip_empty(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_indent_le(_i0, n), (Input _i0) => b_non_content(_i0)})),
        (Input _i1) => opt(_i1, (Input _i0) => l_trail_comments(_i0, n))});
}

// [168] L-KEEP-EMPTY 
static Result l_keep_empty(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => l_empty(_i0, n, "BLOCK-IN")),
        (Input _i1) => opt(_i1, (Input _i0) => l_trail_comments(_i0, n))});
}

// [169] L-TRAIL-COMMENTS 
static Result l_trail_comments(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_indent_lt(_i0, n),
        (Input _i0) => c_nb_comment_text(_i0),
        (Input _i0) => b_comment(_i0),
        (Input _i1) => star(_i1, (Input _i0) => l_comment(_i0))});
}

// [170] C-L+LITERAL 
static Result c_lliteral(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 124),
        (Input _i2) => ((Func<Result>)(() => { var r12_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_int(_i1, (Input _i0) => ns_dec_digit(_i0)),
            (Input _i0) => detect_indent(_i0, n)}); if (r12_.failed) return r12_; int m = r12_.tagInt; _i2 = r12_.rest; return ((Func<Result>)(() => { var r11_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 45), "STRIP"),
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 43), "KEEP"),
            (Input _i0) => val(_i0, "CLIP")}); if (r11_.failed) return r11_; string t = r11_.tag; _i2 = r11_.rest; return Seq(_i2, new PFn[]{
            (Input _i0) => s_b_comment(_i0),
            (Input _i0) => l_literal_content(_i0, (n + m), t)}); }))(); }))()});
}

// [171] L-NB-LITERAL-TEXT 
static Result l_nb_literal_text(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => l_empty(_i0, n, "BLOCK-IN")),
        (Input _i0) => s_indent(_i0, n),
        (Input _i1) => plus_(_i1, (Input _i0) => nb_char(_i0))});
}

// [172] B-NB-LITERAL-NEXT 
static Result b_nb_literal_next(Input inp, int n) {
    return Seq(inp, new PFn[]{(Input _i0) => b_as_line_feed(_i0), (Input _i0) => l_nb_literal_text(_i0, n)});
}

// [173] L-LITERAL-CONTENT 
static Result l_literal_content(Input inp, int n, string t) {
    return scalar(inp, (Input _i4) => Seq(_i4, new PFn[]{
        (Input _i3) => opt(_i3, (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i0) => l_nb_literal_text(_i0, n),
            (Input _i1) => star(_i1, (Input _i0) => b_nb_literal_next(_i0, n)),
            (Input _i0) => b_chomped_last(_i0, t)})),
        (Input _i0) => l_chomped_empty(_i0, n, t)}));
}

// [174] C-L+FOLDED 
static Result c_lfolded(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 62),
        (Input _i2) => ((Func<Result>)(() => { var r16_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_int(_i1, (Input _i0) => ns_dec_digit(_i0)),
            (Input _i0) => detect_indent(_i0, n)}); if (r16_.failed) return r16_; int m = r16_.tagInt; _i2 = r16_.rest; return ((Func<Result>)(() => { var r15_ = Alt(_i2, new PFn[]{
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 45), "STRIP"),
            (Input _i1) => parse_sym(_i1, (Input _i0) => match_cp(_i0, 43), "KEEP"),
            (Input _i0) => val(_i0, "CLIP")}); if (r15_.failed) return r15_; string t = r15_.tag; _i2 = r15_.rest; return Seq(_i2, new PFn[]{
            (Input _i0) => s_b_comment(_i0),
            (Input _i0) => l_folded_content(_i0, (n + m), t)}); }))(); }))()});
}

// [175] S-NB-FOLDED-TEXT 
static Result s_nb_folded_text(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_indent(_i0, n),
        (Input _i0) => ns_char(_i0),
        (Input _i1) => star(_i1, (Input _i0) => nb_char(_i0))});
}

// [176] L-NB-FOLDED-LINES 
static Result l_nb_folded_lines(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_nb_folded_text(_i0, n),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => b_l_folded(_i0, n, "BLOCK-IN"),
            (Input _i0) => s_nb_folded_text(_i0, n)}))});
}

// [177] S-NB-SPACED-TEXT 
static Result s_nb_spaced_text(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_indent(_i0, n),
        (Input _i0) => s_white(_i0),
        (Input _i1) => star(_i1, (Input _i0) => nb_char(_i0))});
}

// [178] B-L-SPACED 
static Result b_l_spaced(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => b_as_line_feed(_i0),
        (Input _i1) => star(_i1, (Input _i0) => l_empty(_i0, n, "BLOCK-IN"))});
}

// [179] L-NB-SPACED-LINES 
static Result l_nb_spaced_lines(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_nb_spaced_text(_i0, n),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => b_l_spaced(_i0, n), (Input _i0) => s_nb_spaced_text(_i0, n)}))});
}

// [180] L-NB-SAME-LINES 
static Result l_nb_same_lines(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => l_empty(_i0, n, "BLOCK-IN")),
        (Input _i1) => Alt(_i1, new PFn[]{
            (Input _i0) => l_nb_folded_lines(_i0, n),
            (Input _i0) => l_nb_spaced_lines(_i0, n)})});
}

// [181] L-NB-DIFF-LINES 
static Result l_nb_diff_lines(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => l_nb_same_lines(_i0, n),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => b_as_line_feed(_i0), (Input _i0) => l_nb_same_lines(_i0, n)}))});
}

// [182] L-FOLDED-CONTENT 
static Result l_folded_content(Input inp, int n, string t) {
    return scalar(inp, (Input _i3) => Seq(_i3, new PFn[]{
        (Input _i2) => opt(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => l_nb_diff_lines(_i0, n), (Input _i0) => b_chomped_last(_i0, t)})),
        (Input _i0) => l_chomped_empty(_i0, n, t)}));
}

// [183] L+BLOCK-SEQUENCE 
static Result lblock_sequence(Input inp, int n) {
    return build(inp, "SEQUENCE", (Input _i3) => ((Func<Result>)(() => { var r17_ = detect_indent(_i3, n); if (r17_.failed) return r17_; int m = r17_.tagInt; _i3 = r17_.rest; return collect(_i3, (Input _i2) => plus_(_i2, (Input _i1) => Seq(_i1, new PFn[]{
        (Input _i0) => s_indent(_i0, (n + m)),
        (Input _i0) => c_l_block_seq_entry(_i0, (n + m))}))); }))());
}

// [184] C-L-BLOCK-SEQ-ENTRY 
static Result c_l_block_seq_entry(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 45),
        (Input _i1) => neg(_i1, (Input _i0) => ns_char(_i0)),
        (Input _i0) => s_lblock_indented(_i0, n, "BLOCK-IN")});
}

// [185] S-L+BLOCK-INDENTED 
static Result s_lblock_indented(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i2) => ((Func<Result>)(() => { var r19_ = detect_indent(_i2, 0); if (r19_.failed) return r19_; int m = r19_.tagInt; _i2 = r19_.rest; return Seq(_i2, new PFn[]{
            (Input _i0) => s_indent(_i0, m),
            (Input _i1) => Alt(_i1, new PFn[]{
                (Input _i0) => ns_l_compact_sequence(_i0, (n + 1 + m)),
                (Input _i0) => ns_l_compact_mapping(_i0, (n + 1 + m))})}); }))(),
        (Input _i0) => s_lblock_node(_i0, n, c),
        (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => e_node(_i0), (Input _i0) => s_l_comments(_i0)})});
}

// [186] NS-L-COMPACT-SEQUENCE 
static Result ns_l_compact_sequence(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_l_block_seq_entry(_i0, n),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_indent(_i0, n), (Input _i0) => c_l_block_seq_entry(_i0, n)}))});
}

// [187] L+BLOCK-MAPPING 
static Result lblock_mapping(Input inp, int n) {
    return build(inp, "MAPPING", (Input _i3) => ((Func<Result>)(() => { var r20_ = detect_indent(_i3, n); if (r20_.failed) return r20_; int m = r20_.tagInt; _i3 = r20_.rest; return collect(_i3, (Input _i2) => plus_(_i2, (Input _i1) => Seq(_i1, new PFn[]{
        (Input _i0) => s_indent(_i0, (n + m)),
        (Input _i0) => ns_l_block_map_entry(_i0, (n + m))}))); }))());
}

// [188] NS-L-BLOCK-MAP-ENTRY 
static Result ns_l_block_map_entry(Input inp, int n) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_l_block_map_explicit_entry(_i0, n),
        (Input _i0) => ns_l_block_map_implicit_entry(_i0, n)});
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
static Result c_l_block_map_explicit_entry(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => c_l_block_map_explicit_key(_i0, n),
        (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => l_block_map_explicit_value(_i0, n), (Input _i0) => e_node(_i0)})});
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
static Result c_l_block_map_explicit_key(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 63),
        (Input _i0) => s_lblock_indented(_i0, n, "BLOCK-OUT")});
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
static Result l_block_map_explicit_value(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_indent(_i0, n),
        (Input _i0) => match_cp(_i0, 58),
        (Input _i0) => s_lblock_indented(_i0, n, "BLOCK-OUT")});
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
static Result ns_l_block_map_implicit_entry(Input inp, int n) {
    return build(inp, "PAIR", (Input _i3) => Seq(_i3, new PFn[]{
        (Input _i2) => scalar(_i2, (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => ns_s_block_map_implicit_key(_i0), (Input _i0) => e_node(_i0)})),
        (Input _i0) => c_l_block_map_implicit_value(_i0, n)}));
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
static Result ns_s_block_map_implicit_key(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => c_s_implicit_json_key(_i0, "BLOCK-KEY"),
        (Input _i0) => ns_s_implicit_yaml_key(_i0, "BLOCK-KEY")});
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
static Result c_l_block_map_implicit_value(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 58),
        (Input _i3) => Alt(_i3, new PFn[]{
            (Input _i0) => s_lblock_node(_i0, n, "BLOCK-OUT"),
            (Input _i2) => scalar(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => e_node(_i0), (Input _i0) => s_l_comments(_i0)}))})});
}

// [195] NS-L-COMPACT-MAPPING 
static Result ns_l_compact_mapping(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ns_l_block_map_entry(_i0, n),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => s_indent(_i0, n), (Input _i0) => ns_l_block_map_entry(_i0, n)}))});
}

// [196] S-L+BLOCK-NODE 
static Result s_lblock_node(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => s_lblock_in_block(_i0, n, c),
        (Input _i0) => s_lflow_in_block(_i0, n)});
}

// [197] S-L+FLOW-IN-BLOCK 
static Result s_lflow_in_block(Input inp, int n) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_separate(_i0, (n + 1), "FLOW-OUT"),
        (Input _i0) => ns_flow_node(_i0, (n + 1), "FLOW-OUT"),
        (Input _i0) => s_l_comments(_i0)});
}

// [198] S-L+BLOCK-IN-BLOCK 
static Result s_lblock_in_block(Input inp, int n, string c) {
    return Alt(inp, new PFn[]{
        (Input _i0) => s_lblock_scalar(_i0, n, c),
        (Input _i0) => s_lblock_collection(_i0, n, c)});
}

// [199] S-L+BLOCK-SCALAR 
static Result s_lblock_scalar(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i0) => s_separate(_i0, (n + 1), c),
        (Input _i2) => opt(_i2, (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => c_ns_properties(_i0, (n + 1), c),
            (Input _i0) => s_separate(_i0, (n + 1), c)})),
        (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => c_lliteral(_i0, n), (Input _i0) => c_lfolded(_i0, n)})});
}

// [200] S-L+BLOCK-COLLECTION 
static Result s_lblock_collection(Input inp, int n, string c) {
    return Seq(inp, new PFn[]{
        (Input _i2) => opt(_i2, (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => s_separate(_i0, (n + 1), c),
            (Input _i0) => c_ns_properties(_i0, (n + 1), c)})),
        (Input _i0) => s_l_comments(_i0),
        (Input _i1) => Alt(_i1, new PFn[]{
            (Input _i0) => lblock_sequence(_i0, SeqSpaces(n, c)),
            (Input _i0) => lblock_mapping(_i0, n)})});
}

// [202] L-DOCUMENT-PREFIX 
static Result l_document_prefix(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i1) => opt(_i1, (Input _i0) => c_byte_order_mark(_i0)),
        (Input _i1) => star(_i1, (Input _i0) => l_comment(_i0))});
}

// [203] C-DIRECTIVES-END 
static Result c_directives_end(Input inp) {
    return match_str(inp, "---");
}

// [204] C-DOCUMENT-END 
static Result c_document_end(Input inp) {
    return match_str(inp, "...");
}

// [205] L-DOCUMENT-SUFFIX 
static Result l_document_suffix(Input inp) {
    return Seq(inp, new PFn[]{(Input _i0) => c_document_end(_i0), (Input _i0) => s_l_comments(_i0)});
}

// [206] C-FORBIDDEN 
static Result c_forbidden(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => sol(_i0),
        (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => c_directives_end(_i0), (Input _i0) => c_document_end(_i0)}),
        (Input _i1) => Alt(_i1, new PFn[]{
            (Input _i0) => b_char(_i0),
            (Input _i0) => s_white(_i0),
            (Input _i0) => eof_ok(_i0)})});
}

// [207] L-BARE-DOCUMENT 
static Result l_bare_document(Input inp) {
    return build(inp, "DOC", (Input _i0) => s_lblock_node(_i0, -1, "BLOCK-IN"));
}

// [208] L-EXPLICIT-DOCUMENT 
static Result l_explicit_document(Input inp) {
    return build(inp, "DOC", (Input _i3) => Seq(_i3, new PFn[]{
        (Input _i0) => c_directives_end(_i0),
        (Input _i2) => Alt(_i2, new PFn[]{
            (Input _i0) => l_bare_document(_i0),
            (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => e_node(_i0), (Input _i0) => s_l_comments(_i0)})})}));
}

// [209] L-DIRECTIVE-DOCUMENT 
static Result l_directive_document(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i1) => plus_(_i1, (Input _i0) => l_directive(_i0)),
        (Input _i0) => l_explicit_document(_i0)});
}

// [210] L-ANY-DOCUMENT 
static Result l_any_document(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => l_directive_document(_i0),
        (Input _i0) => l_explicit_document(_i0),
        (Input _i0) => l_bare_document(_i0)});
}

// [211] L-YAML-STREAM 
static Result l_yaml_stream(Input inp) {
    return build(inp, "STREAM", (Input _i5) => Seq(_i5, new PFn[]{
        (Input _i1) => star(_i1, (Input _i0) => l_document_prefix(_i0)),
        (Input _i1) => opt(_i1, (Input _i0) => l_any_document(_i0)),
        (Input _i4) => star(_i4, (Input _i3) => Alt(_i3, new PFn[]{
            (Input _i2) => Seq(_i2, new PFn[]{
                (Input _i1) => plus_(_i1, (Input _i0) => l_document_suffix(_i0)),
                (Input _i1) => star(_i1, (Input _i0) => l_document_prefix(_i0)),
                (Input _i1) => opt(_i1, (Input _i0) => l_any_document(_i0))}),
            (Input _i2) => Seq(_i2, new PFn[]{
                (Input _i1) => star(_i1, (Input _i0) => l_document_prefix(_i0)),
                (Input _i1) => opt(_i1, (Input _i0) => l_explicit_document(_i0))})}))}));
}

// ── API ──

static void PrintAst(Ast node, int depth) {
    string indent = new string(' ', depth * 2);
    if (node.isLeaf) { Console.WriteLine(indent + "SCALAR: \"" + node.text + "\""); }
    else {
        Console.WriteLine(indent + node.tag);
        foreach (var c in node.children) PrintAst(c, depth + 1);
    }
}

// ── Native Value Type ──

// ── Native Value Type ──

enum YTag { YNull, YBool, YInt, YFloat, YStr, YMap, YSeq }

class YamlValue {
    public YTag tag;
    public bool b;
    public long i;
    public double f;
    public string s;
    public Dictionary<string, YamlValue> m;
    public List<YamlValue> v;

    public static YamlValue Null() => new YamlValue { tag = YTag.YNull };
    public static YamlValue Bool(bool b) => new YamlValue { tag = YTag.YBool, b = b };
    public static YamlValue Int(long i) => new YamlValue { tag = YTag.YInt, i = i };
    public static YamlValue Float(double f) => new YamlValue { tag = YTag.YFloat, f = f };
    public static YamlValue Str(string s) => new YamlValue { tag = YTag.YStr, s = s };
    public static YamlValue Map(Dictionary<string, YamlValue> m) => new YamlValue { tag = YTag.YMap, m = m };
    public static YamlValue Seq(List<YamlValue> v) => new YamlValue { tag = YTag.YSeq, v = v };

    public YamlValue Get(string key) {
        if (tag == YTag.YMap && m.ContainsKey(key)) return m[key];
        return Null();
    }
    public YamlValue At(int idx) {
        if (tag == YTag.YSeq && idx < v.Count) return v[idx];
        return Null();
    }
    public string ToStr() => tag == YTag.YStr ? s : "";
}

// ── Schema Coercion ──

static YamlValue CoerceScalar(string s) {
    if (s == "null" || s == "Null" || s == "NULL" || s == "~" || s == "")
        return YamlValue.Null();
    if (s == "true" || s == "True" || s == "TRUE") return YamlValue.Bool(true);
    if (s == "false" || s == "False" || s == "FALSE") return YamlValue.Bool(false);
    if (s == ".inf" || s == ".Inf" || s == ".INF" || s == "+.inf")
        return YamlValue.Float(double.PositiveInfinity);
    if (s == "-.inf" || s == "-.Inf" || s == "-.INF")
        return YamlValue.Float(double.NegativeInfinity);
    if (s == ".nan" || s == ".NaN" || s == ".NAN")
        return YamlValue.Float(double.NaN);
    if (long.TryParse(s, out long li)) return YamlValue.Int(li);
    if (s.StartsWith("0x") || s.StartsWith("0X")) {
        if (long.TryParse(s.Substring(2), System.Globalization.NumberStyles.HexNumber, null, out long hi))
            return YamlValue.Int(hi);
    }
    if (double.TryParse(s, System.Globalization.NumberStyles.Float,
        System.Globalization.CultureInfo.InvariantCulture, out double d))
        return YamlValue.Float(d);
    return YamlValue.Str(s);
}

// ── AST → Native Conversion with Anchor Resolution ──

class Converter {
    Dictionary<string, YamlValue> anchors = new Dictionary<string, YamlValue>();

    public YamlValue Convert(Ast node) {
        if (node == null) return YamlValue.Null();
        if (node.isLeaf) return CoerceScalar(node.text);
        string t = node.tag;
        if (t == "ANCHOR") {
            string name = null; YamlValue val = YamlValue.Null();
            foreach (var ch in node.children) {
                if (ch.isLeaf && name == null) name = ch.text;
                else val = Convert(ch);
            }
            if (name != null) anchors[name] = val;
            return val;
        }
        if (t == "ALIAS") {
            foreach (var ch in node.children)
                if (ch.isLeaf && anchors.ContainsKey(ch.text)) return anchors[ch.text];
            return YamlValue.Null();
        }
        if (t == "MAPPING") {
            var m = new Dictionary<string, YamlValue>();
            foreach (var ch in node.children) {
                if (ch.tag == "PAIR" && ch.children.Count >= 2) {
                    var key = Convert(ch.children[0]);
                    var val = Convert(ch.children[1]);
                    if (key.ToStr() == "<<" && val.tag == YTag.YMap) {
                        foreach (var kv in val.m) if (!m.ContainsKey(kv.Key)) m[kv.Key] = kv.Value;
                    } else m[key.ToStr()] = val;
                }
            }
            return YamlValue.Map(m);
        }
        if (t == "SEQUENCE") {
            var seq = new List<YamlValue>();
            foreach (var ch in node.children) seq.Add(Convert(ch));
            return YamlValue.Seq(seq);
        }
        if (t == "DOC" || t == "STREAM") {
            if (node.children.Count == 1) return Convert(node.children[0]);
            var docs = new List<YamlValue>();
            foreach (var ch in node.children) docs.Add(Convert(ch));
            return docs.Count == 1 ? docs[0] : YamlValue.Seq(docs);
        }
        if (t == "PAIR" && node.children.Count >= 2) return Convert(node.children[1]);
        if (node.children.Count == 1) return Convert(node.children[0]);
        var items = new List<YamlValue>();
        foreach (var ch in node.children) items.Add(Convert(ch));
        return YamlValue.Seq(items);
    }
}

// ── Public API ──

static YamlValue YamlLoad(string text) {
    var inp = new Input(text, 0, 1, 0);
    var r = l_yaml_stream(inp);
    if (r.failed || r.ast == null) return YamlValue.Null();
    return new Converter().Convert(r.ast);
}

// ── Main ──

public static void Main(string[] args) {
    string text;
    if (args.Length > 0) text = File.ReadAllText(args[0]);
    else text = Console.In.ReadToEnd();
    var inp = new Input(text, 0, 1, 0);
    var r = l_yaml_stream(inp);
    if (!r.failed) {
        Console.WriteLine("OK: " + r.rest.pos + " chars");
        if (r.ast != null) PrintAst(r.ast, 0);
    } else {
        Console.Error.WriteLine("FAIL @" + r.rest.pos + ": " + r.err);
        Environment.Exit(1);
    }
}
} // class YamlReader
