// ════════════════════════════════════════════════════════════════
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

public class JsonReader {

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

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
static Result json_text(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ws(_i0),
        (Input _i0) => value(_i0),
        (Input _i0) => ws(_i0),
        (Input _i0) => eof_ok(_i0)});
}

// [2] VALUE 
static Result value(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => r_object(_i0),
        (Input _i0) => array(_i0),
        (Input _i0) => r_string(_i0),
        (Input _i0) => number(_i0),
        (Input _i0) => match_str(_i0, "true"),
        (Input _i0) => match_str(_i0, "false"),
        (Input _i0) => match_str(_i0, "null")});
}

// [3] OBJECT 
static Result r_object(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => match_cp(_i0, 123),
            (Input _i0) => ws(_i0),
            (Input _i0) => members(_i0),
            (Input _i0) => ws(_i0),
            (Input _i0) => match_cp(_i0, 125)}),
        (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => match_cp(_i0, 123),
            (Input _i0) => ws(_i0),
            (Input _i0) => match_cp(_i0, 125)})});
}

// [4] MEMBERS 
static Result members(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => member(_i0),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => ws(_i0),
            (Input _i0) => match_cp(_i0, 44),
            (Input _i0) => ws(_i0),
            (Input _i0) => member(_i0)}))});
}

// [5] MEMBER 
static Result member(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => ws(_i0),
        (Input _i0) => r_string(_i0),
        (Input _i0) => ws(_i0),
        (Input _i0) => match_cp(_i0, 58),
        (Input _i0) => ws(_i0),
        (Input _i0) => value(_i0),
        (Input _i0) => ws(_i0)});
}

// [6] ARRAY 
static Result array(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => match_cp(_i0, 91),
            (Input _i0) => ws(_i0),
            (Input _i0) => elements(_i0),
            (Input _i0) => ws(_i0),
            (Input _i0) => match_cp(_i0, 93)}),
        (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => match_cp(_i0, 91),
            (Input _i0) => ws(_i0),
            (Input _i0) => match_cp(_i0, 93)})});
}

// [7] ELEMENTS 
static Result elements(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => value(_i0),
        (Input _i2) => star(_i2, (Input _i1) => Seq(_i1, new PFn[]{
            (Input _i0) => ws(_i0),
            (Input _i0) => match_cp(_i0, 44),
            (Input _i0) => ws(_i0),
            (Input _i0) => value(_i0)}))});
}

// [8] STRING 
static Result r_string(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 34),
        (Input _i1) => star(_i1, (Input _i0) => r_char(_i0)),
        (Input _i0) => match_cp(_i0, 34)});
}

// [9] CHAR 
static Result r_char(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => escaped(_i0),
        (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i1) => neg(_i1, (Input _i0) => match_cp(_i0, 34)),
            (Input _i1) => neg(_i1, (Input _i0) => match_cp(_i0, 92)),
            (Input _i1) => neg(_i1, (Input _i0) => match_cp(_i0, 0x0)),
            (Input _i1) => neg(_i1, (Input _i0) => match_range(_i0, 0x0, 0x1F)),
            (Input _i0) => match_range(_i0, 0x20, 0x10FFFF)})});
}

// [10] ESCAPED 
static Result escaped(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 92),
        (Input _i2) => Alt(_i2, new PFn[]{
            (Input _i0) => match_cp(_i0, 34),
            (Input _i0) => match_cp(_i0, 92),
            (Input _i0) => match_cp(_i0, 47),
            (Input _i0) => match_cp(_i0, 98),
            (Input _i0) => match_cp(_i0, 102),
            (Input _i0) => match_cp(_i0, 110),
            (Input _i0) => match_cp(_i0, 114),
            (Input _i0) => match_cp(_i0, 116),
            (Input _i1) => Seq(_i1, new PFn[]{(Input _i0) => match_cp(_i0, 117), (Input _i0) => hex4(_i0)})})});
}

// [11] HEX4 
static Result hex4(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => hexdig(_i0),
        (Input _i0) => hexdig(_i0),
        (Input _i0) => hexdig(_i0),
        (Input _i0) => hexdig(_i0)});
}

// [12] HEXDIG 
static Result hexdig(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => match_range(_i0, 48, 57),
        (Input _i0) => match_range(_i0, 97, 102),
        (Input _i0) => match_range(_i0, 65, 70)});
}

// [13] NUMBER 
static Result number(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i1) => opt(_i1, (Input _i0) => match_cp(_i0, 45)),
        (Input _i0) => integer(_i0),
        (Input _i1) => opt(_i1, (Input _i0) => fraction(_i0)),
        (Input _i1) => opt(_i1, (Input _i0) => exponent(_i0))});
}

// [14] INTEGER 
static Result integer(Input inp) {
    return Alt(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 48),
        (Input _i2) => Seq(_i2, new PFn[]{
            (Input _i0) => match_range(_i0, 49, 57),
            (Input _i1) => star(_i1, (Input _i0) => match_range(_i0, 48, 57))})});
}

// [15] FRACTION 
static Result fraction(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i0) => match_cp(_i0, 46),
        (Input _i1) => plus_(_i1, (Input _i0) => match_range(_i0, 48, 57))});
}

// [16] EXPONENT 
static Result exponent(Input inp) {
    return Seq(inp, new PFn[]{
        (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => match_cp(_i0, 101), (Input _i0) => match_cp(_i0, 69)}),
        (Input _i2) => opt(_i2, (Input _i1) => Alt(_i1, new PFn[]{(Input _i0) => match_cp(_i0, 43), (Input _i0) => match_cp(_i0, 45)})),
        (Input _i1) => plus_(_i1, (Input _i0) => match_range(_i0, 48, 57))});
}

// [17] WS 
static Result ws(Input inp) {
    return star(inp, (Input _i1) => Alt(_i1, new PFn[]{
        (Input _i0) => match_cp(_i0, 0x20),
        (Input _i0) => match_cp(_i0, 0x9),
        (Input _i0) => match_cp(_i0, 0x0A),
        (Input _i0) => match_cp(_i0, 0x0D)}));
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

// ── Main ──

public static void Main(string[] args) {
    string text;
    if (args.Length > 0) text = File.ReadAllText(args[0], System.Text.Encoding.UTF8);
    else text = Console.In.ReadToEnd();
    var inp = new Input(text, 0, 1, 0);
    var r = json_text(inp);
    if (!r.failed) {
        Console.WriteLine("OK: " + r.rest.pos + " chars");
        if (r.ast != null) PrintAst(r.ast, 0);
    } else {
        Console.Error.WriteLine("FAIL @" + r.rest.pos + ": " + r.err);
        Environment.Exit(1);
    }
}
} // class JsonReader
