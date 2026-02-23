;;;; peg-csharp.lisp — C# target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "C#")
(def-tgt "default-output" "YamlReader.cs")
(def-tgt "comment-prefix" "//")

(def-tgt "keywords"
  '("abstract" "as" "base" "bool" "break" "byte" "case" "catch" "char"
    "checked" "class" "const" "continue" "decimal" "default" "delegate"
    "do" "double" "else" "enum" "event" "explicit" "extern" "false"
    "finally" "fixed" "float" "for" "foreach" "goto" "if" "implicit"
    "in" "int" "interface" "internal" "is" "lock" "long" "namespace"
    "new" "null" "object" "operator" "out" "override" "params" "private"
    "protected" "public" "readonly" "ref" "return" "sbyte" "sealed"
    "short" "sizeof" "stackalloc" "static" "string" "struct" "switch"
    "this" "throw" "true" "try" "typeof" "uint" "ulong" "unchecked"
    "unsafe" "ushort" "using" "virtual" "void" "volatile" "while"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(defun cs-sub-inp (s old new_)
  "Replace exact word OLD with NEW_ in S, respecting word boundaries."
  (with-output-to-string (out)
    (let ((len (length s)) (olen (length old)))
      (loop with i = 0 while (< i len) do
        (if (and (<= (+ i olen) len)
                 (string= (subseq s i (+ i olen)) old)
                 (or (= i 0) (not (alphanumericp (char s (1- i)))))
                 (or (= (+ i olen) len) (not (alphanumericp (char s (+ i olen))))))
            (progn (write-string new_ out) (incf i olen))
            (progn (write-char (char s i) out) (incf i)))))))

(defun cs-next-iname (body)
  "Find next available _iN name not used in BODY."
  (loop for n from 0
        for name = (format nil "_i~D" n)
        unless (search name body) return name))

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (let* ((pname (cs-next-iname body))
           (body2 (cs-sub-inp body "inp" pname)))
      (format nil "(Input ~A) => ~A" pname body2))))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (let* ((pname (cs-next-iname body))
           (body2 (cs-sub-inp body "inp" pname)))
      (format nil "(Input ~A) => ~A" pname body2))))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "Seq(inp, new PFn[]{~{~A~^, ~}})" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "Alt(inp, new PFn[]{~{~A~^, ~}})" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "((Func<Result>)(() => {~{ if (~A == ~S) return ~A;~} return Fail(inp, \"no case\"); }))()"
            (loop for (val body) in cases
                  collect param collect val collect body))))

;;; ── Let ──

(defvar *cs-let-counter* 0)

(def-tgt "let-int"
  (lambda (vname expr rest)
    (let ((rn (format nil "r~D_" (incf *cs-let-counter*))))
      (format nil "((Func<Result>)(() => { var ~A = ~A; if (~A.failed) return ~A; int ~A = ~A.tagInt; inp = ~A.rest; return ~A; }))()"
              rn expr rn rn vname rn rn rest))))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (let ((rn (format nil "r~D_" (incf *cs-let-counter*))))
      (format nil "((Func<Result>)(() => { var ~A = ~A; if (~A.failed) return ~A; string ~A = ~A.tag; inp = ~A.rest; return ~A; }))()"
              rn expr rn rn vname rn rn rest))))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "(int)~A" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "InFlow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "SeqSpaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(Input inp~{, ~A~})" name
                (mapcar (lambda (p)
                          (let ((pn (symbol-name p)))
                            (if (member pn '("N" "M") :test #'string-equal)
                                (format nil "int ~A" (peg-ident p))
                                (format nil "string ~A" (peg-ident p)))))
                        params))
        (format nil "~A(Input inp)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "static Result ~A {~%    return ~A;~%}" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

public class YamlReader {")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Input ──

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
    bool nl = c == '\\n';
    int len = char.IsHighSurrogate(c) ? 2 : 1;
    return new Input(i.src, i.pos + len, nl ? i.line + 1 : i.line, nl ? 0 : i.col + 1);
}"

"// ── AST ──

class Ast {
    public string tag;
    public string text;
    public List<Ast> children = new List<Ast>();
    public bool isLeaf;
    public static Ast Branch(string t) => new Ast { tag = t };
    public static Ast Leaf(string t) => new Ast { text = t, isLeaf = true, tag = \"SCALAR\" };
}"

"// ── Result ──

class Result {
    public bool failed;
    public string val = \"\";
    public Input rest;
    public string tag = \"\";
    public int tagInt;
    public Ast ast;
    public List<Ast> astList;
    public string err = \"\";
}

static Result Ok(Input inp) => new Result { rest = inp };
static Result OkV(Input inp, string v) => new Result { val = v, rest = inp };
static Result Fail(Input inp, string m) => new Result { failed = true, rest = inp, err = m };"

"// ── Context ──

static string InFlow(string c) => (c == \"FLOW-OUT\" || c == \"FLOW-IN\") ? \"FLOW-IN\" : \"FLOW-KEY\";
static int SeqSpaces(int n, string c) => c == \"BLOCK-OUT\" ? n - 1 : n;"

"// ── Combinators ──

delegate Result PFn(Input inp);

static Result match_cp(Input inp, int cp) {
    int c = PeekCp(inp);
    if (c == cp) {
        string s = char.ConvertFromUtf32(c);
        Input cur = inp;
        for (int i = 0; i < s.Length; i++) cur = Adv(cur);
        return OkV(cur, s);
    }
    return Fail(inp, \"cp\");
}

static Result match_range(Input inp, int lo, int hi) {
    int c = PeekCp(inp);
    if (c >= lo && c <= hi) {
        string s = char.ConvertFromUtf32(c);
        Input cur = inp;
        for (int i = 0; i < s.Length; i++) cur = Adv(cur);
        return OkV(cur, s);
    }
    return Fail(inp, \"rng\");
}

static Result match_str(Input inp, string t) {
    if (inp.pos + t.Length > inp.src.Length) return Fail(inp, \"str\");
    if (inp.src.Substring(inp.pos, t.Length) != t) return Fail(inp, \"str\");
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
    return Fail(inp, \"alt\");
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
static Result neg(Input inp, PFn f) { var r = f(inp); return r.failed ? Ok(inp) : Fail(inp, \"neg\"); }
static Result minus(Input inp, PFn fa, PFn fb) {
    var ra = fa(inp); if (ra.failed) return ra;
    var rb = fb(inp); if (!rb.failed && rb.rest.pos == ra.rest.pos) return Fail(inp, \"excl\"); return ra;
}
static Result rep(Input inp, int n, PFn f) {
    Input cur = inp; var acc = new StringBuilder();
    for (int i = 0; i < n; i++) { var r = f(cur); if (r.failed) return r; acc.Append(r.val); cur = r.rest; }
    return OkV(cur, acc.ToString());
}
static Result ahead(Input inp, PFn f) { var r = f(inp); return r.failed ? r : Ok(inp); }
static Result behind(Input inp, PFn f) {
    if (inp.pos == 0) return Fail(inp, \"bh\");
    var t = new Input(inp.src, inp.pos - 1, inp.line, Math.Max(0, inp.col - 1));
    var r = f(t); return r.failed ? Fail(inp, \"bh\") : Ok(inp);
}
static Result sol(Input inp) => inp.col == 0 ? Ok(inp) : Fail(inp, \"sol\");
static Result eof_ok(Input inp) => AtEof(inp) ? Ok(inp) : Fail(inp, \"eof\");
static Result ok(Input inp) => Ok(inp);"

"// ── YAML extensions ──

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
    if (i + sp < len && s[i + sp] != '\\n') {
        var r = Ok(inp); r.tagInt = Math.Max(1, sp - n); return r;
    }
    int j = i + sp;
    while (j < len) {
        if (j >= len) { var r = Ok(inp); r.tagInt = 1; return r; }
        if (s[j] == '\\n') {
            j++;
            if (j >= len) { var r = Ok(inp); r.tagInt = 1; return r; }
            sp = 0; while (j + sp < len && s[j + sp] == ' ') sp++;
            int nx = j + sp;
            if (nx >= len || s[nx] == '\\n') { j = nx; continue; }
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

static Result val(Input inp, string v) { var r = Ok(inp); r.tag = v; return r; }"))

;;; ── API ──

(def-tgt "api"
"// ── API ──

static void PrintAst(Ast node, int depth) {
    string indent = new string(' ', depth * 2);
    if (node.isLeaf) { Console.WriteLine(indent + \"SCALAR: \\\"\" + node.text + \"\\\"\"); }
    else {
        Console.WriteLine(indent + node.tag);
        foreach (var c in node.children) PrintAst(c, depth + 1);
    }
}")

(def-tgt "namespace-close" nil)

;;; ── Main ──

(def-tgt "main-fn"
"// ── Main ──

public static void Main(string[] args) {
    string text;
    if (args.Length > 0) text = File.ReadAllText(args[0]);
    else text = Console.In.ReadToEnd();
    var inp = new Input(text, 0, 1, 0);
    var r = l_yaml_stream(inp);
    if (!r.failed) {
        Console.WriteLine(\"OK: \" + r.rest.pos + \" chars\");
        if (r.ast != null) PrintAst(r.ast, 0);
    } else {
        Console.Error.WriteLine(\"FAIL @\" + r.rest.pos + \": \" + r.err);
        Environment.Exit(1);
    }
}
} // class YamlReader")

;;; ── Concern Vocab ──

(let ((cv (make-hash-table :test 'equal)))
  (setf (gethash "value-type-decl" cv)
"// ── Native Value Type ──

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
    public string ToStr() => tag == YTag.YStr ? s : \"\";
}")

  (setf (gethash "coerce-fn" cv)
"static YamlValue CoerceScalar(string s) {
    if (s == \"null\" || s == \"Null\" || s == \"NULL\" || s == \"~\" || s == \"\")
        return YamlValue.Null();
    if (s == \"true\" || s == \"True\" || s == \"TRUE\") return YamlValue.Bool(true);
    if (s == \"false\" || s == \"False\" || s == \"FALSE\") return YamlValue.Bool(false);
    if (s == \".inf\" || s == \".Inf\" || s == \".INF\" || s == \"+.inf\")
        return YamlValue.Float(double.PositiveInfinity);
    if (s == \"-.inf\" || s == \"-.Inf\" || s == \"-.INF\")
        return YamlValue.Float(double.NegativeInfinity);
    if (s == \".nan\" || s == \".NaN\" || s == \".NAN\")
        return YamlValue.Float(double.NaN);
    if (long.TryParse(s, out long li)) return YamlValue.Int(li);
    if (s.StartsWith(\"0x\") || s.StartsWith(\"0X\")) {
        if (long.TryParse(s.Substring(2), System.Globalization.NumberStyles.HexNumber, null, out long hi))
            return YamlValue.Int(hi);
    }
    if (double.TryParse(s, System.Globalization.NumberStyles.Float,
        System.Globalization.CultureInfo.InvariantCulture, out double d))
        return YamlValue.Float(d);
    return YamlValue.Str(s);
}")

  (setf (gethash "converter-decl" cv)
"class Converter {
    Dictionary<string, YamlValue> anchors = new Dictionary<string, YamlValue>();")

  (setf (gethash "convert-fn" cv)
"    public YamlValue Convert(Ast node) {
        if (node == null) return YamlValue.Null();
        if (node.isLeaf) return CoerceScalar(node.text);
        string t = node.tag;
        if (t == \"ANCHOR\") {
            string name = null; YamlValue val = YamlValue.Null();
            foreach (var ch in node.children) {
                if (ch.isLeaf && name == null) name = ch.text;
                else val = Convert(ch);
            }
            if (name != null) anchors[name] = val;
            return val;
        }
        if (t == \"ALIAS\") {
            foreach (var ch in node.children)
                if (ch.isLeaf && anchors.ContainsKey(ch.text)) return anchors[ch.text];
            return YamlValue.Null();
        }
        if (t == \"MAPPING\") {
            var m = new Dictionary<string, YamlValue>();
            foreach (var ch in node.children) {
                if (ch.tag == \"PAIR\" && ch.children.Count >= 2) {
                    var key = Convert(ch.children[0]);
                    var val = Convert(ch.children[1]);
                    if (key.ToStr() == \"<<\" && val.tag == YTag.YMap) {
                        foreach (var kv in val.m) if (!m.ContainsKey(kv.Key)) m[kv.Key] = kv.Value;
                    } else m[key.ToStr()] = val;
                }
            }
            return YamlValue.Map(m);
        }
        if (t == \"SEQUENCE\") {
            var seq = new List<YamlValue>();
            foreach (var ch in node.children) seq.Add(Convert(ch));
            return YamlValue.Seq(seq);
        }
        if (t == \"DOC\" || t == \"STREAM\") {
            if (node.children.Count == 1) return Convert(node.children[0]);
            var docs = new List<YamlValue>();
            foreach (var ch in node.children) docs.Add(Convert(ch));
            return docs.Count == 1 ? docs[0] : YamlValue.Seq(docs);
        }
        if (t == \"PAIR\" && node.children.Count >= 2) return Convert(node.children[1]);
        if (node.children.Count == 1) return Convert(node.children[0]);
        var items = new List<YamlValue>();
        foreach (var ch in node.children) items.Add(Convert(ch));
        return YamlValue.Seq(items);
    }
}")

  (setf (gethash "load-fn" cv)
"static YamlValue YamlLoad(string text) {
    var inp = new Input(text, 0, 1, 0);
    var r = l_yaml_stream(inp);
    if (r.failed || r.ast == null) return YamlValue.Null();
    return new Converter().Convert(r.ast);
}")
  (def-tgt "cv" cv))
