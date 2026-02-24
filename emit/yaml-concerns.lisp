;;;; yaml-concerns.lisp — Post-parse concern layer
;;;;
;;;; Three concerns that turn an AST into a drop-in YAML library:
;;;;
;;;;   1. Native conversion  — MAPPING→map, SEQUENCE→vec, SCALAR→string
;;;;   2. Schema coercion    — "true"→bool, "42"→int, "~"→null
;;;;   3. Anchor resolution  — &anchor/*alias → resolved references
;;;;
;;;; Each target spec provides a "yaml-concerns" string block.
;;;; The projector emits it after the rules and before main.
;;;;
;;;; This file contains the concern implementations for each language.
;;;; They are loaded by the PEG specs.

(in-package #:yaml-eval)

;;; ═══════════════════════════════════════════════════════════════════
;;; C++ Concerns
;;; ═══════════════════════════════════════════════════════════════════

(defparameter *yaml-concerns-cpp*
"// ── Native Value Type ──

struct YamlValue;
using YamlMap = std::map<std::string, YamlValue>;
using YamlSeq = std::vector<YamlValue>;

struct YamlValue {
    enum Tag { Null, Bool, Int, Float, Str, Map, Seq } tag = Null;
    bool    b = false;
    int64_t i = 0;
    double  f = 0.0;
    std::string s;
    YamlMap m;
    YamlSeq v;

    static YamlValue null_val()         { return {}; }
    static YamlValue from_bool(bool x)  { YamlValue y; y.tag=Bool;  y.b=x; return y; }
    static YamlValue from_int(int64_t x){ YamlValue y; y.tag=Int;   y.i=x; return y; }
    static YamlValue from_float(double x){YamlValue y; y.tag=Float; y.f=x; return y; }
    static YamlValue from_str(const std::string& x) { YamlValue y; y.tag=Str; y.s=x; return y; }
    static YamlValue from_map(YamlMap x){ YamlValue y; y.tag=Map;   y.m=std::move(x); return y; }
    static YamlValue from_seq(YamlSeq x){ YamlValue y; y.tag=Seq;   y.v=std::move(x); return y; }

    // Subscript for maps
    YamlValue& operator[](const std::string& key) { return m[key]; }
    const YamlValue& operator[](const std::string& key) const {
        static YamlValue empty;
        auto it = m.find(key); return it != m.end() ? it->second : empty;
    }
    // Subscript for sequences
    YamlValue& operator[](size_t idx) { return v[idx]; }
    const YamlValue& operator[](size_t idx) const { return v[idx]; }

    // Convenience
    size_t size() const { return tag==Map ? m.size() : tag==Seq ? v.size() : 0; }
    bool empty() const { return tag==Null; }
    explicit operator bool() const { return tag!=Null; }
    std::string str() const { return tag==Str ? s : \"\"; }
};

// ── Schema Coercion (YAML Core Schema) ──

YamlValue coerce_scalar(const std::string& s) {
    // Null
    if (s==\"null\" || s==\"Null\" || s==\"NULL\" || s==\"~\" || s.empty())
        return YamlValue::null_val();
    // Bool
    if (s==\"true\"  || s==\"True\"  || s==\"TRUE\")  return YamlValue::from_bool(true);
    if (s==\"false\" || s==\"False\" || s==\"FALSE\") return YamlValue::from_bool(false);
    // Int (decimal, octal 0o, hex 0x)
    if (!s.empty() && (std::isdigit(s[0]) || s[0]=='+' || s[0]=='-')) {
        char* end = nullptr;
        if (s.size()>2 && s[0]=='0' && s[1]=='x') {
            int64_t v = std::strtoll(s.c_str(), &end, 16);
            if (end && *end=='\\0') return YamlValue::from_int(v);
        } else if (s.size()>2 && s[0]=='0' && s[1]=='o') {
            int64_t v = std::strtoll(s.c_str()+2, &end, 8);
            if (end && *end=='\\0') return YamlValue::from_int(v);
        } else if (s.find('.')==std::string::npos && s.find('e')==std::string::npos && s.find('E')==std::string::npos) {
            int64_t v = std::strtoll(s.c_str(), &end, 10);
            if (end && *end=='\\0') return YamlValue::from_int(v);
        } else {
            double v = std::strtod(s.c_str(), &end);
            if (end && *end=='\\0') return YamlValue::from_float(v);
        }
    }
    // .inf, .nan
    if (s==\".inf\" || s==\".Inf\" || s==\".INF\" || s==\"+.inf\") return YamlValue::from_float(1.0/0.0);
    if (s==\"-.inf\" || s==\"-.Inf\" || s==\"-.INF\") return YamlValue::from_float(-1.0/0.0);
    if (s==\".nan\" || s==\".NaN\" || s==\".NAN\") return YamlValue::from_float(0.0/0.0);
    // String (default)
    return YamlValue::from_str(s);
}

// ── AST → Native Conversion with Anchor Resolution ──

struct Converter {
    std::unordered_map<std::string, YamlValue> anchors;

    YamlValue convert(const AST& node) {
        if (!node) return YamlValue::null_val();
        const auto& t = node->type;

        if (t==\"SCALAR\") return coerce_scalar(node->text);

        if (t==\"ANCHOR\") {
            // First child is anchor name, rest is the value
            std::string name;
            YamlValue val = YamlValue::null_val();
            for (auto& c : node->children) {
                if (c->type==\"SCALAR\" && name.empty()) name = c->text;
                else val = convert(c);
            }
            if (!name.empty()) anchors[name] = val;
            return val;
        }

        if (t==\"ALIAS\") {
            for (auto& c : node->children) {
                if (c->type==\"SCALAR\") {
                    auto it = anchors.find(c->text);
                    if (it != anchors.end()) return it->second;
                }
            }
            return YamlValue::null_val();
        }

        if (t==\"MAPPING\") {
            YamlMap m;
            for (auto& c : node->children) {
                if (c->type==\"PAIR\" && c->children.size()>=2) {
                    auto key = convert(c->children[0]);
                    auto val = convert(c->children[1]);
                    // Merge key support (<<)
                    if (key.tag==YamlValue::Str && key.s==\"<<\" && val.tag==YamlValue::Map) {
                        for (auto& [mk,mv] : val.m) {
                            if (m.find(mk)==m.end()) m[mk] = mv;
                        }
                    } else {
                        m[key.str()] = val;
                    }
                }
            }
            return YamlValue::from_map(std::move(m));
        }

        if (t==\"SEQUENCE\") {
            YamlSeq seq;
            for (auto& c : node->children) seq.push_back(convert(c));
            return YamlValue::from_seq(std::move(seq));
        }

        if (t==\"DOC\") {
            if (node->children.size()==1) return convert(node->children[0]);
            YamlSeq docs;
            for (auto& c : node->children) docs.push_back(convert(c));
            return docs.size()==1 ? docs[0] : YamlValue::from_seq(std::move(docs));
        }

        if (t==\"STREAM\") {
            if (node->children.size()==1) return convert(node->children[0]);
            YamlSeq docs;
            for (auto& c : node->children) docs.push_back(convert(c));
            return docs.size()==1 ? docs[0] : YamlValue::from_seq(std::move(docs));
        }

        if (t==\"PAIR\") {
            // Bare pair outside mapping — return value
            if (node->children.size()>=2) return convert(node->children[1]);
        }

        // Fallback: recurse into children
        if (node->children.size()==1) return convert(node->children[0]);
        YamlSeq items;
        for (auto& c : node->children) items.push_back(convert(c));
        return YamlValue::from_seq(std::move(items));
    }
};

// ── Public API ──

YamlValue load(const std::string& text) {
    auto result = parse(text);
    if (!result.success) return YamlValue::null_val();
    Converter conv;
    return conv.convert(result.ast);
}

YamlValue load_file(const std::string& path) {
    std::ifstream f(path);
    if (!f) return YamlValue::null_val();
    std::ostringstream ss; ss << f.rdbuf();
    return load(ss.str());
}")

;;; ═══════════════════════════════════════════════════════════════════
;;; Go Concerns
;;; ═══════════════════════════════════════════════════════════════════

(defparameter *yaml-concerns-go*
"// ── Native Value Type ──

type YamlTag int
const (
    YNull YamlTag = iota
    YBool
    YInt
    YFloat
    YStr
    YMap
    YSeq
)

type YamlValue struct {
    Tag YamlTag
    B   bool
    I   int64
    F   float64
    S   string
    M   map[string]*YamlValue
    V   []*YamlValue
}

func NullVal() *YamlValue { return &YamlValue{Tag: YNull} }
func BoolVal(b bool) *YamlValue { return &YamlValue{Tag: YBool, B: b} }
func IntVal(i int64) *YamlValue { return &YamlValue{Tag: YInt, I: i} }
func FloatVal(f float64) *YamlValue { return &YamlValue{Tag: YFloat, F: f} }
func StrVal(s string) *YamlValue { return &YamlValue{Tag: YStr, S: s} }
func MapVal(m map[string]*YamlValue) *YamlValue { return &YamlValue{Tag: YMap, M: m} }
func SeqVal(v []*YamlValue) *YamlValue { return &YamlValue{Tag: YSeq, V: v} }

func (y *YamlValue) Get(key string) *YamlValue {
    if y.Tag == YMap { if v, ok := y.M[key]; ok { return v } }
    return NullVal()
}
func (y *YamlValue) At(i int) *YamlValue {
    if y.Tag == YSeq && i < len(y.V) { return y.V[i] }
    return NullVal()
}
func (y *YamlValue) Str() string { if y.Tag == YStr { return y.S }; return \"\" }
func (y *YamlValue) Size() int {
    if y.Tag == YMap { return len(y.M) }; if y.Tag == YSeq { return len(y.V) }; return 0
}

// ── Schema Coercion ──

func coerceScalar(s string) *YamlValue {
    switch s {
    case \"null\", \"Null\", \"NULL\", \"~\", \"\":
        return NullVal()
    case \"true\", \"True\", \"TRUE\":
        return BoolVal(true)
    case \"false\", \"False\", \"FALSE\":
        return BoolVal(false)
    case \".inf\", \".Inf\", \".INF\", \"+.inf\":
        return FloatVal(math.Inf(1))
    case \"-.inf\", \"-.Inf\", \"-.INF\":
        return FloatVal(math.Inf(-1))
    case \".nan\", \".NaN\", \".NAN\":
        return FloatVal(math.NaN())
    }
    if i, err := strconv.ParseInt(s, 0, 64); err == nil { return IntVal(i) }
    if f, err := strconv.ParseFloat(s, 64); err == nil { return FloatVal(f) }
    return StrVal(s)
}

// ── AST → Native Conversion with Anchor Resolution ──

type converter struct { anchors map[string]*YamlValue }

func (c *converter) convert(node *ASTNode) *YamlValue {
    if node == nil { return NullVal() }
    switch node.Type {
    case \"SCALAR\":
        return coerceScalar(node.Text)
    case \"ANCHOR\":
        var name string
        var val *YamlValue = NullVal()
        for _, ch := range node.Children {
            if ch.Type == \"SCALAR\" && name == \"\" { name = ch.Text } else { val = c.convert(ch) }
        }
        if name != \"\" { c.anchors[name] = val }
        return val
    case \"ALIAS\":
        for _, ch := range node.Children {
            if ch.Type == \"SCALAR\" { if v, ok := c.anchors[ch.Text]; ok { return v } }
        }
        return NullVal()
    case \"MAPPING\":
        m := make(map[string]*YamlValue)
        for _, ch := range node.Children {
            if ch.Type == \"PAIR\" && len(ch.Children) >= 2 {
                key := c.convert(ch.Children[0])
                val := c.convert(ch.Children[1])
                if key.Tag == YStr && key.S == \"<<\" && val.Tag == YMap {
                    for mk, mv := range val.M { if _, exists := m[mk]; !exists { m[mk] = mv } }
                } else { m[key.Str()] = val }
            }
        }
        return MapVal(m)
    case \"SEQUENCE\":
        var seq []*YamlValue
        for _, ch := range node.Children { seq = append(seq, c.convert(ch)) }
        return SeqVal(seq)
    case \"DOC\", \"STREAM\":
        if len(node.Children) == 1 { return c.convert(node.Children[0]) }
        var docs []*YamlValue
        for _, ch := range node.Children { docs = append(docs, c.convert(ch)) }
        if len(docs) == 1 { return docs[0] }
        return SeqVal(docs)
    }
    if len(node.Children) == 1 { return c.convert(node.Children[0]) }
    var items []*YamlValue
    for _, ch := range node.Children { items = append(items, c.convert(ch)) }
    return SeqVal(items)
}

// ── Public API ──

func Load(text string) *YamlValue {
    result := Parse(text)
    if !result.Success { return NullVal() }
    c := &converter{anchors: make(map[string]*YamlValue)}
    return c.convert(result.AST)
}")

;;; ═══════════════════════════════════════════════════════════════════
;;; Python Concerns
;;; ═══════════════════════════════════════════════════════════════════

(defparameter *yaml-concerns-python*
"# ── Schema Coercion ──

import math

def coerce_scalar(s):
    if s in ('null', 'Null', 'NULL', '~', ''):
        return None
    if s in ('true', 'True', 'TRUE'):
        return True
    if s in ('false', 'False', 'FALSE'):
        return False
    if s in ('.inf', '.Inf', '.INF', '+.inf'):
        return float('inf')
    if s in ('-.inf', '-.Inf', '-.INF'):
        return float('-inf')
    if s in ('.nan', '.NaN', '.NAN'):
        return float('nan')
    try:
        return int(s, 0)
    except (ValueError, TypeError):
        pass
    try:
        return float(s)
    except (ValueError, TypeError):
        pass
    return s

# ── AST → Native Conversion with Anchor Resolution ──

class Converter:
    def __init__(self):
        self.anchors = {}

    def convert(self, node):
        if node is None:
            return None
        t = node.type

        if t == 'SCALAR':
            return coerce_scalar(node.text)

        if t == 'ANCHOR':
            name = None
            val = None
            for c in node.children:
                if c.type == 'SCALAR' and name is None:
                    name = c.text
                else:
                    val = self.convert(c)
            if name:
                self.anchors[name] = val
            return val

        if t == 'ALIAS':
            for c in node.children:
                if c.type == 'SCALAR' and c.text in self.anchors:
                    return self.anchors[c.text]
            return None

        if t == 'MAPPING':
            m = {}
            for c in node.children:
                if c.type == 'PAIR' and len(c.children) >= 2:
                    key = self.convert(c.children[0])
                    val = self.convert(c.children[1])
                    if key == '<<' and isinstance(val, dict):
                        for mk, mv in val.items():
                            if mk not in m:
                                m[mk] = mv
                    else:
                        m[str(key) if key is not None else ''] = val
            return m

        if t == 'SEQUENCE':
            return [self.convert(c) for c in node.children]

        if t in ('DOC', 'STREAM'):
            if len(node.children) == 1:
                return self.convert(node.children[0])
            docs = [self.convert(c) for c in node.children]
            return docs[0] if len(docs) == 1 else docs

        if len(node.children) == 1:
            return self.convert(node.children[0])
        return [self.convert(c) for c in node.children]

# ── Public API ──

def load(text):
    result = parse(text)
    if not result.success:
        return None
    return Converter().convert(result.ast)

def load_file(path):
    with open(path) as f:
        return load(f.read())")

;;; ═══════════════════════════════════════════════════════════════════
;;; Rust Concerns
;;; ═══════════════════════════════════════════════════════════════════

(defparameter *yaml-concerns-rust*
"// ── Native Value Type ──

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum YamlValue {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Map(HashMap<String, YamlValue>),
    Seq(Vec<YamlValue>),
}

impl YamlValue {
    fn get(&self, key: &str) -> &YamlValue {
        match self { YamlValue::Map(m) => m.get(key).unwrap_or(&YamlValue::Null), _ => &YamlValue::Null }
    }
    fn at(&self, i: usize) -> &YamlValue {
        match self { YamlValue::Seq(v) => v.get(i).unwrap_or(&YamlValue::Null), _ => &YamlValue::Null }
    }
    fn as_str(&self) -> &str {
        match self { YamlValue::Str(s) => s.as_str(), _ => \"\" }
    }
    fn size(&self) -> usize {
        match self { YamlValue::Map(m) => m.len(), YamlValue::Seq(v) => v.len(), _ => 0 }
    }
}

// ── Schema Coercion ──

fn coerce_scalar(s: &str) -> YamlValue {
    match s {
        \"null\" | \"Null\" | \"NULL\" | \"~\" | \"\" => YamlValue::Null,
        \"true\" | \"True\" | \"TRUE\" => YamlValue::Bool(true),
        \"false\" | \"False\" | \"FALSE\" => YamlValue::Bool(false),
        \".inf\" | \".Inf\" | \".INF\" | \"+.inf\" => YamlValue::Float(f64::INFINITY),
        \"-.inf\" | \"-.Inf\" | \"-.INF\" => YamlValue::Float(f64::NEG_INFINITY),
        \".nan\" | \".NaN\" | \".NAN\" => YamlValue::Float(f64::NAN),
        _ => {
            if let Ok(i) = s.parse::<i64>() { return YamlValue::Int(i); }
            if let Ok(f) = s.parse::<f64>() { return YamlValue::Float(f); }
            YamlValue::Str(s.to_string())
        }
    }
}

// ── AST → Native Conversion with Anchor Resolution ──

struct YamlConverter { anchors: HashMap<String, YamlValue> }

impl YamlConverter {
    fn new() -> Self { Self { anchors: HashMap::new() } }

    fn convert(&mut self, node: &ASTNode) -> YamlValue {
        match node.node_type.as_str() {
            \"SCALAR\" => coerce_scalar(&node.text),
            \"ANCHOR\" => {
                let mut name = String::new();
                let mut val = YamlValue::Null;
                for c in &node.children {
                    if c.node_type == \"SCALAR\" && name.is_empty() { name = c.text.clone(); }
                    else { val = self.convert(c); }
                }
                if !name.is_empty() { self.anchors.insert(name, val.clone()); }
                val
            }
            \"ALIAS\" => {
                for c in &node.children {
                    if c.node_type == \"SCALAR\" {
                        if let Some(v) = self.anchors.get(&c.text) { return v.clone(); }
                    }
                }
                YamlValue::Null
            }
            \"MAPPING\" => {
                let mut m = HashMap::new();
                for c in &node.children {
                    if c.node_type == \"PAIR\" && c.children.len() >= 2 {
                        let key = self.convert(&c.children[0]);
                        let val = self.convert(&c.children[1]);
                        if let (YamlValue::Str(ref k), YamlValue::Map(ref merge)) = (&key, &val) {
                            if k == \"<<\" {
                                for (mk, mv) in merge {
                                    m.entry(mk.clone()).or_insert_with(|| mv.clone());
                                }
                                continue;
                            }
                        }
                        m.insert(key.as_str().to_string(), val);
                    }
                }
                YamlValue::Map(m)
            }
            \"SEQUENCE\" => YamlValue::Seq(node.children.iter().map(|c| self.convert(c)).collect()),
            \"DOC\" | \"STREAM\" => {
                if node.children.len() == 1 { return self.convert(&node.children[0]); }
                let docs: Vec<_> = node.children.iter().map(|c| self.convert(c)).collect();
                if docs.len() == 1 { docs.into_iter().next().unwrap() } else { YamlValue::Seq(docs) }
            }
            _ => {
                if node.children.len() == 1 { return self.convert(&node.children[0]); }
                YamlValue::Seq(node.children.iter().map(|c| self.convert(c)).collect())
            }
        }
    }
}

// ── Public API ──

fn load(text: &str) -> YamlValue {
    let result = parse(text);
    if !result.success { return YamlValue::Null; }
    if let Some(ast) = result.ast { YamlConverter::new().convert(&ast) } else { YamlValue::Null }
}")
