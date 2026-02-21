;;;; peg-python.lisp — Python target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Python")
(def-tgt "default-output" "yaml_reader.py")

(def-tgt "keywords"
  '("False" "None" "True" "and" "as" "assert" "async" "await"
    "break" "class" "continue" "def" "del" "elif" "else" "except"
    "finally" "for" "from" "global" "if" "import" "in" "is"
    "lambda" "nonlocal" "not" "or" "pass" "raise" "return" "try"
    "while" "with" "yield"))
(def-tgt "keyword-prefix" "r_")
(def-tgt "comment-prefix" "#")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "lambda inp: ~A" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "lambda inp: ~A" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "peg_seq(inp, [~{~A~^, ~}])" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "peg_alt(inp, [~{~A~^, ~}])" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "(~{~A == ~S and (lambda: ~A)() or ~}peg_fail(inp, \"no case\"))"
            (loop for (val body) in cases
                  collect param collect val collect body))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "(lambda r: peg_fail(inp, \"let\") if r.failed else (lambda inp: (lambda ~A: ~A)(r.tag_int))(r.rest))(~A)"
            vname rest expr)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "(lambda r: peg_fail(inp, \"let\") if r.failed else (lambda inp: (lambda ~A: ~A)(r.tag))(r.rest))(~A)"
            vname rest expr)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) name))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "in_flow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seq_spaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(inp~{, ~A~})" name
                (mapcar #'peg-ident params))
        (format nil "~A(inp)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "def ~A:~%    return ~A" sig body)))

(def-tgt "fwd-decl"
  (lambda (name params)
    (declare (ignore name params))
    nil))

;;; ── Header ──

(def-tgt "header"
"#!/usr/bin/env python3
# ════════════════════════════════════════════════════════════════
# yaml_reader.py — YAML 1.2 parser, projected from yaml-grammar.scm
# ════════════════════════════════════════════════════════════════
# Generated. DO NOT EDIT — regenerate from the grammar.
# ════════════════════════════════════════════════════════════════

import sys")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"# ── Input ──

class Input:
    __slots__ = ('src', 'pos', 'line', 'col')
    def __init__(self, src, pos=0, line=1, col=0):
        self.src = src; self.pos = pos; self.line = line; self.col = col

def adv(i):
    if i.pos >= len(i.src): return i
    c = i.src[i.pos]
    return Input(i.src, i.pos+1, i.line+1 if c=='\\n' else i.line, 0 if c=='\\n' else i.col+1)"

"# ── AST ──

class YAMLNode:
    __slots__ = ('type', 'text', 'children', 'is_leaf')
    def __init__(self, type, text=None, children=None, is_leaf=False):
        self.type = type; self.text = text
        self.children = children if children is not None else []; self.is_leaf = is_leaf
    @staticmethod
    def branch(t): return YAMLNode(t)
    @staticmethod
    def leaf(t): return YAMLNode('SCALAR', text=t, is_leaf=True)"

"# ── Result ──

class Result:
    __slots__ = ('failed', 'val', 'rest', 'tag', 'tag_int', 'ast', 'ast_list', 'err')
    def __init__(self, failed=False, val='', rest=None, tag='', tag_int=0, ast=None, ast_list=None, err=None):
        self.failed = failed; self.val = val; self.rest = rest; self.tag = tag
        self.tag_int = tag_int; self.ast = ast; self.ast_list = ast_list; self.err = err

def ok(inp): return Result(rest=inp)
def peg_ok(inp): return Result(rest=inp)
def peg_okv(inp, v): return Result(val=v, rest=inp)
def peg_fail(inp, m): return Result(failed=True, rest=inp, err=m)"

"# ── Context ──

def in_flow(c):
    return 'FLOW-IN' if c in ('FLOW-OUT', 'FLOW-IN') else 'FLOW-KEY'
def seq_spaces(n, c):
    return n - 1 if c == 'BLOCK-OUT' else n"

"# ── Combinators ──

def match_cp(inp, cp):
    if inp.pos >= len(inp.src): return peg_fail(inp, 'cp')
    c = ord(inp.src[inp.pos])
    if c == cp: return peg_okv(adv(inp), inp.src[inp.pos])
    return peg_fail(inp, 'cp')

def match_range(inp, lo, hi):
    if inp.pos >= len(inp.src): return peg_fail(inp, 'rng')
    c = ord(inp.src[inp.pos])
    if lo <= c <= hi: return peg_okv(adv(inp), inp.src[inp.pos])
    return peg_fail(inp, 'rng')

def match_str(inp, t):
    n = len(t)
    if inp.pos + n > len(inp.src): return peg_fail(inp, 'str')
    if inp.src[inp.pos:inp.pos+n] != t: return peg_fail(inp, 'str')
    cur = inp
    for _ in range(n): cur = adv(cur)
    return peg_okv(cur, t)

def merge_asts(dst, r):
    if r.ast: dst.append(r.ast)
    if r.ast_list: dst.extend(r.ast_list)

def peg_seq(inp, fns):
    cur = inp; acc = []; asts = []
    for f in fns:
        r = f(cur)
        if r.failed: return r
        acc.append(r.val or ''); merge_asts(asts, r); cur = r.rest
    res = peg_okv(cur, ''.join(acc))
    if len(asts) == 1: res.ast = asts[0]
    elif len(asts) > 1: res.ast_list = asts
    return res

def peg_alt(inp, fns):
    for f in fns:
        r = f(inp)
        if not r.failed: return r
    return peg_fail(inp, 'alt')

def star(inp, f):
    cur = inp; acc = []; asts = []
    while True:
        r = f(cur)
        if r.failed or r.rest.pos <= cur.pos: break
        acc.append(r.val or ''); merge_asts(asts, r); cur = r.rest
    res = peg_okv(cur, ''.join(acc))
    if asts: res.ast_list = asts
    return res

def plus_(inp, f):
    first = f(inp)
    if first.failed: return first
    rest = star(first.rest, f)
    res = peg_okv(rest.rest, (first.val or '') + (rest.val or ''))
    asts = []
    merge_asts(asts, first); merge_asts(asts, rest)
    if asts: res.ast_list = asts
    return res

def opt(inp, f):
    r = f(inp); return peg_ok(inp) if r.failed else r
def neg(inp, f):
    r = f(inp); return peg_ok(inp) if r.failed else peg_fail(inp, 'neg')
def minus(inp, fa, fb):
    ra = fa(inp)
    if ra.failed: return ra
    rb = fb(inp)
    return peg_fail(inp, 'excl') if (not rb.failed and rb.rest.pos == ra.rest.pos) else ra
def rep(inp, n, f):
    cur = inp; acc = []
    for _ in range(n):
        r = f(cur)
        if r.failed: return r
        acc.append(r.val or ''); cur = r.rest
    return peg_okv(cur, ''.join(acc))
def ahead(inp, f):
    r = f(inp); return r if r.failed else peg_ok(inp)
def behind(inp, f):
    if inp.pos == 0: return peg_fail(inp, 'bh')
    t = Input(inp.src, inp.pos-1, inp.line, max(0, inp.col-1))
    r = f(t); return peg_fail(inp, 'bh') if r.failed else peg_ok(inp)
def sol(inp):
    return peg_ok(inp) if inp.col == 0 else peg_fail(inp, 'sol')
def eof_ok(inp):
    return peg_ok(inp) if inp.pos >= len(inp.src) else peg_fail(inp, 'eof')"

"# ── YAML extensions ──

def build(inp, type, f):
    r = f(inp)
    if r.failed: return r
    node = YAMLNode.branch(type)
    if r.ast: node.children.append(r.ast)
    if r.ast_list: node.children.extend(r.ast_list)
    r.ast = node; r.ast_list = None; return r

def scalar(inp, f):
    r = f(inp)
    if r.failed: return r
    r.ast = YAMLNode.leaf(r.val); return r

def collect(inp, f): return f(inp)

def detect_indent(inp, n):
    s = inp.src; ln = len(s); i = inp.pos
    sp = 0
    while i+sp < ln and s[i+sp] == ' ': sp += 1
    if i+sp < ln and s[i+sp] != '\\n':
        r = peg_ok(inp); r.tag_int = max(1, sp - n); return r
    j = i
    while j < ln and s[j] != '\\n': j += 1
    while j < ln:
        if s[j] == '\\n': j += 1
        if j >= ln: break
        sp = 0
        while j+sp < ln and s[j+sp] == ' ': sp += 1
        nx = j + sp
        if nx >= ln or s[nx] == '\\n': j = nx; continue
        r = peg_ok(inp); r.tag_int = max(1, sp - n); return r
    r = peg_ok(inp); r.tag_int = 1; return r

def parse_int(inp, f):
    r = f(inp)
    if r.failed: return r
    r.tag_int = int(r.val) if r.val.isdigit() else 0; return r

def parse_sym(inp, f, sym):
    r = f(inp)
    if r.failed: return r
    r.tag = sym; return r

def val(inp, v):
    r = peg_ok(inp); r.tag = v; return r

sys.setrecursionlimit(10000)"
))

;;; ── API ──

(def-tgt "api"
"# ── API ──

def print_ast(node, depth=0):
    indent = '  ' * depth
    if node.is_leaf:
        print(f'{indent}SCALAR: \"{node.text}\"')
    else:
        print(f'{indent}{node.type}')
        for c in node.children:
            print_ast(c, depth + 1)")

;;; ── Main ──

(def-tgt "main-fn"
"if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        with open(sys.argv[1], encoding='utf-8') as f:
            text = f.read()
    else:
        text = sys.stdin.read()
    inp = Input(text)
    r = l_yaml_stream(inp)
    if not r.failed:
        print(f'OK: {r.rest.pos} chars')
        if r.ast: print_ast(r.ast)
    else:
        print(f'FAIL @{r.rest.pos}: {r.err}', file=sys.stderr)
        sys.exit(1)")
