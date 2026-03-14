#!/usr/bin/env python3
import sys

# ── Input ──

class Input:
    __slots__ = ('src', 'pos', 'line', 'col')
    def __init__(self, src, pos=0, line=1, col=0):
        self.src = src; self.pos = pos; self.line = line; self.col = col

def adv(i):
    if i.pos >= len(i.src): return i
    c = i.src[i.pos]
    return Input(i.src, i.pos+1, i.line+1 if c=='\n' else i.line, 0 if c=='\n' else i.col+1)

# ── AST ──

class JSONNode:
    __slots__ = ('type', 'text', 'children', 'is_leaf')
    def __init__(self, type, text=None, children=None, is_leaf=False):
        self.type = type; self.text = text
        self.children = children if children is not None else []; self.is_leaf = is_leaf
    @staticmethod
    def branch(t): return JSONNode(t)
    @staticmethod
    def leaf(t): return JSONNode('SCALAR', text=t, is_leaf=True)

# ── Result ──

class Result:
    __slots__ = ('failed', 'val', 'rest', 'tag', 'tag_int', 'ast', 'ast_list', 'err')
    def __init__(self, failed=False, val='', rest=None, tag='', tag_int=0, ast=None, ast_list=None, err=None):
        self.failed = failed; self.val = val; self.rest = rest; self.tag = tag
        self.tag_int = tag_int; self.ast = ast; self.ast_list = ast_list; self.err = err

def ok(inp): return Result(rest=inp)
def peg_ok(inp): return Result(rest=inp)
def peg_okv(inp, v): return Result(val=v, rest=inp)
def peg_fail(inp, m): return Result(failed=True, rest=inp, err=m)

# ── Context ──

def in_flow(c):
    return 'FLOW-IN' if c in ('FLOW-OUT', 'FLOW-IN') else 'FLOW-KEY'
def seq_spaces(n, c):
    return n - 1 if c == 'BLOCK-OUT' else n

# ── Combinators ──

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
    return peg_ok(inp) if inp.pos >= len(inp.src) else peg_fail(inp, 'eof')


# ════════════════════════════════════════════════════════════════ 
# YAML 1.2 Grammar — 211 rules 
# ════════════════════════════════════════════════════════════════ 

# [1] JSON-TEXT 
def json_text(inp):
    return peg_seq(inp, [
        lambda inp: ws(inp),
        lambda inp: value(inp),
        lambda inp: ws(inp),
        lambda inp: eof_ok(inp)])

# [2] VALUE 
def value(inp):
    return peg_alt(inp, [
        lambda inp: object(inp),
        lambda inp: array(inp),
        lambda inp: r_string(inp),
        lambda inp: number(inp),
        lambda inp: match_str(inp, "true"),
        lambda inp: match_str(inp, "false"),
        lambda inp: match_str(inp, "null")])

# [3] OBJECT 
def object(inp):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 123),
            lambda inp: ws(inp),
            lambda inp: members(inp),
            lambda inp: ws(inp),
            lambda inp: match_cp(inp, 125)]),
        lambda inp: peg_seq(inp, [lambda inp: match_cp(inp, 123), lambda inp: ws(inp), lambda inp: match_cp(inp, 125)])])

# [4] MEMBERS 
def members(inp):
    return peg_seq(inp, [
        lambda inp: member(inp),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [
            lambda inp: ws(inp),
            lambda inp: match_cp(inp, 44),
            lambda inp: ws(inp),
            lambda inp: member(inp)]))])

# [5] MEMBER 
def member(inp):
    return peg_seq(inp, [
        lambda inp: ws(inp),
        lambda inp: r_string(inp),
        lambda inp: ws(inp),
        lambda inp: match_cp(inp, 58),
        lambda inp: ws(inp),
        lambda inp: value(inp),
        lambda inp: ws(inp)])

# [6] ARRAY 
def array(inp):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 91),
            lambda inp: ws(inp),
            lambda inp: elements(inp),
            lambda inp: ws(inp),
            lambda inp: match_cp(inp, 93)]),
        lambda inp: peg_seq(inp, [lambda inp: match_cp(inp, 91), lambda inp: ws(inp), lambda inp: match_cp(inp, 93)])])

# [7] ELEMENTS 
def elements(inp):
    return peg_seq(inp, [
        lambda inp: value(inp),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [
            lambda inp: ws(inp),
            lambda inp: match_cp(inp, 44),
            lambda inp: ws(inp),
            lambda inp: value(inp)]))])

# [8] STRING 
def r_string(inp):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 34),
        lambda inp: star(inp, lambda inp: r_char(inp)),
        lambda inp: match_cp(inp, 34)])

# [9] CHAR 
def r_char(inp):
    return peg_alt(inp, [
        lambda inp: escaped(inp),
        lambda inp: peg_seq(inp, [
            lambda inp: neg(inp, lambda inp: match_cp(inp, 34)),
            lambda inp: neg(inp, lambda inp: match_cp(inp, 92)),
            lambda inp: neg(inp, lambda inp: match_cp(inp, 0x0)),
            lambda inp: neg(inp, lambda inp: match_range(inp, 0x0, 0x1F)),
            lambda inp: match_range(inp, 0x20, 0x10FFFF)])])

# [10] ESCAPED 
def escaped(inp):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 92),
        lambda inp: peg_alt(inp, [
            lambda inp: match_cp(inp, 34),
            lambda inp: match_cp(inp, 92),
            lambda inp: match_cp(inp, 47),
            lambda inp: match_cp(inp, 98),
            lambda inp: match_cp(inp, 102),
            lambda inp: match_cp(inp, 110),
            lambda inp: match_cp(inp, 114),
            lambda inp: match_cp(inp, 116),
            lambda inp: peg_seq(inp, [lambda inp: match_cp(inp, 117), lambda inp: hex4(inp)])])])

# [11] HEX4 
def hex4(inp):
    return peg_seq(inp, [
        lambda inp: hexdig(inp),
        lambda inp: hexdig(inp),
        lambda inp: hexdig(inp),
        lambda inp: hexdig(inp)])

# [12] HEXDIG 
def hexdig(inp):
    return peg_alt(inp, [
        lambda inp: match_range(inp, 48, 57),
        lambda inp: match_range(inp, 97, 102),
        lambda inp: match_range(inp, 65, 70)])

# [13] NUMBER 
def number(inp):
    return peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: match_cp(inp, 45)),
        lambda inp: integer(inp),
        lambda inp: opt(inp, lambda inp: fraction(inp)),
        lambda inp: opt(inp, lambda inp: exponent(inp))])

# [14] INTEGER 
def integer(inp):
    return peg_alt(inp, [
        lambda inp: match_cp(inp, 48),
        lambda inp: peg_seq(inp, [
            lambda inp: match_range(inp, 49, 57),
            lambda inp: star(inp, lambda inp: match_range(inp, 48, 57))])])

# [15] FRACTION 
def fraction(inp):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 46),
        lambda inp: plus_(inp, lambda inp: match_range(inp, 48, 57))])

# [16] EXPONENT 
def exponent(inp):
    return peg_seq(inp, [
        lambda inp: peg_alt(inp, [lambda inp: match_cp(inp, 101), lambda inp: match_cp(inp, 69)]),
        lambda inp: opt(inp, lambda inp: peg_alt(inp, [lambda inp: match_cp(inp, 43), lambda inp: match_cp(inp, 45)])),
        lambda inp: plus_(inp, lambda inp: match_range(inp, 48, 57))])

# [17] WS 
def ws(inp):
    return star(inp, lambda inp: peg_alt(inp, [
        lambda inp: match_cp(inp, 0x20),
        lambda inp: match_cp(inp, 0x9),
        lambda inp: match_cp(inp, 0x0A),
        lambda inp: match_cp(inp, 0x0D)]))

# ── API ──

def print_ast(node, depth=0):
    indent = '  ' * depth
    if node.is_leaf:
        print(f'{indent}SCALAR: "{node.text}"')
    else:
        print(f'{indent}{node.type}')
        for c in node.children:
            print_ast(c, depth + 1)

if __name__ == '__main__':
    import sys
    if len(sys.argv) > 1:
        with open(sys.argv[1], encoding='utf-8') as f:
            text = f.read()
    else:
        text = sys.stdin.read()
    inp = Input(text)
    r = json_text(inp)
    if not r.failed:
        print(f'OK: {r.rest.pos} chars')
        if r.ast: print_ast(r.ast)
    else:
        print(f'FAIL @{r.rest.pos}: {r.err}', file=sys.stderr)
        sys.exit(1)
