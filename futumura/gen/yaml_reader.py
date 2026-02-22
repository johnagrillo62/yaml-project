#!/usr/bin/env python3
# ════════════════════════════════════════════════════════════════
# yaml_reader.py — YAML 1.2 parser, projected from yaml-grammar.scm
# ════════════════════════════════════════════════════════════════
# Generated. DO NOT EDIT — regenerate from the grammar.
# ════════════════════════════════════════════════════════════════

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

class YAMLNode:
    __slots__ = ('type', 'text', 'children', 'is_leaf')
    def __init__(self, type, text=None, children=None, is_leaf=False):
        self.type = type; self.text = text
        self.children = children if children is not None else []; self.is_leaf = is_leaf
    @staticmethod
    def branch(t): return YAMLNode(t)
    @staticmethod
    def leaf(t): return YAMLNode('SCALAR', text=t, is_leaf=True)

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

# ── YAML extensions ──

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
    if i+sp < ln and s[i+sp] != '\n':
        r = peg_ok(inp); r.tag_int = max(1, sp - n); return r
    j = i
    while j < ln and s[j] != '\n': j += 1
    while j < ln:
        if s[j] == '\n': j += 1
        if j >= ln: break
        sp = 0
        while j+sp < ln and s[j+sp] == ' ': sp += 1
        nx = j + sp
        if nx >= ln or s[nx] == '\n': j = nx; continue
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

sys.setrecursionlimit(10000)


# ════════════════════════════════════════════════════════════════
# YAML 1.2 Grammar — 211 rules
# ════════════════════════════════════════════════════════════════

# [1] C-PRINTABLE
def c_printable(inp):
    return peg_alt(inp, [
        lambda inp: match_cp(inp, 0x9),
        lambda inp: match_cp(inp, 0x0A),
        lambda inp: match_cp(inp, 0x0D),
        lambda inp: match_range(inp, 0x20, 0x7E),
        lambda inp: match_cp(inp, 0x85),
        lambda inp: match_range(inp, 0xA0, 0xD7FF),
        lambda inp: match_range(inp, 0xE000, 0xFFFD),
        lambda inp: match_range(inp, 0x10000, 0x10FFFF)])

# [2] NB-JSON
def nb_json(inp):
    return peg_alt(inp, [lambda inp: match_cp(inp, 0x9), lambda inp: match_range(inp, 0x20, 0x10FFFF)])

# [3] C-BYTE-ORDER-MARK
def c_byte_order_mark(inp):
    return match_cp(inp, 0xFEFF)

# [4] C-SEQUENCE-ENTRY
def c_sequence_entry(inp):
    return match_cp(inp, 45)

# [5] C-MAPPING-KEY
def c_mapping_key(inp):
    return match_cp(inp, 63)

# [6] C-MAPPING-VALUE
def c_mapping_value(inp):
    return match_cp(inp, 58)

# [7] C-COLLECT-ENTRY
def c_collect_entry(inp):
    return match_cp(inp, 44)

# [8] C-SEQUENCE-START
def c_sequence_start(inp):
    return match_cp(inp, 91)

# [9] C-SEQUENCE-END
def c_sequence_end(inp):
    return match_cp(inp, 93)

# [10] C-MAPPING-START
def c_mapping_start(inp):
    return match_cp(inp, 123)

# [11] C-MAPPING-END
def c_mapping_end(inp):
    return match_cp(inp, 125)

# [12] C-COMMENT
def c_comment(inp):
    return match_cp(inp, 35)

# [13] C-ANCHOR
def c_anchor(inp):
    return match_cp(inp, 38)

# [14] C-ALIAS
def c_alias(inp):
    return match_cp(inp, 42)

# [15] C-TAG
def c_tag(inp):
    return match_cp(inp, 33)

# [16] C-LITERAL
def c_literal(inp):
    return match_cp(inp, 124)

# [17] C-FOLDED
def c_folded(inp):
    return match_cp(inp, 62)

# [18] C-SINGLE-QUOTE
def c_single_quote(inp):
    return match_cp(inp, 39)

# [19] C-DOUBLE-QUOTE
def c_double_quote(inp):
    return match_cp(inp, 34)

# [20] C-DIRECTIVE
def c_directive(inp):
    return match_cp(inp, 37)

# [21] C-RESERVED
def c_reserved(inp):
    return peg_alt(inp, [lambda inp: match_cp(inp, 64), lambda inp: match_cp(inp, 96)])

# [22] C-INDICATOR
def c_indicator(inp):
    return peg_alt(inp, [
        lambda inp: c_sequence_entry(inp),
        lambda inp: c_mapping_key(inp),
        lambda inp: c_mapping_value(inp),
        lambda inp: c_collect_entry(inp),
        lambda inp: c_sequence_start(inp),
        lambda inp: c_sequence_end(inp),
        lambda inp: c_mapping_start(inp),
        lambda inp: c_mapping_end(inp),
        lambda inp: c_comment(inp),
        lambda inp: c_anchor(inp),
        lambda inp: c_alias(inp),
        lambda inp: c_tag(inp),
        lambda inp: c_literal(inp),
        lambda inp: c_folded(inp),
        lambda inp: c_single_quote(inp),
        lambda inp: c_double_quote(inp),
        lambda inp: c_directive(inp),
        lambda inp: c_reserved(inp)])

# [23] C-FLOW-INDICATOR
def c_flow_indicator(inp):
    return peg_alt(inp, [
        lambda inp: c_collect_entry(inp),
        lambda inp: c_sequence_start(inp),
        lambda inp: c_sequence_end(inp),
        lambda inp: c_mapping_start(inp),
        lambda inp: c_mapping_end(inp)])

# [24] B-LINE-FEED
def b_line_feed(inp):
    return match_cp(inp, 0x0A)

# [25] B-CARRIAGE-RETURN
def b_carriage_return(inp):
    return match_cp(inp, 0x0D)

# [26] B-CHAR
def b_char(inp):
    return peg_alt(inp, [lambda inp: b_line_feed(inp), lambda inp: b_carriage_return(inp)])

# [27] NB-CHAR
def nb_char(inp):
    return minus(inp, lambda inp: c_printable(inp), lambda inp: peg_alt(inp, [lambda inp: b_char(inp), lambda inp: c_byte_order_mark(inp)]))

# [28] B-BREAK
def b_break(inp):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [lambda inp: b_carriage_return(inp), lambda inp: b_line_feed(inp)]),
        lambda inp: b_carriage_return(inp),
        lambda inp: b_line_feed(inp)])

# [29] B-AS-LINE-FEED
def b_as_line_feed(inp):
    return b_break(inp)

# [30] B-NON-CONTENT
def b_non_content(inp):
    return b_break(inp)

# [31] S-SPACE
def s_space(inp):
    return match_cp(inp, 0x20)

# [32] S-TAB
def s_tab(inp):
    return match_cp(inp, 0x9)

# [33] S-WHITE
def s_white(inp):
    return peg_alt(inp, [lambda inp: s_space(inp), lambda inp: s_tab(inp)])

# [34] NS-CHAR
def ns_char(inp):
    return minus(inp, lambda inp: nb_char(inp), lambda inp: s_white(inp))

# [35] NS-DEC-DIGIT
def ns_dec_digit(inp):
    return match_range(inp, 0x30, 0x39)

# [36] NS-HEX-DIGIT
def ns_hex_digit(inp):
    return peg_alt(inp, [
        lambda inp: ns_dec_digit(inp),
        lambda inp: match_range(inp, 0x41, 0x46),
        lambda inp: match_range(inp, 0x61, 0x66)])

# [37] NS-ASCII-LETTER
def ns_ascii_letter(inp):
    return peg_alt(inp, [lambda inp: match_range(inp, 0x41, 0x5A), lambda inp: match_range(inp, 0x61, 0x7A)])

# [38] NS-WORD-CHAR
def ns_word_char(inp):
    return peg_alt(inp, [
        lambda inp: ns_dec_digit(inp),
        lambda inp: ns_ascii_letter(inp),
        lambda inp: match_cp(inp, 45)])

# [39] NS-URI-CHAR
def ns_uri_char(inp):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 37),
            lambda inp: ns_hex_digit(inp),
            lambda inp: ns_hex_digit(inp)]),
        lambda inp: ns_word_char(inp),
        lambda inp: match_cp(inp, 35),
        lambda inp: match_cp(inp, 59),
        lambda inp: match_cp(inp, 47),
        lambda inp: match_cp(inp, 63),
        lambda inp: match_cp(inp, 58),
        lambda inp: match_cp(inp, 64),
        lambda inp: match_cp(inp, 38),
        lambda inp: match_cp(inp, 61),
        lambda inp: match_cp(inp, 43),
        lambda inp: match_cp(inp, 36),
        lambda inp: match_cp(inp, 44),
        lambda inp: match_cp(inp, 95),
        lambda inp: match_cp(inp, 46),
        lambda inp: match_cp(inp, 33),
        lambda inp: match_cp(inp, 126),
        lambda inp: match_cp(inp, 42),
        lambda inp: match_cp(inp, 39),
        lambda inp: match_cp(inp, 40),
        lambda inp: match_cp(inp, 41),
        lambda inp: match_cp(inp, 91),
        lambda inp: match_cp(inp, 93)])

# [40] NS-TAG-CHAR
def ns_tag_char(inp):
    return minus(inp, lambda inp: ns_uri_char(inp), lambda inp: peg_alt(inp, [lambda inp: c_tag(inp), lambda inp: c_flow_indicator(inp)]))

# [41] C-ESCAPE
def c_escape(inp):
    return match_cp(inp, 92)

# [42] NS-ESC-NULL
def ns_esc_null(inp):
    return match_cp(inp, 48)

# [43] NS-ESC-BELL
def ns_esc_bell(inp):
    return match_cp(inp, 97)

# [44] NS-ESC-BACKSPACE
def ns_esc_backspace(inp):
    return match_cp(inp, 98)

# [45] NS-ESC-HORIZONTAL-TAB
def ns_esc_horizontal_tab(inp):
    return match_cp(inp, 116)

# [46] NS-ESC-LINE-FEED
def ns_esc_line_feed(inp):
    return match_cp(inp, 110)

# [47] NS-ESC-VERTICAL-TAB
def ns_esc_vertical_tab(inp):
    return match_cp(inp, 118)

# [48] NS-ESC-FORM-FEED
def ns_esc_form_feed(inp):
    return match_cp(inp, 102)

# [49] NS-ESC-CARRIAGE-RETURN
def ns_esc_carriage_return(inp):
    return match_cp(inp, 114)

# [50] NS-ESC-ESCAPE
def ns_esc_escape(inp):
    return match_cp(inp, 101)

# [51] NS-ESC-SPACE
def ns_esc_space(inp):
    return match_cp(inp, 0x20)

# [52] NS-ESC-DOUBLE-QUOTE
def ns_esc_double_quote(inp):
    return match_cp(inp, 34)

# [53] NS-ESC-SLASH
def ns_esc_slash(inp):
    return match_cp(inp, 47)

# [54] NS-ESC-BACKSLASH
def ns_esc_backslash(inp):
    return match_cp(inp, 92)

# [55] NS-ESC-NEXT-LINE
def ns_esc_next_line(inp):
    return match_cp(inp, 78)

# [56] NS-ESC-NON-BREAKING-SPACE
def ns_esc_non_breaking_space(inp):
    return match_cp(inp, 95)

# [57] NS-ESC-LINE-SEPARATOR
def ns_esc_line_separator(inp):
    return match_cp(inp, 76)

# [58] NS-ESC-PARAGRAPH-SEPARATOR
def ns_esc_paragraph_separator(inp):
    return match_cp(inp, 80)

# [59] NS-ESC-8-BIT
def ns_esc_8_bit(inp):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 120),
        lambda inp: rep(inp, 2, lambda inp: ns_hex_digit(inp))])

# [60] NS-ESC-16-BIT
def ns_esc_16_bit(inp):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 117),
        lambda inp: rep(inp, 4, lambda inp: ns_hex_digit(inp))])

# [61] NS-ESC-32-BIT
def ns_esc_32_bit(inp):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 85),
        lambda inp: rep(inp, 8, lambda inp: ns_hex_digit(inp))])

# [62] C-NS-ESC-CHAR
def c_ns_esc_char(inp):
    return peg_seq(inp, [
        lambda inp: c_escape(inp),
        lambda inp: peg_alt(inp, [
            lambda inp: ns_esc_null(inp),
            lambda inp: ns_esc_bell(inp),
            lambda inp: ns_esc_backspace(inp),
            lambda inp: ns_esc_horizontal_tab(inp),
            lambda inp: ns_esc_line_feed(inp),
            lambda inp: ns_esc_vertical_tab(inp),
            lambda inp: ns_esc_form_feed(inp),
            lambda inp: ns_esc_carriage_return(inp),
            lambda inp: ns_esc_escape(inp),
            lambda inp: ns_esc_space(inp),
            lambda inp: ns_esc_double_quote(inp),
            lambda inp: ns_esc_slash(inp),
            lambda inp: ns_esc_backslash(inp),
            lambda inp: ns_esc_next_line(inp),
            lambda inp: ns_esc_non_breaking_space(inp),
            lambda inp: ns_esc_line_separator(inp),
            lambda inp: ns_esc_paragraph_separator(inp),
            lambda inp: ns_esc_8_bit(inp),
            lambda inp: ns_esc_16_bit(inp),
            lambda inp: ns_esc_32_bit(inp)])])

# [63] S-INDENT
def s_indent(inp, n):
    return rep(inp, n, lambda inp: s_space(inp))

# [64] S-INDENT-LT
def s_indent_lt(inp, n):
    return star(inp, lambda inp: s_space(inp))

# [65] S-INDENT-LE
def s_indent_le(inp, n):
    return star(inp, lambda inp: s_space(inp))

# [66] S-SEPARATE-IN-LINE
def s_separate_in_line(inp):
    return peg_alt(inp, [lambda inp: plus_(inp, lambda inp: s_white(inp)), lambda inp: ok(inp)])

# [67] S-LINE-PREFIX
def s_line_prefix(inp, n, c):
    return (c == "BLOCK-IN" and (lambda: s_block_line_prefix(inp, n))() or c == "BLOCK-OUT" and (lambda: s_block_line_prefix(inp, n))() or c == "FLOW-IN" and (lambda: s_flow_line_prefix(inp, n))() or c == "FLOW-OUT" and (lambda: s_flow_line_prefix(inp, n))() or peg_fail(inp, "no case"))

# [68] S-BLOCK-LINE-PREFIX
def s_block_line_prefix(inp, n):
    return s_indent(inp, n)

# [69] S-FLOW-LINE-PREFIX
def s_flow_line_prefix(inp, n):
    return peg_seq(inp, [
        lambda inp: s_indent(inp, n),
        lambda inp: opt(inp, lambda inp: s_separate_in_line(inp))])

# [70] L-EMPTY
def l_empty(inp, n, c):
    return peg_seq(inp, [
        lambda inp: peg_alt(inp, [lambda inp: s_line_prefix(inp, n, c), lambda inp: s_indent_lt(inp, n)]),
        lambda inp: b_as_line_feed(inp)])

# [71] B-L-TRIMMED
def b_l_trimmed(inp, n, c):
    return peg_seq(inp, [
        lambda inp: b_non_content(inp),
        lambda inp: plus_(inp, lambda inp: l_empty(inp, n, c))])

# [72] B-AS-SPACE
def b_as_space(inp):
    return b_break(inp)

# [73] B-L-FOLDED
def b_l_folded(inp, n, c):
    return peg_alt(inp, [lambda inp: b_l_trimmed(inp, n, c), lambda inp: b_as_space(inp)])

# [74] S-FLOW-FOLDED
def s_flow_folded(inp, n):
    return peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: s_separate_in_line(inp)),
        lambda inp: b_l_folded(inp, n, "FLOW-IN"),
        lambda inp: s_flow_line_prefix(inp, n)])

# [75] C-NB-COMMENT-TEXT
def c_nb_comment_text(inp):
    return peg_seq(inp, [lambda inp: c_comment(inp), lambda inp: star(inp, lambda inp: nb_char(inp))])

# [76] B-COMMENT
def b_comment(inp):
    return peg_alt(inp, [lambda inp: b_non_content(inp), lambda inp: ok(inp)])

# [77] S-B-COMMENT
def s_b_comment(inp):
    return peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: s_separate_in_line(inp),
            lambda inp: opt(inp, lambda inp: c_nb_comment_text(inp))])),
        lambda inp: b_comment(inp)])

# [78] L-COMMENT
def l_comment(inp):
    return peg_seq(inp, [
        lambda inp: s_separate_in_line(inp),
        lambda inp: opt(inp, lambda inp: c_nb_comment_text(inp)),
        lambda inp: b_non_content(inp)])

# [79] S-L-COMMENTS
def s_l_comments(inp):
    return peg_seq(inp, [
        lambda inp: peg_alt(inp, [lambda inp: s_b_comment(inp), lambda inp: ok(inp)]),
        lambda inp: star(inp, lambda inp: l_comment(inp))])

# [80] S-SEPARATE
def s_separate(inp, n, c):
    return (c == "BLOCK-OUT" and (lambda: s_separate_lines(inp, n))() or c == "BLOCK-IN" and (lambda: s_separate_lines(inp, n))() or c == "FLOW-OUT" and (lambda: s_separate_lines(inp, n))() or c == "FLOW-IN" and (lambda: s_separate_lines(inp, n))() or c == "BLOCK-KEY" and (lambda: s_separate_in_line(inp))() or c == "FLOW-KEY" and (lambda: s_separate_in_line(inp))() or peg_fail(inp, "no case"))

# [81] S-SEPARATE-LINES
def s_separate_lines(inp, n):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [lambda inp: s_l_comments(inp), lambda inp: s_flow_line_prefix(inp, n)]),
        lambda inp: s_separate_in_line(inp)])

# [82] L-DIRECTIVE
def l_directive(inp):
    return peg_seq(inp, [
        lambda inp: c_directive(inp),
        lambda inp: peg_alt(inp, [
            lambda inp: ns_yaml_directive(inp),
            lambda inp: ns_tag_directive(inp),
            lambda inp: ns_reserved_directive(inp)]),
        lambda inp: s_l_comments(inp)])

# [83] NS-RESERVED-DIRECTIVE
def ns_reserved_directive(inp):
    return peg_seq(inp, [
        lambda inp: ns_directive_name(inp),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [lambda inp: s_separate_in_line(inp), lambda inp: ns_directive_parameter(inp)]))])

# [84] NS-DIRECTIVE-NAME
def ns_directive_name(inp):
    return plus_(inp, lambda inp: ns_char(inp))

# [85] NS-DIRECTIVE-PARAMETER
def ns_directive_parameter(inp):
    return plus_(inp, lambda inp: ns_char(inp))

# [86] NS-YAML-DIRECTIVE
def ns_yaml_directive(inp):
    return peg_seq(inp, [
        lambda inp: match_str(inp, "YAML"),
        lambda inp: s_separate_in_line(inp),
        lambda inp: ns_yaml_version(inp)])

# [87] NS-YAML-VERSION
def ns_yaml_version(inp):
    return peg_seq(inp, [
        lambda inp: plus_(inp, lambda inp: ns_dec_digit(inp)),
        lambda inp: match_cp(inp, 46),
        lambda inp: plus_(inp, lambda inp: ns_dec_digit(inp))])

# [88] NS-TAG-DIRECTIVE
def ns_tag_directive(inp):
    return peg_seq(inp, [
        lambda inp: match_str(inp, "TAG"),
        lambda inp: s_separate_in_line(inp),
        lambda inp: c_tag_handle(inp),
        lambda inp: s_separate_in_line(inp),
        lambda inp: ns_tag_prefix(inp)])

# [89] C-TAG-HANDLE
def c_tag_handle(inp):
    return peg_alt(inp, [
        lambda inp: c_named_tag_handle(inp),
        lambda inp: c_secondary_tag_handle(inp),
        lambda inp: c_primary_tag_handle(inp)])

# [90] C-PRIMARY-TAG-HANDLE
def c_primary_tag_handle(inp):
    return match_cp(inp, 33)

# [91] C-SECONDARY-TAG-HANDLE
def c_secondary_tag_handle(inp):
    return match_str(inp, "!!")

# [92] C-NAMED-TAG-HANDLE
def c_named_tag_handle(inp):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 33),
        lambda inp: plus_(inp, lambda inp: ns_word_char(inp)),
        lambda inp: match_cp(inp, 33)])

# [93] NS-TAG-PREFIX
def ns_tag_prefix(inp):
    return peg_alt(inp, [lambda inp: c_ns_local_tag_prefix(inp), lambda inp: ns_global_tag_prefix(inp)])

# [94] C-NS-LOCAL-TAG-PREFIX
def c_ns_local_tag_prefix(inp):
    return peg_seq(inp, [lambda inp: match_cp(inp, 33), lambda inp: star(inp, lambda inp: ns_uri_char(inp))])

# [95] NS-GLOBAL-TAG-PREFIX
def ns_global_tag_prefix(inp):
    return peg_seq(inp, [lambda inp: ns_tag_char(inp), lambda inp: star(inp, lambda inp: ns_uri_char(inp))])

# [96] C-NS-PROPERTIES
def c_ns_properties(inp, n, c):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: c_ns_tag_property(inp),
            lambda inp: opt(inp, lambda inp: peg_seq(inp, [lambda inp: s_separate(inp, n, c), lambda inp: c_ns_anchor_property(inp)]))]),
        lambda inp: peg_seq(inp, [
            lambda inp: c_ns_anchor_property(inp),
            lambda inp: opt(inp, lambda inp: peg_seq(inp, [lambda inp: s_separate(inp, n, c), lambda inp: c_ns_tag_property(inp)]))])])

# [97] C-NS-TAG-PROPERTY
def c_ns_tag_property(inp):
    return peg_alt(inp, [
        lambda inp: c_verbatim_tag(inp),
        lambda inp: c_ns_shorthand_tag(inp),
        lambda inp: c_non_specific_tag(inp)])

# [98] C-VERBATIM-TAG
def c_verbatim_tag(inp):
    return peg_seq(inp, [
        lambda inp: match_str(inp, "!<"),
        lambda inp: plus_(inp, lambda inp: ns_uri_char(inp)),
        lambda inp: match_cp(inp, 62)])

# [99] C-NS-SHORTHAND-TAG
def c_ns_shorthand_tag(inp):
    return peg_seq(inp, [lambda inp: c_tag_handle(inp), lambda inp: plus_(inp, lambda inp: ns_tag_char(inp))])

# [100] C-NON-SPECIFIC-TAG
def c_non_specific_tag(inp):
    return match_cp(inp, 33)

# [101] C-NS-ANCHOR-PROPERTY
def c_ns_anchor_property(inp):
    return build(inp, "ANCHOR", lambda inp: peg_seq(inp, [lambda inp: c_anchor(inp), lambda inp: scalar(inp, lambda inp: ns_anchor_name(inp))]))

# [102] NS-ANCHOR-CHAR
def ns_anchor_char(inp):
    return minus(inp, lambda inp: ns_char(inp), lambda inp: c_flow_indicator(inp))

# [103] NS-ANCHOR-NAME
def ns_anchor_name(inp):
    return plus_(inp, lambda inp: ns_anchor_char(inp))

# [104] C-NS-ALIAS-NODE
def c_ns_alias_node(inp):
    return build(inp, "ALIAS", lambda inp: peg_seq(inp, [lambda inp: c_alias(inp), lambda inp: scalar(inp, lambda inp: ns_anchor_name(inp))]))

# [105] E-SCALAR
def e_scalar(inp):
    return ok(inp)

# [106] E-NODE
def e_node(inp):
    return e_scalar(inp)

# [107] NB-DOUBLE-CHAR
def nb_double_char(inp):
    return peg_alt(inp, [
        lambda inp: c_ns_esc_char(inp),
        lambda inp: minus(inp, lambda inp: nb_json(inp), lambda inp: peg_alt(inp, [lambda inp: match_cp(inp, 92), lambda inp: match_cp(inp, 34)]))])

# [108] NS-DOUBLE-CHAR
def ns_double_char(inp):
    return minus(inp, lambda inp: nb_double_char(inp), lambda inp: s_white(inp))

# [109] C-DOUBLE-QUOTED
def c_double_quoted(inp, n, c):
    return scalar(inp, lambda inp: peg_seq(inp, [
        lambda inp: match_cp(inp, 34),
        lambda inp: nb_double_text(inp, n, c),
        lambda inp: match_cp(inp, 34)]))

# [110] NB-DOUBLE-TEXT
def nb_double_text(inp, n, c):
    return (c == "FLOW-OUT" and (lambda: nb_double_multi_line(inp, n))() or c == "FLOW-IN" and (lambda: nb_double_multi_line(inp, n))() or c == "BLOCK-KEY" and (lambda: nb_double_one_line(inp))() or c == "FLOW-KEY" and (lambda: nb_double_one_line(inp))() or peg_fail(inp, "no case"))

# [111] NB-DOUBLE-ONE-LINE
def nb_double_one_line(inp):
    return star(inp, lambda inp: nb_double_char(inp))

# [112] S-DOUBLE-ESCAPED
def s_double_escaped(inp, n):
    return peg_seq(inp, [
        lambda inp: star(inp, lambda inp: s_white(inp)),
        lambda inp: match_cp(inp, 92),
        lambda inp: b_non_content(inp),
        lambda inp: star(inp, lambda inp: l_empty(inp, n, "FLOW-IN")),
        lambda inp: s_flow_line_prefix(inp, n)])

# [113] S-DOUBLE-BREAK
def s_double_break(inp, n):
    return peg_alt(inp, [lambda inp: s_double_escaped(inp, n), lambda inp: s_flow_folded(inp, n)])

# [114] NB-NS-DOUBLE-IN-LINE
def nb_ns_double_in_line(inp):
    return star(inp, lambda inp: peg_seq(inp, [lambda inp: star(inp, lambda inp: s_white(inp)), lambda inp: ns_double_char(inp)]))

# [115] S-DOUBLE-NEXT-LINE
def s_double_next_line(inp, n):
    return peg_seq(inp, [
        lambda inp: s_double_break(inp, n),
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: ns_double_char(inp),
            lambda inp: nb_ns_double_in_line(inp),
            lambda inp: peg_alt(inp, [
                lambda inp: s_double_next_line(inp, n),
                lambda inp: star(inp, lambda inp: s_white(inp))])]))])

# [116] NB-DOUBLE-MULTI-LINE
def nb_double_multi_line(inp, n):
    return peg_seq(inp, [
        lambda inp: nb_ns_double_in_line(inp),
        lambda inp: peg_alt(inp, [
            lambda inp: s_double_next_line(inp, n),
            lambda inp: star(inp, lambda inp: s_white(inp))])])

# [117] C-QUOTED-QUOTE
def c_quoted_quote(inp):
    return match_str(inp, "''")

# [118] NB-SINGLE-CHAR
def nb_single_char(inp):
    return peg_alt(inp, [
        lambda inp: c_quoted_quote(inp),
        lambda inp: minus(inp, lambda inp: nb_json(inp), lambda inp: match_cp(inp, 39))])

# [119] NS-SINGLE-CHAR
def ns_single_char(inp):
    return minus(inp, lambda inp: nb_single_char(inp), lambda inp: s_white(inp))

# [120] C-SINGLE-QUOTED
def c_single_quoted(inp, n, c):
    return scalar(inp, lambda inp: peg_seq(inp, [
        lambda inp: match_cp(inp, 39),
        lambda inp: nb_single_text(inp, n, c),
        lambda inp: match_cp(inp, 39)]))

# [121] NB-SINGLE-TEXT
def nb_single_text(inp, n, c):
    return (c == "FLOW-OUT" and (lambda: nb_single_multi_line(inp, n))() or c == "FLOW-IN" and (lambda: nb_single_multi_line(inp, n))() or c == "BLOCK-KEY" and (lambda: nb_single_one_line(inp))() or c == "FLOW-KEY" and (lambda: nb_single_one_line(inp))() or peg_fail(inp, "no case"))

# [122] NB-SINGLE-ONE-LINE
def nb_single_one_line(inp):
    return star(inp, lambda inp: nb_single_char(inp))

# [123] NS-SINGLE-IN-LINE
def ns_single_in_line(inp):
    return star(inp, lambda inp: peg_seq(inp, [lambda inp: star(inp, lambda inp: s_white(inp)), lambda inp: ns_single_char(inp)]))

# [124] S-SINGLE-NEXT-LINE
def s_single_next_line(inp, n):
    return peg_seq(inp, [
        lambda inp: s_flow_folded(inp, n),
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: ns_single_char(inp),
            lambda inp: ns_single_in_line(inp),
            lambda inp: peg_alt(inp, [
                lambda inp: s_single_next_line(inp, n),
                lambda inp: star(inp, lambda inp: s_white(inp))])]))])

# [125] NB-SINGLE-MULTI-LINE
def nb_single_multi_line(inp, n):
    return peg_seq(inp, [
        lambda inp: ns_single_in_line(inp),
        lambda inp: peg_alt(inp, [
            lambda inp: s_single_next_line(inp, n),
            lambda inp: star(inp, lambda inp: s_white(inp))])])

# [126] NS-PLAIN-FIRST
def ns_plain_first(inp, c):
    return peg_alt(inp, [
        lambda inp: minus(inp, lambda inp: ns_char(inp), lambda inp: c_indicator(inp)),
        lambda inp: peg_seq(inp, [
            lambda inp: peg_alt(inp, [
                lambda inp: match_cp(inp, 63),
                lambda inp: match_cp(inp, 58),
                lambda inp: match_cp(inp, 45)]),
            lambda inp: ahead(inp, lambda inp: ns_plain_safe(inp, c))])])

# [127] NS-PLAIN-SAFE
def ns_plain_safe(inp, c):
    return (c == "FLOW-OUT" and (lambda: ns_plain_safe_out(inp))() or c == "FLOW-IN" and (lambda: ns_plain_safe_in(inp))() or c == "BLOCK-KEY" and (lambda: ns_plain_safe_out(inp))() or c == "FLOW-KEY" and (lambda: ns_plain_safe_in(inp))() or peg_fail(inp, "no case"))

# [128] NS-PLAIN-SAFE-OUT
def ns_plain_safe_out(inp):
    return ns_char(inp)

# [129] NS-PLAIN-SAFE-IN
def ns_plain_safe_in(inp):
    return minus(inp, lambda inp: ns_char(inp), lambda inp: c_flow_indicator(inp))

# [130] NS-PLAIN-CHAR
def ns_plain_char(inp, c):
    return peg_alt(inp, [
        lambda inp: minus(inp, lambda inp: ns_plain_safe(inp, c), lambda inp: peg_alt(inp, [lambda inp: match_cp(inp, 58), lambda inp: match_cp(inp, 35)])),
        lambda inp: peg_seq(inp, [lambda inp: behind(inp, lambda inp: ns_char(inp)), lambda inp: match_cp(inp, 35)]),
        lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 58),
            lambda inp: ahead(inp, lambda inp: ns_plain_safe(inp, c))])])

# [131] NS-PLAIN
def ns_plain(inp, n, c):
    return scalar(inp, lambda inp: (c == "FLOW-OUT" and (lambda: ns_plain_multi_line(inp, n, c))() or c == "FLOW-IN" and (lambda: ns_plain_multi_line(inp, n, c))() or c == "BLOCK-KEY" and (lambda: ns_plain_one_line(inp, c))() or c == "FLOW-KEY" and (lambda: ns_plain_one_line(inp, c))() or peg_fail(inp, "no case")))

# [132] NB-NS-PLAIN-IN-LINE
def nb_ns_plain_in_line(inp, c):
    return star(inp, lambda inp: peg_seq(inp, [lambda inp: star(inp, lambda inp: s_white(inp)), lambda inp: ns_plain_char(inp, c)]))

# [133] NS-PLAIN-ONE-LINE
def ns_plain_one_line(inp, c):
    return peg_seq(inp, [lambda inp: ns_plain_first(inp, c), lambda inp: nb_ns_plain_in_line(inp, c)])

# [134] S-NS-PLAIN-NEXT-LINE
def s_ns_plain_next_line(inp, n, c):
    return peg_seq(inp, [
        lambda inp: s_flow_folded(inp, n),
        lambda inp: neg(inp, lambda inp: c_forbidden(inp)),
        lambda inp: ns_plain_char(inp, c),
        lambda inp: nb_ns_plain_in_line(inp, c)])

# [135] NS-PLAIN-MULTI-LINE
def ns_plain_multi_line(inp, n, c):
    return peg_seq(inp, [
        lambda inp: ns_plain_one_line(inp, c),
        lambda inp: star(inp, lambda inp: s_ns_plain_next_line(inp, n, c))])

# [137] C-FLOW-SEQUENCE
def c_flow_sequence(inp, n, c):
    return build(inp, "SEQUENCE", lambda inp: peg_seq(inp, [
        lambda inp: match_cp(inp, 91),
        lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
        lambda inp: opt(inp, lambda inp: collect(inp, lambda inp: ns_s_flow_seq_entries(inp, n, in_flow(c)))),
        lambda inp: match_cp(inp, 93)]))

# [138] NS-S-FLOW-SEQ-ENTRIES
def ns_s_flow_seq_entries(inp, n, c):
    return peg_seq(inp, [
        lambda inp: ns_flow_seq_entry(inp, n, c),
        lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 44),
            lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
            lambda inp: opt(inp, lambda inp: ns_s_flow_seq_entries(inp, n, c))]))])

# [139] NS-FLOW-SEQ-ENTRY
def ns_flow_seq_entry(inp, n, c):
    return peg_alt(inp, [lambda inp: ns_flow_pair(inp, n, c), lambda inp: ns_flow_node(inp, n, c)])

# [140] C-FLOW-MAPPING
def c_flow_mapping(inp, n, c):
    return build(inp, "MAPPING", lambda inp: peg_seq(inp, [
        lambda inp: match_cp(inp, 123),
        lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
        lambda inp: opt(inp, lambda inp: collect(inp, lambda inp: ns_s_flow_map_entries(inp, n, in_flow(c)))),
        lambda inp: match_cp(inp, 125)]))

# [141] NS-S-FLOW-MAP-ENTRIES
def ns_s_flow_map_entries(inp, n, c):
    return peg_seq(inp, [
        lambda inp: ns_flow_map_entry(inp, n, c),
        lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 44),
            lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
            lambda inp: opt(inp, lambda inp: ns_s_flow_map_entries(inp, n, c))]))])

# [142] NS-FLOW-MAP-ENTRY
def ns_flow_map_entry(inp, n, c):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 63),
            lambda inp: s_separate(inp, n, c),
            lambda inp: ns_flow_map_explicit_entry(inp, n, c)]),
        lambda inp: ns_flow_map_implicit_entry(inp, n, c)])

# [143] NS-FLOW-MAP-EXPLICIT-ENTRY
def ns_flow_map_explicit_entry(inp, n, c):
    return peg_alt(inp, [
        lambda inp: ns_flow_map_implicit_entry(inp, n, c),
        lambda inp: peg_seq(inp, [lambda inp: e_node(inp), lambda inp: e_node(inp)])])

# [144] NS-FLOW-MAP-IMPLICIT-ENTRY
def ns_flow_map_implicit_entry(inp, n, c):
    return build(inp, "PAIR", lambda inp: peg_alt(inp, [
        lambda inp: ns_flow_map_yaml_key_entry(inp, n, c),
        lambda inp: c_ns_flow_map_empty_key_entry(inp, n, c),
        lambda inp: c_ns_flow_map_json_key_entry(inp, n, c)]))

# [145] NS-FLOW-MAP-YAML-KEY-ENTRY
def ns_flow_map_yaml_key_entry(inp, n, c):
    return peg_seq(inp, [
        lambda inp: ns_flow_yaml_node(inp, n, c),
        lambda inp: peg_alt(inp, [
            lambda inp: peg_seq(inp, [
                lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
                lambda inp: c_ns_flow_map_separate_value(inp, n, c)]),
            lambda inp: e_node(inp)])])

# [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY
def c_ns_flow_map_empty_key_entry(inp, n, c):
    return peg_seq(inp, [lambda inp: e_node(inp), lambda inp: c_ns_flow_map_separate_value(inp, n, c)])

# [147] C-NS-FLOW-MAP-SEPARATE-VALUE
def c_ns_flow_map_separate_value(inp, n, c):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 58),
        lambda inp: neg(inp, lambda inp: ns_plain_safe(inp, c)),
        lambda inp: peg_alt(inp, [
            lambda inp: peg_seq(inp, [lambda inp: s_separate(inp, n, c), lambda inp: ns_flow_node(inp, n, c)]),
            lambda inp: e_node(inp)])])

# [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY
def c_ns_flow_map_json_key_entry(inp, n, c):
    return peg_seq(inp, [
        lambda inp: c_flow_json_node(inp, n, c),
        lambda inp: peg_alt(inp, [
            lambda inp: peg_seq(inp, [
                lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
                lambda inp: c_ns_flow_map_adjacent_value(inp, n, c)]),
            lambda inp: e_node(inp)])])

# [149] C-NS-FLOW-MAP-ADJACENT-VALUE
def c_ns_flow_map_adjacent_value(inp, n, c):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 58),
        lambda inp: peg_alt(inp, [
            lambda inp: peg_seq(inp, [
                lambda inp: opt(inp, lambda inp: s_separate(inp, n, c)),
                lambda inp: ns_flow_node(inp, n, c)]),
            lambda inp: e_node(inp)])])

# [150] NS-FLOW-PAIR
def ns_flow_pair(inp, n, c):
    return peg_alt(inp, [
        lambda inp: peg_seq(inp, [
            lambda inp: match_cp(inp, 63),
            lambda inp: s_separate(inp, n, c),
            lambda inp: ns_flow_map_explicit_entry(inp, n, c)]),
        lambda inp: ns_flow_pair_entry(inp, n, c)])

# [151] NS-FLOW-PAIR-ENTRY
def ns_flow_pair_entry(inp, n, c):
    return peg_alt(inp, [
        lambda inp: ns_flow_pair_yaml_key_entry(inp, n, c),
        lambda inp: c_ns_flow_map_empty_key_entry(inp, n, c),
        lambda inp: c_ns_flow_pair_json_key_entry(inp, n, c)])

# [152] NS-FLOW-PAIR-YAML-KEY-ENTRY
def ns_flow_pair_yaml_key_entry(inp, n, c):
    return peg_seq(inp, [
        lambda inp: ns_s_implicit_yaml_key(inp, "FLOW-KEY"),
        lambda inp: c_ns_flow_map_separate_value(inp, n, c)])

# [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY
def c_ns_flow_pair_json_key_entry(inp, n, c):
    return peg_seq(inp, [
        lambda inp: c_s_implicit_json_key(inp, "FLOW-KEY"),
        lambda inp: c_ns_flow_map_adjacent_value(inp, n, c)])

# [154] NS-S-IMPLICIT-YAML-KEY
def ns_s_implicit_yaml_key(inp, c):
    return peg_seq(inp, [
        lambda inp: ns_flow_yaml_node(inp, 0, c),
        lambda inp: opt(inp, lambda inp: s_separate_in_line(inp))])

# [155] C-S-IMPLICIT-JSON-KEY
def c_s_implicit_json_key(inp, c):
    return peg_seq(inp, [
        lambda inp: c_flow_json_node(inp, 0, c),
        lambda inp: opt(inp, lambda inp: s_separate_in_line(inp))])

# [156] NS-FLOW-YAML-CONTENT
def ns_flow_yaml_content(inp, n, c):
    return ns_plain(inp, n, c)

# [157] C-FLOW-JSON-CONTENT
def c_flow_json_content(inp, n, c):
    return peg_alt(inp, [
        lambda inp: c_flow_sequence(inp, n, c),
        lambda inp: c_flow_mapping(inp, n, c),
        lambda inp: c_single_quoted(inp, n, c),
        lambda inp: c_double_quoted(inp, n, c)])

# [158] NS-FLOW-CONTENT
def ns_flow_content(inp, n, c):
    return peg_alt(inp, [
        lambda inp: ns_flow_yaml_content(inp, n, c),
        lambda inp: c_flow_json_content(inp, n, c)])

# [159] NS-FLOW-YAML-NODE
def ns_flow_yaml_node(inp, n, c):
    return peg_alt(inp, [
        lambda inp: c_ns_alias_node(inp),
        lambda inp: ns_flow_yaml_content(inp, n, c),
        lambda inp: peg_seq(inp, [
            lambda inp: c_ns_properties(inp, n, c),
            lambda inp: peg_alt(inp, [
                lambda inp: peg_seq(inp, [lambda inp: s_separate(inp, n, c), lambda inp: ns_flow_yaml_content(inp, n, c)]),
                lambda inp: e_scalar(inp)])])])

# [160] C-FLOW-JSON-NODE
def c_flow_json_node(inp, n, c):
    return peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [lambda inp: c_ns_properties(inp, n, c), lambda inp: s_separate(inp, n, c)])),
        lambda inp: c_flow_json_content(inp, n, c)])

# [161] NS-FLOW-NODE
def ns_flow_node(inp, n, c):
    return peg_alt(inp, [
        lambda inp: c_ns_alias_node(inp),
        lambda inp: ns_flow_content(inp, n, c),
        lambda inp: peg_seq(inp, [
            lambda inp: c_ns_properties(inp, n, c),
            lambda inp: peg_alt(inp, [
                lambda inp: peg_seq(inp, [lambda inp: s_separate(inp, n, c), lambda inp: ns_flow_content(inp, n, c)]),
                lambda inp: e_scalar(inp)])])])

# [162] C-B-BLOCK-HEADER
def c_b_block_header(inp, n):
    return peg_alt(inp, [
        lambda inp: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda m: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda t: s_b_comment(inp))(r.tag))(r.rest))(peg_alt(inp, [
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 45), "STRIP"),
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 43), "KEEP"),
            lambda inp: val(inp, "CLIP")])))(r.tag_int))(r.rest))(peg_alt(inp, [
            lambda inp: parse_int(inp, lambda inp: ns_dec_digit(inp)),
            lambda inp: detect_indent(inp, n)])),
        lambda inp: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda t: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda m: s_b_comment(inp))(r.tag_int))(r.rest))(peg_alt(inp, [
            lambda inp: parse_int(inp, lambda inp: ns_dec_digit(inp)),
            lambda inp: detect_indent(inp, n)])))(r.tag))(r.rest))(peg_alt(inp, [
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 45), "STRIP"),
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 43), "KEEP"),
            lambda inp: val(inp, "CLIP")]))])

# [163] C-INDENTATION-INDICATOR
def c_indentation_indicator(inp, n):
    return peg_alt(inp, [lambda inp: ns_dec_digit(inp), lambda inp: ok(inp)])

# [164] C-CHOMPING-INDICATOR
def c_chomping_indicator(inp):
    return peg_alt(inp, [lambda inp: match_cp(inp, 45), lambda inp: match_cp(inp, 43), lambda inp: ok(inp)])

# [165] B-CHOMPED-LAST
def b_chomped_last(inp, t):
    return (t == "STRIP" and (lambda: b_non_content(inp))() or t == "CLIP" and (lambda: b_as_line_feed(inp))() or t == "KEEP" and (lambda: b_as_line_feed(inp))() or peg_fail(inp, "no case"))

# [166] L-CHOMPED-EMPTY
def l_chomped_empty(inp, n, t):
    return (t == "STRIP" and (lambda: l_strip_empty(inp, n))() or t == "CLIP" and (lambda: l_strip_empty(inp, n))() or t == "KEEP" and (lambda: l_keep_empty(inp, n))() or peg_fail(inp, "no case"))

# [167] L-STRIP-EMPTY
def l_strip_empty(inp, n):
    return peg_seq(inp, [
        lambda inp: star(inp, lambda inp: peg_seq(inp, [lambda inp: s_indent_le(inp, n), lambda inp: b_non_content(inp)])),
        lambda inp: opt(inp, lambda inp: l_trail_comments(inp, n))])

# [168] L-KEEP-EMPTY
def l_keep_empty(inp, n):
    return peg_seq(inp, [
        lambda inp: star(inp, lambda inp: l_empty(inp, n, "BLOCK-IN")),
        lambda inp: opt(inp, lambda inp: l_trail_comments(inp, n))])

# [169] L-TRAIL-COMMENTS
def l_trail_comments(inp, n):
    return peg_seq(inp, [
        lambda inp: s_indent_lt(inp, n),
        lambda inp: c_nb_comment_text(inp),
        lambda inp: b_comment(inp),
        lambda inp: star(inp, lambda inp: l_comment(inp))])

# [170] C-L+LITERAL
def c_lliteral(inp, n):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 124),
        lambda inp: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda m: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda t: peg_seq(inp, [lambda inp: s_b_comment(inp), lambda inp: l_literal_content(inp, (n + m), t)]))(r.tag))(r.rest))(peg_alt(inp, [
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 45), "STRIP"),
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 43), "KEEP"),
            lambda inp: val(inp, "CLIP")])))(r.tag_int))(r.rest))(peg_alt(inp, [
            lambda inp: parse_int(inp, lambda inp: ns_dec_digit(inp)),
            lambda inp: detect_indent(inp, n)]))])

# [171] L-NB-LITERAL-TEXT
def l_nb_literal_text(inp, n):
    return peg_seq(inp, [
        lambda inp: star(inp, lambda inp: l_empty(inp, n, "BLOCK-IN")),
        lambda inp: s_indent(inp, n),
        lambda inp: plus_(inp, lambda inp: nb_char(inp))])

# [172] B-NB-LITERAL-NEXT
def b_nb_literal_next(inp, n):
    return peg_seq(inp, [lambda inp: b_as_line_feed(inp), lambda inp: l_nb_literal_text(inp, n)])

# [173] L-LITERAL-CONTENT
def l_literal_content(inp, n, t):
    return scalar(inp, lambda inp: peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: l_nb_literal_text(inp, n),
            lambda inp: star(inp, lambda inp: b_nb_literal_next(inp, n)),
            lambda inp: b_chomped_last(inp, t)])),
        lambda inp: l_chomped_empty(inp, n, t)]))

# [174] C-L+FOLDED
def c_lfolded(inp, n):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 62),
        lambda inp: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda m: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda t: peg_seq(inp, [lambda inp: s_b_comment(inp), lambda inp: l_folded_content(inp, (n + m), t)]))(r.tag))(r.rest))(peg_alt(inp, [
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 45), "STRIP"),
            lambda inp: parse_sym(inp, lambda inp: match_cp(inp, 43), "KEEP"),
            lambda inp: val(inp, "CLIP")])))(r.tag_int))(r.rest))(peg_alt(inp, [
            lambda inp: parse_int(inp, lambda inp: ns_dec_digit(inp)),
            lambda inp: detect_indent(inp, n)]))])

# [175] S-NB-FOLDED-TEXT
def s_nb_folded_text(inp, n):
    return peg_seq(inp, [
        lambda inp: s_indent(inp, n),
        lambda inp: ns_char(inp),
        lambda inp: star(inp, lambda inp: nb_char(inp))])

# [176] L-NB-FOLDED-LINES
def l_nb_folded_lines(inp, n):
    return peg_seq(inp, [
        lambda inp: s_nb_folded_text(inp, n),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [lambda inp: b_l_folded(inp, n, "BLOCK-IN"), lambda inp: s_nb_folded_text(inp, n)]))])

# [177] S-NB-SPACED-TEXT
def s_nb_spaced_text(inp, n):
    return peg_seq(inp, [
        lambda inp: s_indent(inp, n),
        lambda inp: s_white(inp),
        lambda inp: star(inp, lambda inp: nb_char(inp))])

# [178] B-L-SPACED
def b_l_spaced(inp, n):
    return peg_seq(inp, [
        lambda inp: b_as_line_feed(inp),
        lambda inp: star(inp, lambda inp: l_empty(inp, n, "BLOCK-IN"))])

# [179] L-NB-SPACED-LINES
def l_nb_spaced_lines(inp, n):
    return peg_seq(inp, [
        lambda inp: s_nb_spaced_text(inp, n),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [lambda inp: b_l_spaced(inp, n), lambda inp: s_nb_spaced_text(inp, n)]))])

# [180] L-NB-SAME-LINES
def l_nb_same_lines(inp, n):
    return peg_seq(inp, [
        lambda inp: star(inp, lambda inp: l_empty(inp, n, "BLOCK-IN")),
        lambda inp: peg_alt(inp, [lambda inp: l_nb_folded_lines(inp, n), lambda inp: l_nb_spaced_lines(inp, n)])])

# [181] L-NB-DIFF-LINES
def l_nb_diff_lines(inp, n):
    return peg_seq(inp, [
        lambda inp: l_nb_same_lines(inp, n),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [lambda inp: b_as_line_feed(inp), lambda inp: l_nb_same_lines(inp, n)]))])

# [182] L-FOLDED-CONTENT
def l_folded_content(inp, n, t):
    return scalar(inp, lambda inp: peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [lambda inp: l_nb_diff_lines(inp, n), lambda inp: b_chomped_last(inp, t)])),
        lambda inp: l_chomped_empty(inp, n, t)]))

# [183] L+BLOCK-SEQUENCE
def lblock_sequence(inp, n):
    return build(inp, "SEQUENCE", lambda inp: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda m: collect(inp, lambda inp: plus_(inp, lambda inp: peg_seq(inp, [lambda inp: s_indent(inp, (n + m)), lambda inp: c_l_block_seq_entry(inp, (n + m))]))))(r.tag_int))(r.rest))(detect_indent(inp, n)))

# [184] C-L-BLOCK-SEQ-ENTRY
def c_l_block_seq_entry(inp, n):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 45),
        lambda inp: neg(inp, lambda inp: ns_char(inp)),
        lambda inp: s_lblock_indented(inp, n, "BLOCK-IN")])

# [185] S-L+BLOCK-INDENTED
def s_lblock_indented(inp, n, c):
    return peg_alt(inp, [
        lambda inp: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda m: peg_seq(inp, [
            lambda inp: s_indent(inp, m),
            lambda inp: peg_alt(inp, [
                lambda inp: ns_l_compact_sequence(inp, (n + 1 + m)),
                lambda inp: ns_l_compact_mapping(inp, (n + 1 + m))])]))(r.tag_int))(r.rest))(detect_indent(inp, 0)),
        lambda inp: s_lblock_node(inp, n, c),
        lambda inp: peg_seq(inp, [lambda inp: e_node(inp), lambda inp: s_l_comments(inp)])])

# [186] NS-L-COMPACT-SEQUENCE
def ns_l_compact_sequence(inp, n):
    return peg_seq(inp, [
        lambda inp: c_l_block_seq_entry(inp, n),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [lambda inp: s_indent(inp, n), lambda inp: c_l_block_seq_entry(inp, n)]))])

# [187] L+BLOCK-MAPPING
def lblock_mapping(inp, n):
    return build(inp, "MAPPING", lambda inp: (lambda r: peg_fail(inp, "let") if r.failed else (lambda inp: (lambda m: collect(inp, lambda inp: plus_(inp, lambda inp: peg_seq(inp, [lambda inp: s_indent(inp, (n + m)), lambda inp: ns_l_block_map_entry(inp, (n + m))]))))(r.tag_int))(r.rest))(detect_indent(inp, n)))

# [188] NS-L-BLOCK-MAP-ENTRY
def ns_l_block_map_entry(inp, n):
    return peg_alt(inp, [
        lambda inp: c_l_block_map_explicit_entry(inp, n),
        lambda inp: ns_l_block_map_implicit_entry(inp, n)])

# [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY
def c_l_block_map_explicit_entry(inp, n):
    return peg_seq(inp, [
        lambda inp: c_l_block_map_explicit_key(inp, n),
        lambda inp: peg_alt(inp, [lambda inp: l_block_map_explicit_value(inp, n), lambda inp: e_node(inp)])])

# [190] C-L-BLOCK-MAP-EXPLICIT-KEY
def c_l_block_map_explicit_key(inp, n):
    return peg_seq(inp, [lambda inp: match_cp(inp, 63), lambda inp: s_lblock_indented(inp, n, "BLOCK-OUT")])

# [191] L-BLOCK-MAP-EXPLICIT-VALUE
def l_block_map_explicit_value(inp, n):
    return peg_seq(inp, [
        lambda inp: s_indent(inp, n),
        lambda inp: match_cp(inp, 58),
        lambda inp: s_lblock_indented(inp, n, "BLOCK-OUT")])

# [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY
def ns_l_block_map_implicit_entry(inp, n):
    return build(inp, "PAIR", lambda inp: peg_seq(inp, [
        lambda inp: scalar(inp, lambda inp: peg_alt(inp, [lambda inp: ns_s_block_map_implicit_key(inp), lambda inp: e_node(inp)])),
        lambda inp: c_l_block_map_implicit_value(inp, n)]))

# [193] NS-S-BLOCK-MAP-IMPLICIT-KEY
def ns_s_block_map_implicit_key(inp):
    return peg_alt(inp, [
        lambda inp: c_s_implicit_json_key(inp, "BLOCK-KEY"),
        lambda inp: ns_s_implicit_yaml_key(inp, "BLOCK-KEY")])

# [194] C-L-BLOCK-MAP-IMPLICIT-VALUE
def c_l_block_map_implicit_value(inp, n):
    return peg_seq(inp, [
        lambda inp: match_cp(inp, 58),
        lambda inp: peg_alt(inp, [
            lambda inp: s_lblock_node(inp, n, "BLOCK-OUT"),
            lambda inp: scalar(inp, lambda inp: peg_seq(inp, [lambda inp: e_node(inp), lambda inp: s_l_comments(inp)]))])])

# [195] NS-L-COMPACT-MAPPING
def ns_l_compact_mapping(inp, n):
    return peg_seq(inp, [
        lambda inp: ns_l_block_map_entry(inp, n),
        lambda inp: star(inp, lambda inp: peg_seq(inp, [lambda inp: s_indent(inp, n), lambda inp: ns_l_block_map_entry(inp, n)]))])

# [196] S-L+BLOCK-NODE
def s_lblock_node(inp, n, c):
    return peg_alt(inp, [lambda inp: s_lblock_in_block(inp, n, c), lambda inp: s_lflow_in_block(inp, n)])

# [197] S-L+FLOW-IN-BLOCK
def s_lflow_in_block(inp, n):
    return peg_seq(inp, [
        lambda inp: s_separate(inp, (n + 1), "FLOW-OUT"),
        lambda inp: ns_flow_node(inp, (n + 1), "FLOW-OUT"),
        lambda inp: s_l_comments(inp)])

# [198] S-L+BLOCK-IN-BLOCK
def s_lblock_in_block(inp, n, c):
    return peg_alt(inp, [lambda inp: s_lblock_scalar(inp, n, c), lambda inp: s_lblock_collection(inp, n, c)])

# [199] S-L+BLOCK-SCALAR
def s_lblock_scalar(inp, n, c):
    return peg_seq(inp, [
        lambda inp: s_separate(inp, (n + 1), c),
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: c_ns_properties(inp, (n + 1), c),
            lambda inp: s_separate(inp, (n + 1), c)])),
        lambda inp: peg_alt(inp, [lambda inp: c_lliteral(inp, n), lambda inp: c_lfolded(inp, n)])])

# [200] S-L+BLOCK-COLLECTION
def s_lblock_collection(inp, n, c):
    return peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: peg_seq(inp, [
            lambda inp: s_separate(inp, (n + 1), c),
            lambda inp: c_ns_properties(inp, (n + 1), c)])),
        lambda inp: s_l_comments(inp),
        lambda inp: peg_alt(inp, [
            lambda inp: lblock_sequence(inp, seq_spaces(n, c)),
            lambda inp: lblock_mapping(inp, n)])])

# [202] L-DOCUMENT-PREFIX
def l_document_prefix(inp):
    return peg_seq(inp, [
        lambda inp: opt(inp, lambda inp: c_byte_order_mark(inp)),
        lambda inp: star(inp, lambda inp: l_comment(inp))])

# [203] C-DIRECTIVES-END
def c_directives_end(inp):
    return match_str(inp, "---")

# [204] C-DOCUMENT-END
def c_document_end(inp):
    return match_str(inp, "...")

# [205] L-DOCUMENT-SUFFIX
def l_document_suffix(inp):
    return peg_seq(inp, [lambda inp: c_document_end(inp), lambda inp: s_l_comments(inp)])

# [206] C-FORBIDDEN
def c_forbidden(inp):
    return peg_seq(inp, [
        lambda inp: sol(inp),
        lambda inp: peg_alt(inp, [lambda inp: c_directives_end(inp), lambda inp: c_document_end(inp)]),
        lambda inp: peg_alt(inp, [lambda inp: b_char(inp), lambda inp: s_white(inp), lambda inp: eof_ok(inp)])])

# [207] L-BARE-DOCUMENT
def l_bare_document(inp):
    return build(inp, "DOC", lambda inp: s_lblock_node(inp, -1, "BLOCK-IN"))

# [208] L-EXPLICIT-DOCUMENT
def l_explicit_document(inp):
    return build(inp, "DOC", lambda inp: peg_seq(inp, [
        lambda inp: c_directives_end(inp),
        lambda inp: peg_alt(inp, [
            lambda inp: l_bare_document(inp),
            lambda inp: peg_seq(inp, [lambda inp: e_node(inp), lambda inp: s_l_comments(inp)])])]))

# [209] L-DIRECTIVE-DOCUMENT
def l_directive_document(inp):
    return peg_seq(inp, [
        lambda inp: plus_(inp, lambda inp: l_directive(inp)),
        lambda inp: l_explicit_document(inp)])

# [210] L-ANY-DOCUMENT
def l_any_document(inp):
    return peg_alt(inp, [
        lambda inp: l_directive_document(inp),
        lambda inp: l_explicit_document(inp),
        lambda inp: l_bare_document(inp)])

# [211] L-YAML-STREAM
def l_yaml_stream(inp):
    return build(inp, "STREAM", lambda inp: peg_seq(inp, [
        lambda inp: star(inp, lambda inp: l_document_prefix(inp)),
        lambda inp: opt(inp, lambda inp: l_any_document(inp)),
        lambda inp: star(inp, lambda inp: peg_alt(inp, [
            lambda inp: peg_seq(inp, [
                lambda inp: plus_(inp, lambda inp: l_document_suffix(inp)),
                lambda inp: star(inp, lambda inp: l_document_prefix(inp)),
                lambda inp: opt(inp, lambda inp: l_any_document(inp))]),
            lambda inp: peg_seq(inp, [
                lambda inp: star(inp, lambda inp: l_document_prefix(inp)),
                lambda inp: opt(inp, lambda inp: l_explicit_document(inp))])]))]))

# ── API ──

def print_ast(node, depth=0):
    indent = '  ' * depth
    if node.is_leaf:
        print(f'{indent}SCALAR: "{node.text}"')
    else:
        print(f'{indent}{node.type}')
        for c in node.children:
            print_ast(c, depth + 1)

# ── Native Value Type ──



# ── Schema Coercion ──

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
        if node.is_leaf:
            return coerce_scalar(node.text)
        t = node.type
        if t == 'ANCHOR':
            name = None
            val = None
            for c in node.children:
                if c.is_leaf and name is None:
                    name = c.text
                else:
                    val = self.convert(c)
            if name:
                self.anchors[name] = val
            return val
        if t == 'ALIAS':
            for c in node.children:
                if c.is_leaf and c.text in self.anchors:
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
    inp = Input(text, 0, 1, 0)
    r = l_yaml_stream(inp)
    if r.failed:
        return None
    return Converter().convert(r.ast)

def load_file(path):
    with open(path) as f:
        return load(f.read())

if __name__ == '__main__':
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
        sys.exit(1)
