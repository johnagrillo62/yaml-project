// ════════════════════════════════════════════════════════════════
package main

import (
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"strings"
)

// ── Input ──

type Input struct {
	src *string
	pos int
	line int
	col int
}

func newInput(s *string) Input { return Input{src: s, pos: 0, line: 1, col: 0} }
func atEof(i Input) bool { return i.pos >= len(*i.src) }
func peekCp(i Input) int {
	if atEof(i) { return -1 }
	b := (*i.src)[i.pos]
	if b < 0x80 { return int(b) }
	// UTF-8 decode
	s := *i.src
	if i.pos >= len(s) { return -1 }
	r := []rune(s[i.pos:])[0]
	return int(r)
}
func adv(i Input) Input {
	if atEof(i) { return i }
	c := (*i.src)[i.pos]
	nl := c == '\n'
	line := i.line; col := i.col + 1
	if nl { line++; col = 0 }
	return Input{i.src, i.pos + 1, line, col}
}

// ── AST ──

type Ast struct {
	tag      string
	text     string
	children []*Ast
	isLeaf   bool
}

func makeAst(tag string) *Ast { return &Ast{tag: tag} }
func leafAst(text string) *Ast { return &Ast{text: text, isLeaf: true} }

// ── Result ──

type Result struct {
	fail    bool
	val     string
	rest    Input
	tag     string
	tagInt  int
	ast     *Ast
	astList []*Ast
	err     string
}

func ok(inp Input) Result { return Result{rest: inp} }
func okV(inp Input, v string) Result { return Result{val: v, rest: inp} }
func fail(inp Input, msg string) Result { return Result{fail: true, rest: inp, err: msg} }

// ── Context ──

func inFlow(c string) string {
	if c == "FLOW-OUT" || c == "FLOW-IN" { return "FLOW-IN" }
	return "FLOW-KEY"
}
func seqSpaces(n int, c string) int {
	if c == "BLOCK-OUT" { return n - 1 }
	return n
}

// ── Combinators ──

type PFn = func(Input) Result

func matchCp(inp Input, cp int) Result {
	c := peekCp(inp)
	if c == cp {
		r := []rune{rune(c)}
		s := string(r)
		cur := inp
		for i := 0; i < len(s); i++ { cur = adv(cur) }
		return okV(cur, s)
	}
	return fail(inp, "cp")
}

func match_cp(inp Input, cp int) Result { return matchCp(inp, cp) }

func matchRange(inp Input, lo, hi int) Result {
	c := peekCp(inp)
	if c >= lo && c <= hi {
		r := []rune{rune(c)}
		s := string(r)
		cur := inp
		for i := 0; i < len(s); i++ { cur = adv(cur) }
		return okV(cur, s)
	}
	return fail(inp, "rng")
}

func match_range(inp Input, lo, hi int) Result { return matchRange(inp, lo, hi) }

func match_str(inp Input, t string) Result {
	n := len(t)
	if inp.pos+n > len(*inp.src) { return fail(inp, "str") }
	if (*inp.src)[inp.pos:inp.pos+n] != t { return fail(inp, "str") }
	cur := inp
	for i := 0; i < n; i++ { cur = adv(cur) }
	return okV(cur, t)
}

func mergeAsts(dst *[]*Ast, r Result) {
	if r.ast != nil { *dst = append(*dst, r.ast) }
	if len(r.astList) > 0 { *dst = append(*dst, r.astList...) }
}

func seq(inp Input, fns []PFn) Result {
	cur := inp; acc := ""; var asts []*Ast
	for _, f := range fns {
		r := f(cur); if r.fail { return r }
		acc += r.val; mergeAsts(&asts, r); cur = r.rest
	}
	res := okV(cur, acc)
	if len(asts) == 1 { res.ast = asts[0] } else if len(asts) > 1 { res.astList = asts }
	return res
}

func alt(inp Input, fns []PFn) Result {
	for _, f := range fns { r := f(inp); if !r.fail { return r } }
	return fail(inp, "alt")
}

func star(inp Input, f PFn) Result {
	cur := inp; acc := ""; var asts []*Ast
	for {
		r := f(cur); if r.fail || r.rest.pos <= cur.pos { break }
		acc += r.val; mergeAsts(&asts, r); cur = r.rest
	}
	res := okV(cur, acc)
	if len(asts) > 0 { res.astList = asts }
	return res
}

func plus_(inp Input, f PFn) Result {
	first := f(inp); if first.fail { return first }
	rest := star(first.rest, f)
	res := okV(rest.rest, first.val+rest.val)
	var asts []*Ast; mergeAsts(&asts, first); mergeAsts(&asts, rest)
	if len(asts) > 0 { res.astList = asts }
	return res
}

func opt(inp Input, f PFn) Result { r := f(inp); if r.fail { return ok(inp) }; return r }
func neg(inp Input, f PFn) Result { r := f(inp); if r.fail { return ok(inp) }; return fail(inp, "neg") }
func minus(inp Input, fa, fb PFn) Result {
	ra := fa(inp); if ra.fail { return ra }
	rb := fb(inp); if !rb.fail && rb.rest.pos == ra.rest.pos { return fail(inp, "excl") }; return ra
}
func rep(inp Input, n int, f PFn) Result {
	cur := inp; acc := ""
	for i := 0; i < n; i++ { r := f(cur); if r.fail { return r }; acc += r.val; cur = r.rest }
	return okV(cur, acc)
}
func ahead(inp Input, f PFn) Result { r := f(inp); if r.fail { return r }; return ok(inp) }
func behind(inp Input, f PFn) Result {
	if inp.pos == 0 { return fail(inp, "bh") }
	t := Input{inp.src, inp.pos - 1, inp.line, max(0, inp.col - 1)}
	r := f(t); if r.fail { return fail(inp, "bh") }; return ok(inp)
}
func sol(inp Input) Result { if inp.col == 0 { return ok(inp) }; return fail(inp, "sol") }
func eof_ok(inp Input) Result { if atEof(inp) { return ok(inp) }; return fail(inp, "eof") }

func max(a, b int) int { if a > b { return a }; return b }

// ── YAML extensions ──

func build(inp Input, typ string, f PFn) Result {
	r := f(inp); if r.fail { return r }
	node := makeAst(typ)
	if r.ast != nil { node.children = append(node.children, r.ast) }
	if len(r.astList) > 0 { node.children = append(node.children, r.astList...) }
	r.ast = node; r.astList = nil; return r
}

func scalar(inp Input, f PFn) Result {
	r := f(inp); if r.fail { return r }
	r.ast = leafAst(r.val); return r
}

func collect(inp Input, f PFn) Result { return f(inp) }

func detect_indent(inp Input, n int) Result {
	s := *inp.src; l := len(s); i := inp.pos
	sp := 0; for i+sp < l && s[i+sp] == ' ' { sp++ }
	if i+sp < l && s[i+sp] != '\n' { r := ok(inp); r.tagInt = max(1, sp-n); return r }
	j := i; for j < l && s[j] != '\n' { j++ }
	for j < l {
		if s[j] == '\n' { j++ }; if j >= l { break }
		sp = 0; for j+sp < l && s[j+sp] == ' ' { sp++ }
		nx := j + sp; if nx >= l || s[nx] == '\n' { j = nx; continue }
		r := ok(inp); r.tagInt = max(1, sp-n); return r
	}
	r := ok(inp); r.tagInt = 1; return r
}

func parse_int(inp Input, f PFn) Result {
	r := f(inp); if r.fail { return r }
	v := 0; for _, c := range r.val { if c >= '0' && c <= '9' { v = v*10 + int(c-'0') } }
	r.tagInt = v; return r
}

func parse_sym(inp Input, f PFn, sym string) Result {
	r := f(inp); if r.fail { return r }; r.tag = sym; return r
}

func val(inp Input, v string) Result { r := ok(inp); r.tag = v; return r }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] C-PRINTABLE 
func c_printable(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 0x9) },
        func(inp Input) Result { return match_cp(inp, 0x0A) },
        func(inp Input) Result { return match_cp(inp, 0x0D) },
        func(inp Input) Result { return match_range(inp, 0x20, 0x7E) },
        func(inp Input) Result { return match_cp(inp, 0x85) },
        func(inp Input) Result { return match_range(inp, 0xA0, 0xD7FF) },
        func(inp Input) Result { return match_range(inp, 0xE000, 0xFFFD) },
        func(inp Input) Result { return match_range(inp, 0x10000, 0x10FFFF) }})
}

// [2] NB-JSON 
func nb_json(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 0x9) },
        func(inp Input) Result { return match_range(inp, 0x20, 0x10FFFF) }})
}

// [3] C-BYTE-ORDER-MARK 
func c_byte_order_mark(inp Input) Result {
    return match_cp(inp, 0xFEFF)
}

// [4] C-SEQUENCE-ENTRY 
func c_sequence_entry(inp Input) Result {
    return match_cp(inp, 45)
}

// [5] C-MAPPING-KEY 
func c_mapping_key(inp Input) Result {
    return match_cp(inp, 63)
}

// [6] C-MAPPING-VALUE 
func c_mapping_value(inp Input) Result {
    return match_cp(inp, 58)
}

// [7] C-COLLECT-ENTRY 
func c_collect_entry(inp Input) Result {
    return match_cp(inp, 44)
}

// [8] C-SEQUENCE-START 
func c_sequence_start(inp Input) Result {
    return match_cp(inp, 91)
}

// [9] C-SEQUENCE-END 
func c_sequence_end(inp Input) Result {
    return match_cp(inp, 93)
}

// [10] C-MAPPING-START 
func c_mapping_start(inp Input) Result {
    return match_cp(inp, 123)
}

// [11] C-MAPPING-END 
func c_mapping_end(inp Input) Result {
    return match_cp(inp, 125)
}

// [12] C-COMMENT 
func c_comment(inp Input) Result {
    return match_cp(inp, 35)
}

// [13] C-ANCHOR 
func c_anchor(inp Input) Result {
    return match_cp(inp, 38)
}

// [14] C-ALIAS 
func c_alias(inp Input) Result {
    return match_cp(inp, 42)
}

// [15] C-TAG 
func c_tag(inp Input) Result {
    return match_cp(inp, 33)
}

// [16] C-LITERAL 
func c_literal(inp Input) Result {
    return match_cp(inp, 124)
}

// [17] C-FOLDED 
func c_folded(inp Input) Result {
    return match_cp(inp, 62)
}

// [18] C-SINGLE-QUOTE 
func c_single_quote(inp Input) Result {
    return match_cp(inp, 39)
}

// [19] C-DOUBLE-QUOTE 
func c_double_quote(inp Input) Result {
    return match_cp(inp, 34)
}

// [20] C-DIRECTIVE 
func c_directive(inp Input) Result {
    return match_cp(inp, 37)
}

// [21] C-RESERVED 
func c_reserved(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 64) },
        func(inp Input) Result { return match_cp(inp, 96) }})
}

// [22] C-INDICATOR 
func c_indicator(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_sequence_entry(inp) },
        func(inp Input) Result { return c_mapping_key(inp) },
        func(inp Input) Result { return c_mapping_value(inp) },
        func(inp Input) Result { return c_collect_entry(inp) },
        func(inp Input) Result { return c_sequence_start(inp) },
        func(inp Input) Result { return c_sequence_end(inp) },
        func(inp Input) Result { return c_mapping_start(inp) },
        func(inp Input) Result { return c_mapping_end(inp) },
        func(inp Input) Result { return c_comment(inp) },
        func(inp Input) Result { return c_anchor(inp) },
        func(inp Input) Result { return c_alias(inp) },
        func(inp Input) Result { return c_tag(inp) },
        func(inp Input) Result { return c_literal(inp) },
        func(inp Input) Result { return c_folded(inp) },
        func(inp Input) Result { return c_single_quote(inp) },
        func(inp Input) Result { return c_double_quote(inp) },
        func(inp Input) Result { return c_directive(inp) },
        func(inp Input) Result { return c_reserved(inp) }})
}

// [23] C-FLOW-INDICATOR 
func c_flow_indicator(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_collect_entry(inp) },
        func(inp Input) Result { return c_sequence_start(inp) },
        func(inp Input) Result { return c_sequence_end(inp) },
        func(inp Input) Result { return c_mapping_start(inp) },
        func(inp Input) Result { return c_mapping_end(inp) }})
}

// [24] B-LINE-FEED 
func b_line_feed(inp Input) Result {
    return match_cp(inp, 0x0A)
}

// [25] B-CARRIAGE-RETURN 
func b_carriage_return(inp Input) Result {
    return match_cp(inp, 0x0D)
}

// [26] B-CHAR 
func b_char(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return b_line_feed(inp) },
        func(inp Input) Result { return b_carriage_return(inp) }})
}

// [27] NB-CHAR 
func nb_char(inp Input) Result {
    return minus(inp, func(inp Input) Result { return c_printable(inp) }, func(inp Input) Result { return alt(inp, []PFn{
        func(inp Input) Result { return b_char(inp) },
        func(inp Input) Result { return c_byte_order_mark(inp) }}) })
}

// [28] B-BREAK 
func b_break(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return b_carriage_return(inp) },
            func(inp Input) Result { return b_line_feed(inp) }}) },
        func(inp Input) Result { return b_carriage_return(inp) },
        func(inp Input) Result { return b_line_feed(inp) }})
}

// [29] B-AS-LINE-FEED 
func b_as_line_feed(inp Input) Result {
    return b_break(inp)
}

// [30] B-NON-CONTENT 
func b_non_content(inp Input) Result {
    return b_break(inp)
}

// [31] S-SPACE 
func s_space(inp Input) Result {
    return match_cp(inp, 0x20)
}

// [32] S-TAB 
func s_tab(inp Input) Result {
    return match_cp(inp, 0x9)
}

// [33] S-WHITE 
func s_white(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return s_space(inp) },
        func(inp Input) Result { return s_tab(inp) }})
}

// [34] NS-CHAR 
func ns_char(inp Input) Result {
    return minus(inp, func(inp Input) Result { return nb_char(inp) }, func(inp Input) Result { return s_white(inp) })
}

// [35] NS-DEC-DIGIT 
func ns_dec_digit(inp Input) Result {
    return match_range(inp, 0x30, 0x39)
}

// [36] NS-HEX-DIGIT 
func ns_hex_digit(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return ns_dec_digit(inp) },
        func(inp Input) Result { return match_range(inp, 0x41, 0x46) },
        func(inp Input) Result { return match_range(inp, 0x61, 0x66) }})
}

// [37] NS-ASCII-LETTER 
func ns_ascii_letter(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return match_range(inp, 0x41, 0x5A) },
        func(inp Input) Result { return match_range(inp, 0x61, 0x7A) }})
}

// [38] NS-WORD-CHAR 
func ns_word_char(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return ns_dec_digit(inp) },
        func(inp Input) Result { return ns_ascii_letter(inp) },
        func(inp Input) Result { return match_cp(inp, 45) }})
}

// [39] NS-URI-CHAR 
func ns_uri_char(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 37) },
            func(inp Input) Result { return ns_hex_digit(inp) },
            func(inp Input) Result { return ns_hex_digit(inp) }}) },
        func(inp Input) Result { return ns_word_char(inp) },
        func(inp Input) Result { return match_cp(inp, 35) },
        func(inp Input) Result { return match_cp(inp, 59) },
        func(inp Input) Result { return match_cp(inp, 47) },
        func(inp Input) Result { return match_cp(inp, 63) },
        func(inp Input) Result { return match_cp(inp, 58) },
        func(inp Input) Result { return match_cp(inp, 64) },
        func(inp Input) Result { return match_cp(inp, 38) },
        func(inp Input) Result { return match_cp(inp, 61) },
        func(inp Input) Result { return match_cp(inp, 43) },
        func(inp Input) Result { return match_cp(inp, 36) },
        func(inp Input) Result { return match_cp(inp, 44) },
        func(inp Input) Result { return match_cp(inp, 95) },
        func(inp Input) Result { return match_cp(inp, 46) },
        func(inp Input) Result { return match_cp(inp, 33) },
        func(inp Input) Result { return match_cp(inp, 126) },
        func(inp Input) Result { return match_cp(inp, 42) },
        func(inp Input) Result { return match_cp(inp, 39) },
        func(inp Input) Result { return match_cp(inp, 40) },
        func(inp Input) Result { return match_cp(inp, 41) },
        func(inp Input) Result { return match_cp(inp, 91) },
        func(inp Input) Result { return match_cp(inp, 93) }})
}

// [40] NS-TAG-CHAR 
func ns_tag_char(inp Input) Result {
    return minus(inp, func(inp Input) Result { return ns_uri_char(inp) }, func(inp Input) Result { return alt(inp, []PFn{
        func(inp Input) Result { return c_tag(inp) },
        func(inp Input) Result { return c_flow_indicator(inp) }}) })
}

// [41] C-ESCAPE 
func c_escape(inp Input) Result {
    return match_cp(inp, 92)
}

// [42] NS-ESC-NULL 
func ns_esc_null(inp Input) Result {
    return match_cp(inp, 48)
}

// [43] NS-ESC-BELL 
func ns_esc_bell(inp Input) Result {
    return match_cp(inp, 97)
}

// [44] NS-ESC-BACKSPACE 
func ns_esc_backspace(inp Input) Result {
    return match_cp(inp, 98)
}

// [45] NS-ESC-HORIZONTAL-TAB 
func ns_esc_horizontal_tab(inp Input) Result {
    return match_cp(inp, 116)
}

// [46] NS-ESC-LINE-FEED 
func ns_esc_line_feed(inp Input) Result {
    return match_cp(inp, 110)
}

// [47] NS-ESC-VERTICAL-TAB 
func ns_esc_vertical_tab(inp Input) Result {
    return match_cp(inp, 118)
}

// [48] NS-ESC-FORM-FEED 
func ns_esc_form_feed(inp Input) Result {
    return match_cp(inp, 102)
}

// [49] NS-ESC-CARRIAGE-RETURN 
func ns_esc_carriage_return(inp Input) Result {
    return match_cp(inp, 114)
}

// [50] NS-ESC-ESCAPE 
func ns_esc_escape(inp Input) Result {
    return match_cp(inp, 101)
}

// [51] NS-ESC-SPACE 
func ns_esc_space(inp Input) Result {
    return match_cp(inp, 0x20)
}

// [52] NS-ESC-DOUBLE-QUOTE 
func ns_esc_double_quote(inp Input) Result {
    return match_cp(inp, 34)
}

// [53] NS-ESC-SLASH 
func ns_esc_slash(inp Input) Result {
    return match_cp(inp, 47)
}

// [54] NS-ESC-BACKSLASH 
func ns_esc_backslash(inp Input) Result {
    return match_cp(inp, 92)
}

// [55] NS-ESC-NEXT-LINE 
func ns_esc_next_line(inp Input) Result {
    return match_cp(inp, 78)
}

// [56] NS-ESC-NON-BREAKING-SPACE 
func ns_esc_non_breaking_space(inp Input) Result {
    return match_cp(inp, 95)
}

// [57] NS-ESC-LINE-SEPARATOR 
func ns_esc_line_separator(inp Input) Result {
    return match_cp(inp, 76)
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR 
func ns_esc_paragraph_separator(inp Input) Result {
    return match_cp(inp, 80)
}

// [59] NS-ESC-8-BIT 
func ns_esc_8_bit(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 120) },
        func(inp Input) Result { return rep(inp, 2, func(inp Input) Result { return ns_hex_digit(inp) }) }})
}

// [60] NS-ESC-16-BIT 
func ns_esc_16_bit(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 117) },
        func(inp Input) Result { return rep(inp, 4, func(inp Input) Result { return ns_hex_digit(inp) }) }})
}

// [61] NS-ESC-32-BIT 
func ns_esc_32_bit(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 85) },
        func(inp Input) Result { return rep(inp, 8, func(inp Input) Result { return ns_hex_digit(inp) }) }})
}

// [62] C-NS-ESC-CHAR 
func c_ns_esc_char(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_escape(inp) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return ns_esc_null(inp) },
            func(inp Input) Result { return ns_esc_bell(inp) },
            func(inp Input) Result { return ns_esc_backspace(inp) },
            func(inp Input) Result { return ns_esc_horizontal_tab(inp) },
            func(inp Input) Result { return ns_esc_line_feed(inp) },
            func(inp Input) Result { return ns_esc_vertical_tab(inp) },
            func(inp Input) Result { return ns_esc_form_feed(inp) },
            func(inp Input) Result { return ns_esc_carriage_return(inp) },
            func(inp Input) Result { return ns_esc_escape(inp) },
            func(inp Input) Result { return ns_esc_space(inp) },
            func(inp Input) Result { return ns_esc_double_quote(inp) },
            func(inp Input) Result { return ns_esc_slash(inp) },
            func(inp Input) Result { return ns_esc_backslash(inp) },
            func(inp Input) Result { return ns_esc_next_line(inp) },
            func(inp Input) Result { return ns_esc_non_breaking_space(inp) },
            func(inp Input) Result { return ns_esc_line_separator(inp) },
            func(inp Input) Result { return ns_esc_paragraph_separator(inp) },
            func(inp Input) Result { return ns_esc_8_bit(inp) },
            func(inp Input) Result { return ns_esc_16_bit(inp) },
            func(inp Input) Result { return ns_esc_32_bit(inp) }}) }})
}

// [63] S-INDENT 
func s_indent(inp Input, n int) Result {
    return rep(inp, n, func(inp Input) Result { return s_space(inp) })
}

// [64] S-INDENT-LT 
func s_indent_lt(inp Input, n int) Result {
    return star(inp, func(inp Input) Result { return s_space(inp) })
}

// [65] S-INDENT-LE 
func s_indent_le(inp Input, n int) Result {
    return star(inp, func(inp Input) Result { return s_space(inp) })
}

// [66] S-SEPARATE-IN-LINE 
func s_separate_in_line(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return s_white(inp) }) },
        func(inp Input) Result { return ok(inp) }})
}

// [67] S-LINE-PREFIX 
func s_line_prefix(inp Input, n int, c string) Result {
    return func() Result { if c == "BLOCK-IN" { return s_block_line_prefix(inp, n) }; if c == "BLOCK-OUT" { return s_block_line_prefix(inp, n) }; if c == "FLOW-IN" { return s_flow_line_prefix(inp, n) }; if c == "FLOW-OUT" { return s_flow_line_prefix(inp, n) }; return fail(inp, "no case") }()
}

// [68] S-BLOCK-LINE-PREFIX 
func s_block_line_prefix(inp Input, n int) Result {
    return s_indent(inp, n)
}

// [69] S-FLOW-LINE-PREFIX 
func s_flow_line_prefix(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_indent(inp, n) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate_in_line(inp) }) }})
}

// [70] L-EMPTY 
func l_empty(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return s_line_prefix(inp, n, c) },
            func(inp Input) Result { return s_indent_lt(inp, n) }}) },
        func(inp Input) Result { return b_as_line_feed(inp) }})
}

// [71] B-L-TRIMMED 
func b_l_trimmed(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return b_non_content(inp) },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return l_empty(inp, n, c) }) }})
}

// [72] B-AS-SPACE 
func b_as_space(inp Input) Result {
    return b_break(inp)
}

// [73] B-L-FOLDED 
func b_l_folded(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return b_l_trimmed(inp, n, c) },
        func(inp Input) Result { return b_as_space(inp) }})
}

// [74] S-FLOW-FOLDED 
func s_flow_folded(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate_in_line(inp) }) },
        func(inp Input) Result { return b_l_folded(inp, n, "FLOW-IN") },
        func(inp Input) Result { return s_flow_line_prefix(inp, n) }})
}

// [75] C-NB-COMMENT-TEXT 
func c_nb_comment_text(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_comment(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return nb_char(inp) }) }})
}

// [76] B-COMMENT 
func b_comment(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return b_non_content(inp) },
        func(inp Input) Result { return ok(inp) }})
}

// [77] S-B-COMMENT 
func s_b_comment(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return s_separate_in_line(inp) },
            func(inp Input) Result { return opt(inp, func(inp Input) Result { return c_nb_comment_text(inp) }) }}) }) },
        func(inp Input) Result { return b_comment(inp) }})
}

// [78] L-COMMENT 
func l_comment(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_separate_in_line(inp) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return c_nb_comment_text(inp) }) },
        func(inp Input) Result { return b_non_content(inp) }})
}

// [79] S-L-COMMENTS 
func s_l_comments(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return s_b_comment(inp) },
            func(inp Input) Result { return ok(inp) }}) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_comment(inp) }) }})
}

// [80] S-SEPARATE 
func s_separate(inp Input, n int, c string) Result {
    return func() Result { if c == "BLOCK-OUT" { return s_separate_lines(inp, n) }; if c == "BLOCK-IN" { return s_separate_lines(inp, n) }; if c == "FLOW-OUT" { return s_separate_lines(inp, n) }; if c == "FLOW-IN" { return s_separate_lines(inp, n) }; if c == "BLOCK-KEY" { return s_separate_in_line(inp) }; if c == "FLOW-KEY" { return s_separate_in_line(inp) }; return fail(inp, "no case") }()
}

// [81] S-SEPARATE-LINES 
func s_separate_lines(inp Input, n int) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return s_l_comments(inp) },
            func(inp Input) Result { return s_flow_line_prefix(inp, n) }}) },
        func(inp Input) Result { return s_separate_in_line(inp) }})
}

// [82] L-DIRECTIVE 
func l_directive(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_directive(inp) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return ns_yaml_directive(inp) },
            func(inp Input) Result { return ns_tag_directive(inp) },
            func(inp Input) Result { return ns_reserved_directive(inp) }}) },
        func(inp Input) Result { return s_l_comments(inp) }})
}

// [83] NS-RESERVED-DIRECTIVE 
func ns_reserved_directive(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_directive_name(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return s_separate_in_line(inp) },
            func(inp Input) Result { return ns_directive_parameter(inp) }}) }) }})
}

// [84] NS-DIRECTIVE-NAME 
func ns_directive_name(inp Input) Result {
    return plus_(inp, func(inp Input) Result { return ns_char(inp) })
}

// [85] NS-DIRECTIVE-PARAMETER 
func ns_directive_parameter(inp Input) Result {
    return plus_(inp, func(inp Input) Result { return ns_char(inp) })
}

// [86] NS-YAML-DIRECTIVE 
func ns_yaml_directive(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_str(inp, "YAML") },
        func(inp Input) Result { return s_separate_in_line(inp) },
        func(inp Input) Result { return ns_yaml_version(inp) }})
}

// [87] NS-YAML-VERSION 
func ns_yaml_version(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return ns_dec_digit(inp) }) },
        func(inp Input) Result { return match_cp(inp, 46) },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return ns_dec_digit(inp) }) }})
}

// [88] NS-TAG-DIRECTIVE 
func ns_tag_directive(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_str(inp, "TAG") },
        func(inp Input) Result { return s_separate_in_line(inp) },
        func(inp Input) Result { return c_tag_handle(inp) },
        func(inp Input) Result { return s_separate_in_line(inp) },
        func(inp Input) Result { return ns_tag_prefix(inp) }})
}

// [89] C-TAG-HANDLE 
func c_tag_handle(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_named_tag_handle(inp) },
        func(inp Input) Result { return c_secondary_tag_handle(inp) },
        func(inp Input) Result { return c_primary_tag_handle(inp) }})
}

// [90] C-PRIMARY-TAG-HANDLE 
func c_primary_tag_handle(inp Input) Result {
    return match_cp(inp, 33)
}

// [91] C-SECONDARY-TAG-HANDLE 
func c_secondary_tag_handle(inp Input) Result {
    return match_str(inp, "!!")
}

// [92] C-NAMED-TAG-HANDLE 
func c_named_tag_handle(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 33) },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return ns_word_char(inp) }) },
        func(inp Input) Result { return match_cp(inp, 33) }})
}

// [93] NS-TAG-PREFIX 
func ns_tag_prefix(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_ns_local_tag_prefix(inp) },
        func(inp Input) Result { return ns_global_tag_prefix(inp) }})
}

// [94] C-NS-LOCAL-TAG-PREFIX 
func c_ns_local_tag_prefix(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 33) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return ns_uri_char(inp) }) }})
}

// [95] NS-GLOBAL-TAG-PREFIX 
func ns_global_tag_prefix(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_tag_char(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return ns_uri_char(inp) }) }})
}

// [96] C-NS-PROPERTIES 
func c_ns_properties(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return c_ns_tag_property(inp) },
            func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return s_separate(inp, n, c) },
                func(inp Input) Result { return c_ns_anchor_property(inp) }}) }) }}) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return c_ns_anchor_property(inp) },
            func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return s_separate(inp, n, c) },
                func(inp Input) Result { return c_ns_tag_property(inp) }}) }) }}) }})
}

// [97] C-NS-TAG-PROPERTY 
func c_ns_tag_property(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_verbatim_tag(inp) },
        func(inp Input) Result { return c_ns_shorthand_tag(inp) },
        func(inp Input) Result { return c_non_specific_tag(inp) }})
}

// [98] C-VERBATIM-TAG 
func c_verbatim_tag(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_str(inp, "!<") },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return ns_uri_char(inp) }) },
        func(inp Input) Result { return match_cp(inp, 62) }})
}

// [99] C-NS-SHORTHAND-TAG 
func c_ns_shorthand_tag(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_tag_handle(inp) },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return ns_tag_char(inp) }) }})
}

// [100] C-NON-SPECIFIC-TAG 
func c_non_specific_tag(inp Input) Result {
    return match_cp(inp, 33)
}

// [101] C-NS-ANCHOR-PROPERTY 
func c_ns_anchor_property(inp Input) Result {
    return build(inp, "ANCHOR", func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return c_anchor(inp) },
        func(inp Input) Result { return scalar(inp, func(inp Input) Result { return ns_anchor_name(inp) }) }}) })
}

// [102] NS-ANCHOR-CHAR 
func ns_anchor_char(inp Input) Result {
    return minus(inp, func(inp Input) Result { return ns_char(inp) }, func(inp Input) Result { return c_flow_indicator(inp) })
}

// [103] NS-ANCHOR-NAME 
func ns_anchor_name(inp Input) Result {
    return plus_(inp, func(inp Input) Result { return ns_anchor_char(inp) })
}

// [104] C-NS-ALIAS-NODE 
func c_ns_alias_node(inp Input) Result {
    return build(inp, "ALIAS", func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return c_alias(inp) },
        func(inp Input) Result { return scalar(inp, func(inp Input) Result { return ns_anchor_name(inp) }) }}) })
}

// [105] E-SCALAR 
func e_scalar(inp Input) Result {
    return ok(inp)
}

// [106] E-NODE 
func e_node(inp Input) Result {
    return e_scalar(inp)
}

// [107] NB-DOUBLE-CHAR 
func nb_double_char(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_ns_esc_char(inp) },
        func(inp Input) Result { return minus(inp, func(inp Input) Result { return nb_json(inp) }, func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 92) },
            func(inp Input) Result { return match_cp(inp, 34) }}) }) }})
}

// [108] NS-DOUBLE-CHAR 
func ns_double_char(inp Input) Result {
    return minus(inp, func(inp Input) Result { return nb_double_char(inp) }, func(inp Input) Result { return s_white(inp) })
}

// [109] C-DOUBLE-QUOTED 
func c_double_quoted(inp Input, n int, c string) Result {
    return scalar(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 34) },
        func(inp Input) Result { return nb_double_text(inp, n, c) },
        func(inp Input) Result { return match_cp(inp, 34) }}) })
}

// [110] NB-DOUBLE-TEXT 
func nb_double_text(inp Input, n int, c string) Result {
    return func() Result { if c == "FLOW-OUT" { return nb_double_multi_line(inp, n) }; if c == "FLOW-IN" { return nb_double_multi_line(inp, n) }; if c == "BLOCK-KEY" { return nb_double_one_line(inp) }; if c == "FLOW-KEY" { return nb_double_one_line(inp) }; return fail(inp, "no case") }()
}

// [111] NB-DOUBLE-ONE-LINE 
func nb_double_one_line(inp Input) Result {
    return star(inp, func(inp Input) Result { return nb_double_char(inp) })
}

// [112] S-DOUBLE-ESCAPED 
func s_double_escaped(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) },
        func(inp Input) Result { return match_cp(inp, 92) },
        func(inp Input) Result { return b_non_content(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_empty(inp, n, "FLOW-IN") }) },
        func(inp Input) Result { return s_flow_line_prefix(inp, n) }})
}

// [113] S-DOUBLE-BREAK 
func s_double_break(inp Input, n int) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return s_double_escaped(inp, n) },
        func(inp Input) Result { return s_flow_folded(inp, n) }})
}

// [114] NB-NS-DOUBLE-IN-LINE 
func nb_ns_double_in_line(inp Input) Result {
    return star(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) },
        func(inp Input) Result { return ns_double_char(inp) }}) })
}

// [115] S-DOUBLE-NEXT-LINE 
func s_double_next_line(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_double_break(inp, n) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return ns_double_char(inp) },
            func(inp Input) Result { return nb_ns_double_in_line(inp) },
            func(inp Input) Result { return alt(inp, []PFn{
                func(inp Input) Result { return s_double_next_line(inp, n) },
                func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) }}) }}) }) }})
}

// [116] NB-DOUBLE-MULTI-LINE 
func nb_double_multi_line(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return nb_ns_double_in_line(inp) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return s_double_next_line(inp, n) },
            func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) }}) }})
}

// [117] C-QUOTED-QUOTE 
func c_quoted_quote(inp Input) Result {
    return match_str(inp, "''")
}

// [118] NB-SINGLE-CHAR 
func nb_single_char(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_quoted_quote(inp) },
        func(inp Input) Result { return minus(inp, func(inp Input) Result { return nb_json(inp) }, func(inp Input) Result { return match_cp(inp, 39) }) }})
}

// [119] NS-SINGLE-CHAR 
func ns_single_char(inp Input) Result {
    return minus(inp, func(inp Input) Result { return nb_single_char(inp) }, func(inp Input) Result { return s_white(inp) })
}

// [120] C-SINGLE-QUOTED 
func c_single_quoted(inp Input, n int, c string) Result {
    return scalar(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 39) },
        func(inp Input) Result { return nb_single_text(inp, n, c) },
        func(inp Input) Result { return match_cp(inp, 39) }}) })
}

// [121] NB-SINGLE-TEXT 
func nb_single_text(inp Input, n int, c string) Result {
    return func() Result { if c == "FLOW-OUT" { return nb_single_multi_line(inp, n) }; if c == "FLOW-IN" { return nb_single_multi_line(inp, n) }; if c == "BLOCK-KEY" { return nb_single_one_line(inp) }; if c == "FLOW-KEY" { return nb_single_one_line(inp) }; return fail(inp, "no case") }()
}

// [122] NB-SINGLE-ONE-LINE 
func nb_single_one_line(inp Input) Result {
    return star(inp, func(inp Input) Result { return nb_single_char(inp) })
}

// [123] NS-SINGLE-IN-LINE 
func ns_single_in_line(inp Input) Result {
    return star(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) },
        func(inp Input) Result { return ns_single_char(inp) }}) })
}

// [124] S-SINGLE-NEXT-LINE 
func s_single_next_line(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_flow_folded(inp, n) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return ns_single_char(inp) },
            func(inp Input) Result { return ns_single_in_line(inp) },
            func(inp Input) Result { return alt(inp, []PFn{
                func(inp Input) Result { return s_single_next_line(inp, n) },
                func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) }}) }}) }) }})
}

// [125] NB-SINGLE-MULTI-LINE 
func nb_single_multi_line(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_single_in_line(inp) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return s_single_next_line(inp, n) },
            func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) }}) }})
}

// [126] NS-PLAIN-FIRST 
func ns_plain_first(inp Input, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return minus(inp, func(inp Input) Result { return ns_char(inp) }, func(inp Input) Result { return c_indicator(inp) }) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return alt(inp, []PFn{
                func(inp Input) Result { return match_cp(inp, 63) },
                func(inp Input) Result { return match_cp(inp, 58) },
                func(inp Input) Result { return match_cp(inp, 45) }}) },
            func(inp Input) Result { return ahead(inp, func(inp Input) Result { return ns_plain_safe(inp, c) }) }}) }})
}

// [127] NS-PLAIN-SAFE 
func ns_plain_safe(inp Input, c string) Result {
    return func() Result { if c == "FLOW-OUT" { return ns_plain_safe_out(inp) }; if c == "FLOW-IN" { return ns_plain_safe_in(inp) }; if c == "BLOCK-KEY" { return ns_plain_safe_out(inp) }; if c == "FLOW-KEY" { return ns_plain_safe_in(inp) }; return fail(inp, "no case") }()
}

// [128] NS-PLAIN-SAFE-OUT 
func ns_plain_safe_out(inp Input) Result {
    return ns_char(inp)
}

// [129] NS-PLAIN-SAFE-IN 
func ns_plain_safe_in(inp Input) Result {
    return minus(inp, func(inp Input) Result { return ns_char(inp) }, func(inp Input) Result { return c_flow_indicator(inp) })
}

// [130] NS-PLAIN-CHAR 
func ns_plain_char(inp Input, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return minus(inp, func(inp Input) Result { return ns_plain_safe(inp, c) }, func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 58) },
            func(inp Input) Result { return match_cp(inp, 35) }}) }) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return behind(inp, func(inp Input) Result { return ns_char(inp) }) },
            func(inp Input) Result { return match_cp(inp, 35) }}) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 58) },
            func(inp Input) Result { return ahead(inp, func(inp Input) Result { return ns_plain_safe(inp, c) }) }}) }})
}

// [131] NS-PLAIN 
func ns_plain(inp Input, n int, c string) Result {
    return scalar(inp, func(inp Input) Result { return func() Result { if c == "FLOW-OUT" { return ns_plain_multi_line(inp, n, c) }; if c == "FLOW-IN" { return ns_plain_multi_line(inp, n, c) }; if c == "BLOCK-KEY" { return ns_plain_one_line(inp, c) }; if c == "FLOW-KEY" { return ns_plain_one_line(inp, c) }; return fail(inp, "no case") }() })
}

// [132] NB-NS-PLAIN-IN-LINE 
func nb_ns_plain_in_line(inp Input, c string) Result {
    return star(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return s_white(inp) }) },
        func(inp Input) Result { return ns_plain_char(inp, c) }}) })
}

// [133] NS-PLAIN-ONE-LINE 
func ns_plain_one_line(inp Input, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_plain_first(inp, c) },
        func(inp Input) Result { return nb_ns_plain_in_line(inp, c) }})
}

// [134] S-NS-PLAIN-NEXT-LINE 
func s_ns_plain_next_line(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_flow_folded(inp, n) },
        func(inp Input) Result { return neg(inp, func(inp Input) Result { return c_forbidden(inp) }) },
        func(inp Input) Result { return ns_plain_char(inp, c) },
        func(inp Input) Result { return nb_ns_plain_in_line(inp, c) }})
}

// [135] NS-PLAIN-MULTI-LINE 
func ns_plain_multi_line(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_plain_one_line(inp, c) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return s_ns_plain_next_line(inp, n, c) }) }})
}

// [137] C-FLOW-SEQUENCE 
func c_flow_sequence(inp Input, n int, c string) Result {
    return build(inp, "SEQUENCE", func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 91) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return collect(inp, func(inp Input) Result { return ns_s_flow_seq_entries(inp, n, inFlow(c)) }) }) },
        func(inp Input) Result { return match_cp(inp, 93) }}) })
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
func ns_s_flow_seq_entries(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_flow_seq_entry(inp, n, c) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 44) },
            func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
            func(inp Input) Result { return opt(inp, func(inp Input) Result { return ns_s_flow_seq_entries(inp, n, c) }) }}) }) }})
}

// [139] NS-FLOW-SEQ-ENTRY 
func ns_flow_seq_entry(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return ns_flow_pair(inp, n, c) },
        func(inp Input) Result { return ns_flow_node(inp, n, c) }})
}

// [140] C-FLOW-MAPPING 
func c_flow_mapping(inp Input, n int, c string) Result {
    return build(inp, "MAPPING", func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 123) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return collect(inp, func(inp Input) Result { return ns_s_flow_map_entries(inp, n, inFlow(c)) }) }) },
        func(inp Input) Result { return match_cp(inp, 125) }}) })
}

// [141] NS-S-FLOW-MAP-ENTRIES 
func ns_s_flow_map_entries(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_flow_map_entry(inp, n, c) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 44) },
            func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
            func(inp Input) Result { return opt(inp, func(inp Input) Result { return ns_s_flow_map_entries(inp, n, c) }) }}) }) }})
}

// [142] NS-FLOW-MAP-ENTRY 
func ns_flow_map_entry(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 63) },
            func(inp Input) Result { return s_separate(inp, n, c) },
            func(inp Input) Result { return ns_flow_map_explicit_entry(inp, n, c) }}) },
        func(inp Input) Result { return ns_flow_map_implicit_entry(inp, n, c) }})
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
func ns_flow_map_explicit_entry(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return ns_flow_map_implicit_entry(inp, n, c) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return e_node(inp) },
            func(inp Input) Result { return e_node(inp) }}) }})
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
func ns_flow_map_implicit_entry(inp Input, n int, c string) Result {
    return build(inp, "PAIR", func(inp Input) Result { return alt(inp, []PFn{
        func(inp Input) Result { return ns_flow_map_yaml_key_entry(inp, n, c) },
        func(inp Input) Result { return c_ns_flow_map_empty_key_entry(inp, n, c) },
        func(inp Input) Result { return c_ns_flow_map_json_key_entry(inp, n, c) }}) })
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
func ns_flow_map_yaml_key_entry(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_flow_yaml_node(inp, n, c) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
                func(inp Input) Result { return c_ns_flow_map_separate_value(inp, n, c) }}) },
            func(inp Input) Result { return e_node(inp) }}) }})
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
func c_ns_flow_map_empty_key_entry(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return e_node(inp) },
        func(inp Input) Result { return c_ns_flow_map_separate_value(inp, n, c) }})
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
func c_ns_flow_map_separate_value(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 58) },
        func(inp Input) Result { return neg(inp, func(inp Input) Result { return ns_plain_safe(inp, c) }) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return s_separate(inp, n, c) },
                func(inp Input) Result { return ns_flow_node(inp, n, c) }}) },
            func(inp Input) Result { return e_node(inp) }}) }})
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
func c_ns_flow_map_json_key_entry(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_flow_json_node(inp, n, c) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
                func(inp Input) Result { return c_ns_flow_map_adjacent_value(inp, n, c) }}) },
            func(inp Input) Result { return e_node(inp) }}) }})
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
func c_ns_flow_map_adjacent_value(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 58) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate(inp, n, c) }) },
                func(inp Input) Result { return ns_flow_node(inp, n, c) }}) },
            func(inp Input) Result { return e_node(inp) }}) }})
}

// [150] NS-FLOW-PAIR 
func ns_flow_pair(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 63) },
            func(inp Input) Result { return s_separate(inp, n, c) },
            func(inp Input) Result { return ns_flow_map_explicit_entry(inp, n, c) }}) },
        func(inp Input) Result { return ns_flow_pair_entry(inp, n, c) }})
}

// [151] NS-FLOW-PAIR-ENTRY 
func ns_flow_pair_entry(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return ns_flow_pair_yaml_key_entry(inp, n, c) },
        func(inp Input) Result { return c_ns_flow_map_empty_key_entry(inp, n, c) },
        func(inp Input) Result { return c_ns_flow_pair_json_key_entry(inp, n, c) }})
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
func ns_flow_pair_yaml_key_entry(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_s_implicit_yaml_key(inp, "FLOW-KEY") },
        func(inp Input) Result { return c_ns_flow_map_separate_value(inp, n, c) }})
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
func c_ns_flow_pair_json_key_entry(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_s_implicit_json_key(inp, "FLOW-KEY") },
        func(inp Input) Result { return c_ns_flow_map_adjacent_value(inp, n, c) }})
}

// [154] NS-S-IMPLICIT-YAML-KEY 
func ns_s_implicit_yaml_key(inp Input, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_flow_yaml_node(inp, 0, c) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate_in_line(inp) }) }})
}

// [155] C-S-IMPLICIT-JSON-KEY 
func c_s_implicit_json_key(inp Input, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_flow_json_node(inp, 0, c) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return s_separate_in_line(inp) }) }})
}

// [156] NS-FLOW-YAML-CONTENT 
func ns_flow_yaml_content(inp Input, n int, c string) Result {
    return ns_plain(inp, n, c)
}

// [157] C-FLOW-JSON-CONTENT 
func c_flow_json_content(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_flow_sequence(inp, n, c) },
        func(inp Input) Result { return c_flow_mapping(inp, n, c) },
        func(inp Input) Result { return c_single_quoted(inp, n, c) },
        func(inp Input) Result { return c_double_quoted(inp, n, c) }})
}

// [158] NS-FLOW-CONTENT 
func ns_flow_content(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return ns_flow_yaml_content(inp, n, c) },
        func(inp Input) Result { return c_flow_json_content(inp, n, c) }})
}

// [159] NS-FLOW-YAML-NODE 
func ns_flow_yaml_node(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_ns_alias_node(inp) },
        func(inp Input) Result { return ns_flow_yaml_content(inp, n, c) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return c_ns_properties(inp, n, c) },
            func(inp Input) Result { return alt(inp, []PFn{
                func(inp Input) Result { return seq(inp, []PFn{
                    func(inp Input) Result { return s_separate(inp, n, c) },
                    func(inp Input) Result { return ns_flow_yaml_content(inp, n, c) }}) },
                func(inp Input) Result { return e_scalar(inp) }}) }}) }})
}

// [160] C-FLOW-JSON-NODE 
func c_flow_json_node(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return c_ns_properties(inp, n, c) },
            func(inp Input) Result { return s_separate(inp, n, c) }}) }) },
        func(inp Input) Result { return c_flow_json_content(inp, n, c) }})
}

// [161] NS-FLOW-NODE 
func ns_flow_node(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_ns_alias_node(inp) },
        func(inp Input) Result { return ns_flow_content(inp, n, c) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return c_ns_properties(inp, n, c) },
            func(inp Input) Result { return alt(inp, []PFn{
                func(inp Input) Result { return seq(inp, []PFn{
                    func(inp Input) Result { return s_separate(inp, n, c) },
                    func(inp Input) Result { return ns_flow_content(inp, n, c) }}) },
                func(inp Input) Result { return e_scalar(inp) }}) }}) }})
}

// [162] C-B-BLOCK-HEADER 
func c_b_block_header(inp Input, n int) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_int(inp, func(inp Input) Result { return ns_dec_digit(inp) }) },
            func(inp Input) Result { return detect_indent(inp, n) }}); if r.fail { return r }; m := r.tagInt; _ = m; inp := r.rest; return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 45) }, "STRIP") },
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 43) }, "KEEP") },
            func(inp Input) Result { return val(inp, "CLIP") }}); if r.fail { return r }; t := r.tag; _ = t; inp := r.rest; return s_b_comment(inp) }() }() },
        func(inp Input) Result { return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 45) }, "STRIP") },
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 43) }, "KEEP") },
            func(inp Input) Result { return val(inp, "CLIP") }}); if r.fail { return r }; t := r.tag; _ = t; inp := r.rest; return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_int(inp, func(inp Input) Result { return ns_dec_digit(inp) }) },
            func(inp Input) Result { return detect_indent(inp, n) }}); if r.fail { return r }; m := r.tagInt; _ = m; inp := r.rest; return s_b_comment(inp) }() }() }})
}

// [163] C-INDENTATION-INDICATOR 
func c_indentation_indicator(inp Input, n int) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return ns_dec_digit(inp) },
        func(inp Input) Result { return ok(inp) }})
}

// [164] C-CHOMPING-INDICATOR 
func c_chomping_indicator(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 45) },
        func(inp Input) Result { return match_cp(inp, 43) },
        func(inp Input) Result { return ok(inp) }})
}

// [165] B-CHOMPED-LAST 
func b_chomped_last(inp Input, t string) Result {
    return func() Result { if t == "STRIP" { return b_non_content(inp) }; if t == "CLIP" { return b_as_line_feed(inp) }; if t == "KEEP" { return b_as_line_feed(inp) }; return fail(inp, "no case") }()
}

// [166] L-CHOMPED-EMPTY 
func l_chomped_empty(inp Input, n int, t string) Result {
    return func() Result { if t == "STRIP" { return l_strip_empty(inp, n) }; if t == "CLIP" { return l_strip_empty(inp, n) }; if t == "KEEP" { return l_keep_empty(inp, n) }; return fail(inp, "no case") }()
}

// [167] L-STRIP-EMPTY 
func l_strip_empty(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return s_indent_le(inp, n) },
            func(inp Input) Result { return b_non_content(inp) }}) }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return l_trail_comments(inp, n) }) }})
}

// [168] L-KEEP-EMPTY 
func l_keep_empty(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_empty(inp, n, "BLOCK-IN") }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return l_trail_comments(inp, n) }) }})
}

// [169] L-TRAIL-COMMENTS 
func l_trail_comments(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_indent_lt(inp, n) },
        func(inp Input) Result { return c_nb_comment_text(inp) },
        func(inp Input) Result { return b_comment(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_comment(inp) }) }})
}

// [170] C-L+LITERAL 
func c_lliteral(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 124) },
        func(inp Input) Result { return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_int(inp, func(inp Input) Result { return ns_dec_digit(inp) }) },
            func(inp Input) Result { return detect_indent(inp, n) }}); if r.fail { return r }; m := r.tagInt; _ = m; inp := r.rest; return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 45) }, "STRIP") },
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 43) }, "KEEP") },
            func(inp Input) Result { return val(inp, "CLIP") }}); if r.fail { return r }; t := r.tag; _ = t; inp := r.rest; return seq(inp, []PFn{
            func(inp Input) Result { return s_b_comment(inp) },
            func(inp Input) Result { return l_literal_content(inp, (n + m), t) }}) }() }() }})
}

// [171] L-NB-LITERAL-TEXT 
func l_nb_literal_text(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_empty(inp, n, "BLOCK-IN") }) },
        func(inp Input) Result { return s_indent(inp, n) },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return nb_char(inp) }) }})
}

// [172] B-NB-LITERAL-NEXT 
func b_nb_literal_next(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return b_as_line_feed(inp) },
        func(inp Input) Result { return l_nb_literal_text(inp, n) }})
}

// [173] L-LITERAL-CONTENT 
func l_literal_content(inp Input, n int, t string) Result {
    return scalar(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return l_nb_literal_text(inp, n) },
            func(inp Input) Result { return star(inp, func(inp Input) Result { return b_nb_literal_next(inp, n) }) },
            func(inp Input) Result { return b_chomped_last(inp, t) }}) }) },
        func(inp Input) Result { return l_chomped_empty(inp, n, t) }}) })
}

// [174] C-L+FOLDED 
func c_lfolded(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 62) },
        func(inp Input) Result { return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_int(inp, func(inp Input) Result { return ns_dec_digit(inp) }) },
            func(inp Input) Result { return detect_indent(inp, n) }}); if r.fail { return r }; m := r.tagInt; _ = m; inp := r.rest; return func() Result { r := alt(inp, []PFn{
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 45) }, "STRIP") },
            func(inp Input) Result { return parse_sym(inp, func(inp Input) Result { return match_cp(inp, 43) }, "KEEP") },
            func(inp Input) Result { return val(inp, "CLIP") }}); if r.fail { return r }; t := r.tag; _ = t; inp := r.rest; return seq(inp, []PFn{
            func(inp Input) Result { return s_b_comment(inp) },
            func(inp Input) Result { return l_folded_content(inp, (n + m), t) }}) }() }() }})
}

// [175] S-NB-FOLDED-TEXT 
func s_nb_folded_text(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_indent(inp, n) },
        func(inp Input) Result { return ns_char(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return nb_char(inp) }) }})
}

// [176] L-NB-FOLDED-LINES 
func l_nb_folded_lines(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_nb_folded_text(inp, n) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return b_l_folded(inp, n, "BLOCK-IN") },
            func(inp Input) Result { return s_nb_folded_text(inp, n) }}) }) }})
}

// [177] S-NB-SPACED-TEXT 
func s_nb_spaced_text(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_indent(inp, n) },
        func(inp Input) Result { return s_white(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return nb_char(inp) }) }})
}

// [178] B-L-SPACED 
func b_l_spaced(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return b_as_line_feed(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_empty(inp, n, "BLOCK-IN") }) }})
}

// [179] L-NB-SPACED-LINES 
func l_nb_spaced_lines(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_nb_spaced_text(inp, n) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return b_l_spaced(inp, n) },
            func(inp Input) Result { return s_nb_spaced_text(inp, n) }}) }) }})
}

// [180] L-NB-SAME-LINES 
func l_nb_same_lines(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_empty(inp, n, "BLOCK-IN") }) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return l_nb_folded_lines(inp, n) },
            func(inp Input) Result { return l_nb_spaced_lines(inp, n) }}) }})
}

// [181] L-NB-DIFF-LINES 
func l_nb_diff_lines(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return l_nb_same_lines(inp, n) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return b_as_line_feed(inp) },
            func(inp Input) Result { return l_nb_same_lines(inp, n) }}) }) }})
}

// [182] L-FOLDED-CONTENT 
func l_folded_content(inp Input, n int, t string) Result {
    return scalar(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return l_nb_diff_lines(inp, n) },
            func(inp Input) Result { return b_chomped_last(inp, t) }}) }) },
        func(inp Input) Result { return l_chomped_empty(inp, n, t) }}) })
}

// [183] L+BLOCK-SEQUENCE 
func lblock_sequence(inp Input, n int) Result {
    return build(inp, "SEQUENCE", func(inp Input) Result { return func() Result { r := detect_indent(inp, n); if r.fail { return r }; m := r.tagInt; _ = m; inp := r.rest; return collect(inp, func(inp Input) Result { return plus_(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return s_indent(inp, (n + m)) },
        func(inp Input) Result { return c_l_block_seq_entry(inp, (n + m)) }}) }) }) }() })
}

// [184] C-L-BLOCK-SEQ-ENTRY 
func c_l_block_seq_entry(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 45) },
        func(inp Input) Result { return neg(inp, func(inp Input) Result { return ns_char(inp) }) },
        func(inp Input) Result { return s_lblock_indented(inp, n, "BLOCK-IN") }})
}

// [185] S-L+BLOCK-INDENTED 
func s_lblock_indented(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return func() Result { r := detect_indent(inp, 0); if r.fail { return r }; m := r.tagInt; _ = m; inp := r.rest; return seq(inp, []PFn{
            func(inp Input) Result { return s_indent(inp, m) },
            func(inp Input) Result { return alt(inp, []PFn{
                func(inp Input) Result { return ns_l_compact_sequence(inp, (n + 1 + m)) },
                func(inp Input) Result { return ns_l_compact_mapping(inp, (n + 1 + m)) }}) }}) }() },
        func(inp Input) Result { return s_lblock_node(inp, n, c) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return e_node(inp) },
            func(inp Input) Result { return s_l_comments(inp) }}) }})
}

// [186] NS-L-COMPACT-SEQUENCE 
func ns_l_compact_sequence(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_l_block_seq_entry(inp, n) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return s_indent(inp, n) },
            func(inp Input) Result { return c_l_block_seq_entry(inp, n) }}) }) }})
}

// [187] L+BLOCK-MAPPING 
func lblock_mapping(inp Input, n int) Result {
    return build(inp, "MAPPING", func(inp Input) Result { return func() Result { r := detect_indent(inp, n); if r.fail { return r }; m := r.tagInt; _ = m; inp := r.rest; return collect(inp, func(inp Input) Result { return plus_(inp, func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return s_indent(inp, (n + m)) },
        func(inp Input) Result { return ns_l_block_map_entry(inp, (n + m)) }}) }) }) }() })
}

// [188] NS-L-BLOCK-MAP-ENTRY 
func ns_l_block_map_entry(inp Input, n int) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_l_block_map_explicit_entry(inp, n) },
        func(inp Input) Result { return ns_l_block_map_implicit_entry(inp, n) }})
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
func c_l_block_map_explicit_entry(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_l_block_map_explicit_key(inp, n) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return l_block_map_explicit_value(inp, n) },
            func(inp Input) Result { return e_node(inp) }}) }})
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
func c_l_block_map_explicit_key(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 63) },
        func(inp Input) Result { return s_lblock_indented(inp, n, "BLOCK-OUT") }})
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
func l_block_map_explicit_value(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_indent(inp, n) },
        func(inp Input) Result { return match_cp(inp, 58) },
        func(inp Input) Result { return s_lblock_indented(inp, n, "BLOCK-OUT") }})
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
func ns_l_block_map_implicit_entry(inp Input, n int) Result {
    return build(inp, "PAIR", func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return scalar(inp, func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return ns_s_block_map_implicit_key(inp) },
            func(inp Input) Result { return e_node(inp) }}) }) },
        func(inp Input) Result { return c_l_block_map_implicit_value(inp, n) }}) })
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
func ns_s_block_map_implicit_key(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return c_s_implicit_json_key(inp, "BLOCK-KEY") },
        func(inp Input) Result { return ns_s_implicit_yaml_key(inp, "BLOCK-KEY") }})
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
func c_l_block_map_implicit_value(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 58) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return s_lblock_node(inp, n, "BLOCK-OUT") },
            func(inp Input) Result { return scalar(inp, func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return e_node(inp) },
                func(inp Input) Result { return s_l_comments(inp) }}) }) }}) }})
}

// [195] NS-L-COMPACT-MAPPING 
func ns_l_compact_mapping(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ns_l_block_map_entry(inp, n) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return s_indent(inp, n) },
            func(inp Input) Result { return ns_l_block_map_entry(inp, n) }}) }) }})
}

// [196] S-L+BLOCK-NODE 
func s_lblock_node(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return s_lblock_in_block(inp, n, c) },
        func(inp Input) Result { return s_lflow_in_block(inp, n) }})
}

// [197] S-L+FLOW-IN-BLOCK 
func s_lflow_in_block(inp Input, n int) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_separate(inp, (n + 1), "FLOW-OUT") },
        func(inp Input) Result { return ns_flow_node(inp, (n + 1), "FLOW-OUT") },
        func(inp Input) Result { return s_l_comments(inp) }})
}

// [198] S-L+BLOCK-IN-BLOCK 
func s_lblock_in_block(inp Input, n int, c string) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return s_lblock_scalar(inp, n, c) },
        func(inp Input) Result { return s_lblock_collection(inp, n, c) }})
}

// [199] S-L+BLOCK-SCALAR 
func s_lblock_scalar(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return s_separate(inp, (n + 1), c) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return c_ns_properties(inp, (n + 1), c) },
            func(inp Input) Result { return s_separate(inp, (n + 1), c) }}) }) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return c_lliteral(inp, n) },
            func(inp Input) Result { return c_lfolded(inp, n) }}) }})
}

// [200] S-L+BLOCK-COLLECTION 
func s_lblock_collection(inp Input, n int, c string) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return s_separate(inp, (n + 1), c) },
            func(inp Input) Result { return c_ns_properties(inp, (n + 1), c) }}) }) },
        func(inp Input) Result { return s_l_comments(inp) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return lblock_sequence(inp, seqSpaces(n, c)) },
            func(inp Input) Result { return lblock_mapping(inp, n) }}) }})
}

// [202] L-DOCUMENT-PREFIX 
func l_document_prefix(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return c_byte_order_mark(inp) }) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_comment(inp) }) }})
}

// [203] C-DIRECTIVES-END 
func c_directives_end(inp Input) Result {
    return match_str(inp, "---")
}

// [204] C-DOCUMENT-END 
func c_document_end(inp Input) Result {
    return match_str(inp, "...")
}

// [205] L-DOCUMENT-SUFFIX 
func l_document_suffix(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return c_document_end(inp) },
        func(inp Input) Result { return s_l_comments(inp) }})
}

// [206] C-FORBIDDEN 
func c_forbidden(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return sol(inp) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return c_directives_end(inp) },
            func(inp Input) Result { return c_document_end(inp) }}) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return b_char(inp) },
            func(inp Input) Result { return s_white(inp) },
            func(inp Input) Result { return eof_ok(inp) }}) }})
}

// [207] L-BARE-DOCUMENT 
func l_bare_document(inp Input) Result {
    return build(inp, "DOC", func(inp Input) Result { return s_lblock_node(inp, -1, "BLOCK-IN") })
}

// [208] L-EXPLICIT-DOCUMENT 
func l_explicit_document(inp Input) Result {
    return build(inp, "DOC", func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return c_directives_end(inp) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return l_bare_document(inp) },
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return e_node(inp) },
                func(inp Input) Result { return s_l_comments(inp) }}) }}) }}) })
}

// [209] L-DIRECTIVE-DOCUMENT 
func l_directive_document(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return l_directive(inp) }) },
        func(inp Input) Result { return l_explicit_document(inp) }})
}

// [210] L-ANY-DOCUMENT 
func l_any_document(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return l_directive_document(inp) },
        func(inp Input) Result { return l_explicit_document(inp) },
        func(inp Input) Result { return l_bare_document(inp) }})
}

// [211] L-YAML-STREAM 
func l_yaml_stream(inp Input) Result {
    return build(inp, "STREAM", func(inp Input) Result { return seq(inp, []PFn{
        func(inp Input) Result { return star(inp, func(inp Input) Result { return l_document_prefix(inp) }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return l_any_document(inp) }) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return plus_(inp, func(inp Input) Result { return l_document_suffix(inp) }) },
                func(inp Input) Result { return star(inp, func(inp Input) Result { return l_document_prefix(inp) }) },
                func(inp Input) Result { return opt(inp, func(inp Input) Result { return l_any_document(inp) }) }}) },
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return star(inp, func(inp Input) Result { return l_document_prefix(inp) }) },
                func(inp Input) Result { return opt(inp, func(inp Input) Result { return l_explicit_document(inp) }) }}) }}) }) }}) })
}

// ── API ──

func printAst(node *Ast, depth int) {
	indent := strings.Repeat("  ", depth)
	if node.isLeaf {
		fmt.Printf("%sSCALAR: \"%s\"\n", indent, node.text)
	} else {
		fmt.Printf("%s%s\n", indent, node.tag)
		for _, c := range node.children { printAst(c, depth+1) }
	}
}

// ── Native Value Type ──

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
func (y *YamlValue) Str() string { if y.Tag == YStr { return y.S }; return "" }
func (y *YamlValue) Size() int {
    if y.Tag == YMap { return len(y.M) }; if y.Tag == YSeq { return len(y.V) }; return 0
}

// ── Schema Coercion ──

func coerceScalar(s string) *YamlValue {
    switch s {
    case "null", "Null", "NULL", "~", "":
        return NullVal()
    case "true", "True", "TRUE":
        return BoolVal(true)
    case "false", "False", "FALSE":
        return BoolVal(false)
    case ".inf", ".Inf", ".INF", "+.inf":
        return FloatVal(math.Inf(1))
    case "-.inf", "-.Inf", "-.INF":
        return FloatVal(math.Inf(-1))
    case ".nan", ".NaN", ".NAN":
        return FloatVal(math.NaN())
    }
    if i, err := strconv.ParseInt(s, 0, 64); err == nil { return IntVal(i) }
    if f, err := strconv.ParseFloat(s, 64); err == nil { return FloatVal(f) }
    return StrVal(s)
}

// ── AST → Native Conversion with Anchor Resolution ──

type yamlConverter struct { anchors map[string]*YamlValue }

func (c *yamlConverter) convert(node *Ast) *YamlValue {
    if node == nil { return NullVal() }
    if node.isLeaf { return coerceScalar(node.text) }
    switch node.tag {
    case "ANCHOR":
        var name string
        var val *YamlValue = NullVal()
        for _, ch := range node.children {
            if ch.isLeaf && name == "" { name = ch.text } else { val = c.convert(ch) }
        }
        if name != "" { c.anchors[name] = val }
        return val
    case "ALIAS":
        for _, ch := range node.children {
            if ch.isLeaf { if v, ok := c.anchors[ch.text]; ok { return v } }
        }
        return NullVal()
    case "MAPPING":
        m := make(map[string]*YamlValue)
        for _, ch := range node.children {
            if ch.tag == "PAIR" && len(ch.children) >= 2 {
                key := c.convert(ch.children[0])
                val := c.convert(ch.children[1])
                if key.Tag == YStr && key.S == "<<" && val.Tag == YMap {
                    for mk, mv := range val.M { if _, exists := m[mk]; !exists { m[mk] = mv } }
                } else { m[key.Str()] = val }
            }
        }
        return MapVal(m)
    case "SEQUENCE":
        var seq []*YamlValue
        for _, ch := range node.children { seq = append(seq, c.convert(ch)) }
        return SeqVal(seq)
    case "DOC", "STREAM":
        if len(node.children) == 1 { return c.convert(node.children[0]) }
        var docs []*YamlValue
        for _, ch := range node.children { docs = append(docs, c.convert(ch)) }
        if len(docs) == 1 { return docs[0] }
        return SeqVal(docs)
    }
    if len(node.children) == 1 { return c.convert(node.children[0]) }
    var items []*YamlValue
    for _, ch := range node.children { items = append(items, c.convert(ch)) }
    return SeqVal(items)
}

// ── Public API ──

func Load(text string) *YamlValue {
    inp := Input{src: &text, pos: 0, line: 1, col: 0}
    r := l_yaml_stream(inp)
    if r.fail { return NullVal() }
    c := &yamlConverter{anchors: make(map[string]*YamlValue)}
    return c.convert(r.ast)
}

func main() {
	var text string
	if len(os.Args) > 1 {
		b, err := os.ReadFile(os.Args[1])
		if err != nil { fmt.Fprintf(os.Stderr, "Cannot open %s\n", os.Args[1]); os.Exit(1) }
		text = string(b)
	} else {
		b, _ := io.ReadAll(os.Stdin)
		text = string(b)
	}
	inp := newInput(&text)
	r := l_yaml_stream(inp)
	if !r.fail {
		fmt.Printf("OK: %d chars\n", r.rest.pos)
		if r.ast != nil { printAst(r.ast, 0) }
	} else {
		fmt.Fprintf(os.Stderr, "FAIL @%d: %s\n", r.rest.pos, r.err)
		os.Exit(1)
	}
}
