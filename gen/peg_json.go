// ════════════════════════════════════════════════════════════════
// Generated from the JSON grammar (RFC 8259).
// Do not edit — regenerate from yaml-grammar.scm
// ════════════════════════════════════════════════════════════════
package main

import (
	"fmt"
	"os"
	"io"
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

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
func json_text(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ws(inp) },
        func(inp Input) Result { return value(inp) },
        func(inp Input) Result { return ws(inp) },
        func(inp Input) Result { return eof_ok(inp) }})
}

// [2] VALUE 
func value(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return object(inp) },
        func(inp Input) Result { return array(inp) },
        func(inp Input) Result { return r_string(inp) },
        func(inp Input) Result { return number(inp) },
        func(inp Input) Result { return match_str(inp, "true") },
        func(inp Input) Result { return match_str(inp, "false") },
        func(inp Input) Result { return match_str(inp, "null") }})
}

// [3] OBJECT 
func object(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 123) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return members(inp) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return match_cp(inp, 125) }}) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 123) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return match_cp(inp, 125) }}) }})
}

// [4] MEMBERS 
func members(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return member(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return match_cp(inp, 44) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return member(inp) }}) }) }})
}

// [5] MEMBER 
func member(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return ws(inp) },
        func(inp Input) Result { return r_string(inp) },
        func(inp Input) Result { return ws(inp) },
        func(inp Input) Result { return match_cp(inp, 58) },
        func(inp Input) Result { return ws(inp) },
        func(inp Input) Result { return value(inp) },
        func(inp Input) Result { return ws(inp) }})
}

// [6] ARRAY 
func array(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 91) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return elements(inp) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return match_cp(inp, 93) }}) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 91) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return match_cp(inp, 93) }}) }})
}

// [7] ELEMENTS 
func elements(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return value(inp) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return match_cp(inp, 44) },
            func(inp Input) Result { return ws(inp) },
            func(inp Input) Result { return value(inp) }}) }) }})
}

// [8] STRING 
func r_string(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 34) },
        func(inp Input) Result { return star(inp, func(inp Input) Result { return r_char(inp) }) },
        func(inp Input) Result { return match_cp(inp, 34) }})
}

// [9] CHAR 
func r_char(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return escaped(inp) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return neg(inp, func(inp Input) Result { return match_cp(inp, 34) }) },
            func(inp Input) Result { return neg(inp, func(inp Input) Result { return match_cp(inp, 92) }) },
            func(inp Input) Result { return neg(inp, func(inp Input) Result { return match_cp(inp, 0x0) }) },
            func(inp Input) Result { return neg(inp, func(inp Input) Result { return match_range(inp, 0x0, 0x1F) }) },
            func(inp Input) Result { return match_range(inp, 0x20, 0x10FFFF) }}) }})
}

// [10] ESCAPED 
func escaped(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 92) },
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 34) },
            func(inp Input) Result { return match_cp(inp, 92) },
            func(inp Input) Result { return match_cp(inp, 47) },
            func(inp Input) Result { return match_cp(inp, 98) },
            func(inp Input) Result { return match_cp(inp, 102) },
            func(inp Input) Result { return match_cp(inp, 110) },
            func(inp Input) Result { return match_cp(inp, 114) },
            func(inp Input) Result { return match_cp(inp, 116) },
            func(inp Input) Result { return seq(inp, []PFn{
                func(inp Input) Result { return match_cp(inp, 117) },
                func(inp Input) Result { return hex4(inp) }}) }}) }})
}

// [11] HEX4 
func hex4(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return hexdig(inp) },
        func(inp Input) Result { return hexdig(inp) },
        func(inp Input) Result { return hexdig(inp) },
        func(inp Input) Result { return hexdig(inp) }})
}

// [12] HEXDIG 
func hexdig(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return match_range(inp, 48, 57) },
        func(inp Input) Result { return match_range(inp, 97, 102) },
        func(inp Input) Result { return match_range(inp, 65, 70) }})
}

// [13] NUMBER 
func number(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return match_cp(inp, 45) }) },
        func(inp Input) Result { return integer(inp) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return fraction(inp) }) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return exponent(inp) }) }})
}

// [14] INTEGER 
func integer(inp Input) Result {
    return alt(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 48) },
        func(inp Input) Result { return seq(inp, []PFn{
            func(inp Input) Result { return match_range(inp, 49, 57) },
            func(inp Input) Result { return star(inp, func(inp Input) Result { return match_range(inp, 48, 57) }) }}) }})
}

// [15] FRACTION 
func fraction(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 46) },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return match_range(inp, 48, 57) }) }})
}

// [16] EXPONENT 
func exponent(inp Input) Result {
    return seq(inp, []PFn{
        func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 101) },
            func(inp Input) Result { return match_cp(inp, 69) }}) },
        func(inp Input) Result { return opt(inp, func(inp Input) Result { return alt(inp, []PFn{
            func(inp Input) Result { return match_cp(inp, 43) },
            func(inp Input) Result { return match_cp(inp, 45) }}) }) },
        func(inp Input) Result { return plus_(inp, func(inp Input) Result { return match_range(inp, 48, 57) }) }})
}

// [17] WS 
func ws(inp Input) Result {
    return star(inp, func(inp Input) Result { return alt(inp, []PFn{
        func(inp Input) Result { return match_cp(inp, 0x20) },
        func(inp Input) Result { return match_cp(inp, 0x9) },
        func(inp Input) Result { return match_cp(inp, 0x0A) },
        func(inp Input) Result { return match_cp(inp, 0x0D) }}) })
}

// ── API ──

func PrintAst(node *Ast, depth int) {
	indent := strings.Repeat("  ", depth)
	if node.isLeaf {
		fmt.Printf("%sSCALAR: \"%s\"\n", indent, node.text)
	} else {
		fmt.Printf("%s%s\n", indent, node.tag)
		for _, c := range node.children { PrintAst(c, depth+1) }
	}
}

// Parse parses JSON and returns the AST root.
func Parse(text string) (*Ast, error) {
	inp := Input{src: &text, pos: 0, line: 1, col: 0}
	r := json_text(inp)
	if r.fail { return nil, fmt.Errorf("parse failed at position %d: %s", r.rest.pos, r.err) }
	return r.ast, nil
}

func main() {
	var text string
	args := os.Args[1:]
	if len(args) > 0 {
		b, err := os.ReadFile(args[0])
		if err != nil { fmt.Fprintf(os.Stderr, "Cannot open %s: %v\n", args[0], err); os.Exit(1) }
		text = string(b)
	} else {
		b, err := io.ReadAll(os.Stdin)
		if err != nil { fmt.Fprintf(os.Stderr, "Read error: %v\n", err); os.Exit(1) }
		text = string(b)
	}
	ast, err := Parse(text)
	if err != nil { fmt.Fprintf(os.Stderr, "FAIL: %v\n", err); os.Exit(1) }
	fmt.Printf("OK: %d chars\n", len(text))
	if ast != nil { PrintAst(ast, 0) }
	os.Exit(0)
}
