;;;; peg-go.lisp — Go target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Go")
(def-tgt "default-output" "yaml_reader.go")

(def-tgt "keywords"
  '("break" "case" "chan" "const" "continue" "default" "defer" "else"
    "fallthrough" "for" "func" "go" "goto" "if" "import" "interface"
    "map" "package" "range" "return" "select" "struct" "switch" "type" "var"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

;; Go closures: func(inp Input) Result { return body }
(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "func(inp Input) Result { return ~A }" body)))

;; Go boxed closures (same — Go has GC, no ownership issues)
(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "func(inp Input) Result { return ~A }" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "seq(inp, []PFn{~{~A~^, ~}})" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "alt(inp, []PFn{~{~A~^, ~}})" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "func() Result {~{ if ~A == ~S { return ~A };~} return fail(inp, \"no case\") }()"
            (loop for (val body) in cases
                  collect param collect val collect body))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "func() Result { r := ~A; if r.fail { return r }; ~A := r.tagInt; _ = ~A; inp := r.rest; return ~A }()"
            expr vname vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "func() Result { r := ~A; if r.fail { return r }; ~A := r.tag; _ = ~A; inp := r.rest; return ~A }()"
            expr vname vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "int(~A)" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "inFlow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seqSpaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(inp Input~{, ~A~})" name
                (mapcar (lambda (p)
                          (let ((pn (symbol-name p)))
                            (if (member pn '("N" "M") :test #'string-equal)
                                (format nil "~A int" (peg-ident p))
                                (format nil "~A string" (peg-ident p)))))
                        params))
        (format nil "~A(inp Input)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "func ~A Result {~%    return ~A~%}" sig body)))

(def-tgt "fwd-decl" nil)  ;; Go doesn't need forward declarations

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
// yaml_reader.go — YAML 1.2 parser, projected from yaml-grammar.scm
// ════════════════════════════════════════════════════════════════
// Generated. DO NOT EDIT — regenerate from the grammar.
// ════════════════════════════════════════════════════════════════

package main

import (
	\"fmt\"
	\"io\"
	\"os\"
	\"strings\"
)")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Input ──

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
	nl := c == '\\n'
	line := i.line; col := i.col + 1
	if nl { line++; col = 0 }
	return Input{i.src, i.pos + 1, line, col}
}"

"// ── AST ──

type Ast struct {
	tag      string
	text     string
	children []*Ast
	isLeaf   bool
}

func makeAst(tag string) *Ast { return &Ast{tag: tag} }
func leafAst(text string) *Ast { return &Ast{text: text, isLeaf: true} }"

"// ── Result ──

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
func fail(inp Input, msg string) Result { return Result{fail: true, rest: inp, err: msg} }"

"// ── Context ──

func inFlow(c string) string {
	if c == \"FLOW-OUT\" || c == \"FLOW-IN\" { return \"FLOW-IN\" }
	return \"FLOW-KEY\"
}
func seqSpaces(n int, c string) int {
	if c == \"BLOCK-OUT\" { return n - 1 }
	return n
}"

"// ── Combinators ──

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
	return fail(inp, \"cp\")
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
	return fail(inp, \"rng\")
}

func match_range(inp Input, lo, hi int) Result { return matchRange(inp, lo, hi) }

func match_str(inp Input, t string) Result {
	n := len(t)
	if inp.pos+n > len(*inp.src) { return fail(inp, \"str\") }
	if (*inp.src)[inp.pos:inp.pos+n] != t { return fail(inp, \"str\") }
	cur := inp
	for i := 0; i < n; i++ { cur = adv(cur) }
	return okV(cur, t)
}

func mergeAsts(dst *[]*Ast, r Result) {
	if r.ast != nil { *dst = append(*dst, r.ast) }
	if len(r.astList) > 0 { *dst = append(*dst, r.astList...) }
}

func seq(inp Input, fns []PFn) Result {
	cur := inp; acc := \"\"; var asts []*Ast
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
	return fail(inp, \"alt\")
}

func star(inp Input, f PFn) Result {
	cur := inp; acc := \"\"; var asts []*Ast
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
func neg(inp Input, f PFn) Result { r := f(inp); if r.fail { return ok(inp) }; return fail(inp, \"neg\") }
func minus(inp Input, fa, fb PFn) Result {
	ra := fa(inp); if ra.fail { return ra }
	rb := fb(inp); if !rb.fail && rb.rest.pos == ra.rest.pos { return fail(inp, \"excl\") }; return ra
}
func rep(inp Input, n int, f PFn) Result {
	cur := inp; acc := \"\"
	for i := 0; i < n; i++ { r := f(cur); if r.fail { return r }; acc += r.val; cur = r.rest }
	return okV(cur, acc)
}
func ahead(inp Input, f PFn) Result { r := f(inp); if r.fail { return r }; return ok(inp) }
func behind(inp Input, f PFn) Result {
	if inp.pos == 0 { return fail(inp, \"bh\") }
	t := Input{inp.src, inp.pos - 1, inp.line, max(0, inp.col - 1)}
	r := f(t); if r.fail { return fail(inp, \"bh\") }; return ok(inp)
}
func sol(inp Input) Result { if inp.col == 0 { return ok(inp) }; return fail(inp, \"sol\") }
func eof_ok(inp Input) Result { if atEof(inp) { return ok(inp) }; return fail(inp, \"eof\") }

func max(a, b int) int { if a > b { return a }; return b }"

"// ── YAML extensions ──

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
	if i+sp < l && s[i+sp] != '\\n' { r := ok(inp); r.tagInt = max(1, sp-n); return r }
	j := i; for j < l && s[j] != '\\n' { j++ }
	for j < l {
		if s[j] == '\\n' { j++ }; if j >= l { break }
		sp = 0; for j+sp < l && s[j+sp] == ' ' { sp++ }
		nx := j + sp; if nx >= l || s[nx] == '\\n' { j = nx; continue }
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

func val(inp Input, v string) Result { r := ok(inp); r.tag = v; return r }"
))

;;; ── API ──

(def-tgt "api"
"// ── API ──

func printAst(node *Ast, depth int) {
	indent := strings.Repeat(\"  \", depth)
	if node.isLeaf {
		fmt.Printf(\"%sSCALAR: \\\"%s\\\"\\n\", indent, node.text)
	} else {
		fmt.Printf(\"%s%s\\n\", indent, node.tag)
		for _, c := range node.children { printAst(c, depth+1) }
	}
}")

;;; ── Main ──

(def-tgt "main-fn"
"func main() {
	var text string
	if len(os.Args) > 1 {
		b, err := os.ReadFile(os.Args[1])
		if err != nil { fmt.Fprintf(os.Stderr, \"Cannot open %s\\n\", os.Args[1]); os.Exit(1) }
		text = string(b)
	} else {
		b, _ := io.ReadAll(os.Stdin)
		text = string(b)
	}
	inp := newInput(&text)
	r := l_yaml_stream(inp)
	if !r.fail {
		fmt.Printf(\"OK: %d chars\\n\", r.rest.pos)
		if r.ast != nil { printAst(r.ast, 0) }
	} else {
		fmt.Fprintf(os.Stderr, \"FAIL @%d: %s\\n\", r.rest.pos, r.err)
		os.Exit(1)
	}
}")
