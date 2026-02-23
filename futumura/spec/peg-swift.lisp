;;;; peg-swift.lisp — Swift target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Swift")
(def-tgt "default-output" "yaml_reader.swift")

(def-tgt "keywords"
  '("as" "break" "case" "catch" "class" "continue" "default" "defer" "do"
    "else" "enum" "extension" "fallthrough" "false" "for" "func" "guard"
    "if" "import" "in" "init" "inout" "internal" "is" "let" "nil" "operator"
    "private" "protocol" "public" "repeat" "return" "self" "static" "struct"
    "subscript" "super" "switch" "throw" "throws" "true" "try" "typealias"
    "var" "where" "while"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "{ (inp: Input) -> Result in return ~A }" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "{ (inp: Input) -> Result in return ~A }" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "seq(inp, [~{~A~^, ~}])" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "alt(inp, [~{~A~^, ~}])" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (with-output-to-string (s)
      (format s "{ () -> Result in switch ~A {~%" param)
      (loop for (val body) in cases
            do (format s "        case ~S: return ~A~%" val body))
      (format s "        default: return fail(inp, \"no case\")~%    } }()"))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "bindInt(~A, { ~A, inp in ~A })"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "bindCtx(~A, { ~A, inp in ~A })"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "Int(~A)" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "inFlow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seqSpaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(_ inp: Input~{, _ ~A~})" name
                (mapcar (lambda (p)
                          (let ((pn (symbol-name p)))
                            (if (member pn '("N" "M") :test #'string-equal)
                                (format nil "~A: Int" (peg-ident p))
                                (format nil "~A: String" (peg-ident p)))))
                        params))
        (format nil "~A(_ inp: Input)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "func ~A -> Result {~%    return ~A~%}" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
import Foundation

typealias PFn = (Input) -> Result")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Input ──

struct Input {
    let src: String
    let chars: [Character]
    let pos: Int
    let line: Int
    let col: Int
    init(_ s: String, _ p: Int = 0, _ l: Int = 1, _ c: Int = 0) {
        src = s; chars = Array(s); pos = p; line = l; col = c
    }
    init(chars cs: [Character], src s: String, pos p: Int, line l: Int, col c: Int) {
        self.chars = cs; self.src = s; self.pos = p; self.line = l; self.col = c
    }
    func atEof() -> Bool { return pos >= chars.count }
    func peek() -> Int {
        if atEof() { return -1 }
        return Int(chars[pos].unicodeScalars.first!.value)
    }
    func adv() -> Input {
        if atEof() { return self }
        let c = chars[pos]
        let nl = c == \"\\n\"
        return Input(chars: chars, src: src, pos: pos + 1, line: nl ? line + 1 : line, col: nl ? 0 : col + 1)
    }
}"

"// ── AST ──

class Ast {
    let tag: String
    let text: String
    var children: [Ast]
    let isLeaf: Bool
    init(tag: String) { self.tag = tag; self.text = \"\"; self.children = []; self.isLeaf = false }
    init(text: String) { self.tag = \"\"; self.text = text; self.children = []; self.isLeaf = true }
    static func branch(_ tag: String) -> Ast { return Ast(tag: tag) }
    static func leaf(_ text: String) -> Ast { return Ast(text: text) }
}"

"// ── Result ──

struct Result {
    var fail: Bool = false
    var val_: String = \"\"
    var rest: Input
    var tag: String = \"\"
    var tagInt: Int = 0
    var ast: Ast? = nil
    var astList: [Ast]? = nil
    var err: String = \"\"
}

func ok(_ inp: Input) -> Result { return Result(rest: inp) }
func okV(_ inp: Input, _ v: String) -> Result { return Result(val_: v, rest: inp) }
func fail(_ inp: Input, _ msg: String) -> Result { return Result(fail: true, rest: inp, err: msg) }"

"// ── Context ──

func inFlow(_ c: String) -> String { return (c == \"FLOW-OUT\" || c == \"FLOW-IN\") ? \"FLOW-IN\" : \"FLOW-KEY\" }
func seqSpaces(_ n: Int, _ c: String) -> Int { return c == \"BLOCK-OUT\" ? n - 1 : n }"

"// ── Combinators ──

func match_cp(_ inp: Input, _ cp: Int) -> Result {
    let c = inp.peek()
    if c == cp {
        let s = String(UnicodeScalar(c)!)
        var cur = inp; for _ in s.indices { cur = cur.adv() }
        return okV(cur, s)
    }
    return fail(inp, \"cp\")
}

func match_range(_ inp: Input, _ lo: Int, _ hi: Int) -> Result {
    let c = inp.peek()
    if c >= lo && c <= hi {
        let s = String(UnicodeScalar(c)!)
        var cur = inp; for _ in s.indices { cur = cur.adv() }
        return okV(cur, s)
    }
    return fail(inp, \"rng\")
}

func match_str(_ inp: Input, _ t: String) -> Result {
    let n = t.count
    if inp.pos + n > inp.chars.count { return fail(inp, \"str\") }
    let sub = String(inp.chars[inp.pos..<inp.pos+n])
    if sub != t { return fail(inp, \"str\") }
    var cur = inp; for _ in 0..<n { cur = cur.adv() }
    return okV(cur, t)
}

func mergeAsts(_ dst: inout [Ast], _ r: Result) {
    if let a = r.ast { dst.append(a) }
    if let al = r.astList { dst.append(contentsOf: al) }
}

func seq(_ inp: Input, _ fns: [PFn]) -> Result {
    var cur = inp; var acc = \"\"; var asts: [Ast] = []
    for f in fns { var r = f(cur); if r.fail { return r }; acc += r.val_; mergeAsts(&asts, r); cur = r.rest }
    var res = okV(cur, acc)
    if asts.count == 1 { res.ast = asts[0] } else if asts.count > 1 { res.astList = asts }
    return res
}

func alt(_ inp: Input, _ fns: [PFn]) -> Result {
    for f in fns { let r = f(inp); if !r.fail { return r } }
    return fail(inp, \"alt\")
}

func star(_ inp: Input, _ f: PFn) -> Result {
    var cur = inp; var acc = \"\"; var asts: [Ast] = []
    while true { let r = f(cur); if r.fail || r.rest.pos <= cur.pos { break }; acc += r.val_; mergeAsts(&asts, r); cur = r.rest }
    var res = okV(cur, acc)
    if !asts.isEmpty { res.astList = asts }
    return res
}

func plus_(_ inp: Input, _ f: PFn) -> Result {
    let first = f(inp); if first.fail { return first }
    let rest = star(first.rest, f)
    var res = okV(rest.rest, first.val_ + rest.val_)
    var asts: [Ast] = []; mergeAsts(&asts, first); mergeAsts(&asts, rest)
    if !asts.isEmpty { res.astList = asts }
    return res
}

func opt(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return ok(inp) }; return r }
func neg(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return ok(inp) }; return fail(inp, \"neg\") }
func minus(_ inp: Input, _ fa: PFn, _ fb: PFn) -> Result {
    let ra = fa(inp); if ra.fail { return ra }
    let rb = fb(inp); if !rb.fail && rb.rest.pos == ra.rest.pos { return fail(inp, \"excl\") }; return ra
}
func rep(_ inp: Input, _ n: Int, _ f: PFn) -> Result {
    var cur = inp; var acc = \"\"
    for _ in 0..<n { let r = f(cur); if r.fail { return r }; acc += r.val_; cur = r.rest }
    return okV(cur, acc)
}
func ahead(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return r }; return ok(inp) }
func behind(_ inp: Input, _ f: PFn) -> Result {
    if inp.pos == 0 { return fail(inp, \"bh\") }
    let t = Input(chars: inp.chars, src: inp.src, pos: inp.pos - 1, line: inp.line, col: max(0, inp.col - 1))
    let r = f(t); if r.fail { return fail(inp, \"bh\") }; return ok(inp)
}
func sol(_ inp: Input) -> Result { if inp.col == 0 { return ok(inp) }; return fail(inp, \"sol\") }
func eof_ok(_ inp: Input) -> Result { if inp.atEof() { return ok(inp) }; return fail(inp, \"eof\") }"

"// ── YAML extensions ──

func bindInt(_ r: Result, _ k: (Int, Input) -> Result) -> Result {
    if r.fail { return r }
    return k(r.tagInt, r.rest)
}

func bindCtx(_ r: Result, _ k: (String, Input) -> Result) -> Result {
    if r.fail { return r }
    return k(r.tag, r.rest)
}

func build(_ inp: Input, _ typ: String, _ f: PFn) -> Result {
    var r = f(inp); if r.fail { return r }
    let node = Ast.branch(typ)
    if let a = r.ast { node.children.append(a) }
    if let al = r.astList { node.children.append(contentsOf: al) }
    r.ast = node; r.astList = nil; return r
}

func scalar(_ inp: Input, _ f: PFn) -> Result {
    var r = f(inp); if r.fail { return r }
    r.ast = Ast.leaf(r.val_); return r
}

func collect(_ inp: Input, _ f: PFn) -> Result { return f(inp) }

func detect_indent(_ inp: Input, _ n: Int) -> Result {
    let s = inp.chars; let l = s.count; let i = inp.pos
    var sp = 0; while i + sp < l && s[i + sp] == \" \" { sp += 1 }
    if i + sp < l && s[i + sp] != \"\\n\" { var r = ok(inp); r.tagInt = max(1, sp - n); return r }
    var j = i; while j < l && s[j] != \"\\n\" { j += 1 }
    while j < l {
        if s[j] == \"\\n\" { j += 1 }; if j >= l { break }
        sp = 0; while j + sp < l && s[j + sp] == \" \" { sp += 1 }
        let nx = j + sp; if nx >= l || s[nx] == \"\\n\" { j = nx; continue }
        var r = ok(inp); r.tagInt = max(1, sp - n); return r
    }
    var r = ok(inp); r.tagInt = 1; return r
}

func parse_int(_ inp: Input, _ f: PFn) -> Result {
    var r = f(inp); if r.fail { return r }
    var v = 0; for ch in r.val_ { if ch >= \"0\" && ch <= \"9\" { v = v * 10 + Int(ch.asciiValue! - 48) } }
    r.tagInt = v; return r
}

func parse_sym(_ inp: Input, _ f: PFn, _ sym: String) -> Result {
    var r = f(inp); if r.fail { return r }; r.tag = sym; return r
}

func val(_ inp: Input, _ v: String) -> Result { var r = ok(inp); r.tag = v; return r }"
))

;;; ── API ──

(def-tgt "api"
"// ── API ──

func printAst(_ node: Ast, _ depth: Int) {
    let indent = String(repeating: \"  \", count: depth)
    if node.isLeaf {
        print(\"\\(indent)SCALAR: \\\"\\(node.text)\\\"\")
    } else {
        print(\"\\(indent)\\(node.tag)\")
        for c in node.children { printAst(c, depth + 1) }
    }
}")

;;; ── Main ──

(def-tgt "main-fn"
"// ── Main ──

func runMain() {
    let text: String
    if CommandLine.arguments.count > 1 {
        text = try! String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    } else {
        text = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? \"\"
    }
    let inp = Input(text)
    let r = l_yaml_stream(inp)
    if !r.fail {
        print(\"OK: \\(r.rest.pos) chars\")
        if let ast = r.ast { printAst(ast, 0) }
    } else {
        FileHandle.standardError.write(\"FAIL @\\(r.rest.pos): \\(r.err)\\n\".data(using: .utf8)!)
        _exit(1)
    }
    _exit(0)
}

// Run with 64MB stack to handle deep PEG recursion
let thread = Thread { runMain() }
thread.stackSize = 64 * 1024 * 1024
thread.start()
dispatchMain()")

(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)
