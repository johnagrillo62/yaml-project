// ════════════════════════════════════════════════════════════════
import Foundation

typealias PFn = (Input) -> Result

// ── Input ──

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
        let nl = c == "\n"
        return Input(chars: chars, src: src, pos: pos + 1, line: nl ? line + 1 : line, col: nl ? 0 : col + 1)
    }
}

// ── AST ──

class Ast {
    let tag: String
    let text: String
    var children: [Ast]
    let isLeaf: Bool
    init(tag: String) { self.tag = tag; self.text = ""; self.children = []; self.isLeaf = false }
    init(text: String) { self.tag = ""; self.text = text; self.children = []; self.isLeaf = true }
    static func branch(_ tag: String) -> Ast { return Ast(tag: tag) }
    static func leaf(_ text: String) -> Ast { return Ast(text: text) }
}

// ── Result ──

struct Result {
    var fail: Bool = false
    var val_: String = ""
    var rest: Input
    var tag: String = ""
    var tagInt: Int = 0
    var ast: Ast? = nil
    var astList: [Ast]? = nil
    var err: String = ""
}

func ok(_ inp: Input) -> Result { return Result(rest: inp) }
func okV(_ inp: Input, _ v: String) -> Result { return Result(val_: v, rest: inp) }
func fail(_ inp: Input, _ msg: String) -> Result { return Result(fail: true, rest: inp, err: msg) }

// ── Context ──

func inFlow(_ c: String) -> String { return (c == "FLOW-OUT" || c == "FLOW-IN") ? "FLOW-IN" : "FLOW-KEY" }
func seqSpaces(_ n: Int, _ c: String) -> Int { return c == "BLOCK-OUT" ? n - 1 : n }

// ── Combinators ──

func match_cp(_ inp: Input, _ cp: Int) -> Result {
    let c = inp.peek()
    if c == cp {
        let s = String(UnicodeScalar(c)!)
        var cur = inp; for _ in s.indices { cur = cur.adv() }
        return okV(cur, s)
    }
    return fail(inp, "cp")
}

func match_range(_ inp: Input, _ lo: Int, _ hi: Int) -> Result {
    let c = inp.peek()
    if c >= lo && c <= hi {
        let s = String(UnicodeScalar(c)!)
        var cur = inp; for _ in s.indices { cur = cur.adv() }
        return okV(cur, s)
    }
    return fail(inp, "rng")
}

func match_str(_ inp: Input, _ t: String) -> Result {
    let n = t.count
    if inp.pos + n > inp.chars.count { return fail(inp, "str") }
    let sub = String(inp.chars[inp.pos..<inp.pos+n])
    if sub != t { return fail(inp, "str") }
    var cur = inp; for _ in 0..<n { cur = cur.adv() }
    return okV(cur, t)
}

func mergeAsts(_ dst: inout [Ast], _ r: Result) {
    if let a = r.ast { dst.append(a) }
    if let al = r.astList { dst.append(contentsOf: al) }
}

func seq(_ inp: Input, _ fns: [PFn]) -> Result {
    var cur = inp; var acc = ""; var asts: [Ast] = []
    for f in fns { var r = f(cur); if r.fail { return r }; acc += r.val_; mergeAsts(&asts, r); cur = r.rest }
    var res = okV(cur, acc)
    if asts.count == 1 { res.ast = asts[0] } else if asts.count > 1 { res.astList = asts }
    return res
}

func alt(_ inp: Input, _ fns: [PFn]) -> Result {
    for f in fns { let r = f(inp); if !r.fail { return r } }
    return fail(inp, "alt")
}

func star(_ inp: Input, _ f: PFn) -> Result {
    var cur = inp; var acc = ""; var asts: [Ast] = []
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
func neg(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return ok(inp) }; return fail(inp, "neg") }
func minus(_ inp: Input, _ fa: PFn, _ fb: PFn) -> Result {
    let ra = fa(inp); if ra.fail { return ra }
    let rb = fb(inp); if !rb.fail && rb.rest.pos == ra.rest.pos { return fail(inp, "excl") }; return ra
}
func rep(_ inp: Input, _ n: Int, _ f: PFn) -> Result {
    var cur = inp; var acc = ""
    for _ in 0..<n { let r = f(cur); if r.fail { return r }; acc += r.val_; cur = r.rest }
    return okV(cur, acc)
}
func ahead(_ inp: Input, _ f: PFn) -> Result { let r = f(inp); if r.fail { return r }; return ok(inp) }
func behind(_ inp: Input, _ f: PFn) -> Result {
    if inp.pos == 0 { return fail(inp, "bh") }
    let t = Input(chars: inp.chars, src: inp.src, pos: inp.pos - 1, line: inp.line, col: max(0, inp.col - 1))
    let r = f(t); if r.fail { return fail(inp, "bh") }; return ok(inp)
}
func sol(_ inp: Input) -> Result { if inp.col == 0 { return ok(inp) }; return fail(inp, "sol") }
func eof_ok(_ inp: Input) -> Result { if inp.atEof() { return ok(inp) }; return fail(inp, "eof") }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
func json_text(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ws(inp) },
        { (inp: Input) -> Result in return value(inp) },
        { (inp: Input) -> Result in return ws(inp) },
        { (inp: Input) -> Result in return eof_ok(inp) }])
}

// [2] VALUE 
func value(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return object(inp) },
        { (inp: Input) -> Result in return array(inp) },
        { (inp: Input) -> Result in return r_string(inp) },
        { (inp: Input) -> Result in return number(inp) },
        { (inp: Input) -> Result in return match_str(inp, "true") },
        { (inp: Input) -> Result in return match_str(inp, "false") },
        { (inp: Input) -> Result in return match_str(inp, "null") }])
}

// [3] OBJECT 
func object(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 123) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return members(inp) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return match_cp(inp, 125) }]) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 123) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return match_cp(inp, 125) }]) }])
}

// [4] MEMBERS 
func members(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return member(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return match_cp(inp, 44) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return member(inp) }]) }) }])
}

// [5] MEMBER 
func member(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return ws(inp) },
        { (inp: Input) -> Result in return r_string(inp) },
        { (inp: Input) -> Result in return ws(inp) },
        { (inp: Input) -> Result in return match_cp(inp, 58) },
        { (inp: Input) -> Result in return ws(inp) },
        { (inp: Input) -> Result in return value(inp) },
        { (inp: Input) -> Result in return ws(inp) }])
}

// [6] ARRAY 
func array(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 91) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return elements(inp) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return match_cp(inp, 93) }]) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 91) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return match_cp(inp, 93) }]) }])
}

// [7] ELEMENTS 
func elements(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return value(inp) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return match_cp(inp, 44) },
            { (inp: Input) -> Result in return ws(inp) },
            { (inp: Input) -> Result in return value(inp) }]) }) }])
}

// [8] STRING 
func r_string(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 34) },
        { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return r_char(inp) }) },
        { (inp: Input) -> Result in return match_cp(inp, 34) }])
}

// [9] CHAR 
func r_char(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return escaped(inp) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return neg(inp, { (inp: Input) -> Result in return match_cp(inp, 34) }) },
            { (inp: Input) -> Result in return neg(inp, { (inp: Input) -> Result in return match_cp(inp, 92) }) },
            { (inp: Input) -> Result in return neg(inp, { (inp: Input) -> Result in return match_cp(inp, 0x0) }) },
            { (inp: Input) -> Result in return neg(inp, { (inp: Input) -> Result in return match_range(inp, 0x0, 0x1F) }) },
            { (inp: Input) -> Result in return match_range(inp, 0x20, 0x10FFFF) }]) }])
}

// [10] ESCAPED 
func escaped(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 92) },
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 34) },
            { (inp: Input) -> Result in return match_cp(inp, 92) },
            { (inp: Input) -> Result in return match_cp(inp, 47) },
            { (inp: Input) -> Result in return match_cp(inp, 98) },
            { (inp: Input) -> Result in return match_cp(inp, 102) },
            { (inp: Input) -> Result in return match_cp(inp, 110) },
            { (inp: Input) -> Result in return match_cp(inp, 114) },
            { (inp: Input) -> Result in return match_cp(inp, 116) },
            { (inp: Input) -> Result in return seq(inp, [
                { (inp: Input) -> Result in return match_cp(inp, 117) },
                { (inp: Input) -> Result in return hex4(inp) }]) }]) }])
}

// [11] HEX4 
func hex4(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return hexdig(inp) },
        { (inp: Input) -> Result in return hexdig(inp) },
        { (inp: Input) -> Result in return hexdig(inp) },
        { (inp: Input) -> Result in return hexdig(inp) }])
}

// [12] HEXDIG 
func hexdig(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return match_range(inp, 48, 57) },
        { (inp: Input) -> Result in return match_range(inp, 97, 102) },
        { (inp: Input) -> Result in return match_range(inp, 65, 70) }])
}

// [13] NUMBER 
func number(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return match_cp(inp, 45) }) },
        { (inp: Input) -> Result in return integer(inp) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return fraction(inp) }) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return exponent(inp) }) }])
}

// [14] INTEGER 
func integer(_ inp: Input) -> Result {
    return alt(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 48) },
        { (inp: Input) -> Result in return seq(inp, [
            { (inp: Input) -> Result in return match_range(inp, 49, 57) },
            { (inp: Input) -> Result in return star(inp, { (inp: Input) -> Result in return match_range(inp, 48, 57) }) }]) }])
}

// [15] FRACTION 
func fraction(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 46) },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return match_range(inp, 48, 57) }) }])
}

// [16] EXPONENT 
func exponent(_ inp: Input) -> Result {
    return seq(inp, [
        { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 101) },
            { (inp: Input) -> Result in return match_cp(inp, 69) }]) },
        { (inp: Input) -> Result in return opt(inp, { (inp: Input) -> Result in return alt(inp, [
            { (inp: Input) -> Result in return match_cp(inp, 43) },
            { (inp: Input) -> Result in return match_cp(inp, 45) }]) }) },
        { (inp: Input) -> Result in return plus_(inp, { (inp: Input) -> Result in return match_range(inp, 48, 57) }) }])
}

// [17] WS 
func ws(_ inp: Input) -> Result {
    return star(inp, { (inp: Input) -> Result in return alt(inp, [
        { (inp: Input) -> Result in return match_cp(inp, 0x20) },
        { (inp: Input) -> Result in return match_cp(inp, 0x9) },
        { (inp: Input) -> Result in return match_cp(inp, 0x0A) },
        { (inp: Input) -> Result in return match_cp(inp, 0x0D) }]) })
}

// ── API ──

func out(_ s: String) {
    FileHandle.standardOutput.write((s + "\n").data(using: .utf8)!)
}

func printAst(_ node: Ast, _ depth: Int) {
    let indent = String(repeating: "  ", count: depth)
    if node.isLeaf {
        out("\(indent)SCALAR: \"\(node.text)\"")
    } else {
        out("\(indent)\(node.tag)")
        for c in node.children { printAst(c, depth + 1) }
    }
}

// ── Main ──

func runMain() {
    let text: String
    if CommandLine.arguments.count > 1 {
        let path = CommandLine.arguments[1]
        guard let data = FileManager.default.contents(atPath: path),
              let s = String(data: data, encoding: .utf8) else {
            FileHandle.standardError.write("Cannot read: \(path)\n".data(using: .utf8)!)
            _exit(1)
            return
        }
        text = s
    } else {
        text = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
    }
    let inp = Input(text)
    let r = json_text(inp)
    if !r.fail {
        out("OK: \(r.rest.pos) chars")
        if let ast = r.ast { printAst(ast, 0) }
    } else {
        FileHandle.standardError.write("FAIL @\(r.rest.pos): \(r.err)\n".data(using: .utf8)!)
        _exit(1)
    }
    _exit(0)
}

// Run with 64MB stack to handle deep PEG recursion
let thread = Thread { runMain() }
thread.stackSize = 64 * 1024 * 1024
thread.start()
dispatchMain()
