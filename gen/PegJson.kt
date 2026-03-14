// ════════════════════════════════════════════════════════════════
import java.io.File

typealias PFn = (Input) -> Result

// ── Input ──

data class Input(val src: String, val pos: Int = 0, val line: Int = 1, val col: Int = 0) {
    fun atEof(): Boolean = pos >= src.length
    fun peek(): Int = if (atEof()) -1 else src.codePointAt(pos)
    fun adv(): Input {
        if (atEof()) return this
        val c = src[pos]
        return Input(src, pos + 1, if (c == '\n') line + 1 else line, if (c == '\n') 0 else col + 1)
    }
}

// ── AST ──

data class Ast(
    val tag: String = "",
    val text: String = "",
    val children: MutableList<Ast> = mutableListOf(),
    val isLeaf: Boolean = false
) {
    companion object {
        fun branch(tag: String) = Ast(tag = tag)
        fun leaf(text: String) = Ast(text = text, isLeaf = true)
    }
}

// ── Result ──

data class Result(
    val fail: Boolean = false,
    val rest: Input,
    val err: String = "",
    var tag: String = "",
    var tagInt: Int = 0,
    var ast: Ast? = null,
    var astList: MutableList<Ast>? = null,
    val r_val: String = ""
)

fun ok(inp: Input) = Result(rest = inp)
fun okV(inp: Input, v: String) = Result(rest = inp, r_val = v)
fun fail(inp: Input, msg: String) = Result(fail = true, rest = inp, err = msg)

// ── Context ──

fun inFlow(c: String): String = if (c == "FLOW-OUT" || c == "FLOW-IN") "FLOW-IN" else "FLOW-KEY"
fun seqSpaces(n: Int, c: String): Int = if (c == "BLOCK-OUT") n - 1 else n

// ── Combinators ──

fun match_cp(inp: Input, cp: Int): Result {
    val c = inp.peek()
    if (c == cp) {
        val s = String(Character.toChars(c))
        var cur = inp; for (i in s.indices) cur = cur.adv()
        return okV(cur, s)
    }
    return fail(inp, "cp")
}

fun match_range(inp: Input, lo: Int, hi: Int): Result {
    val c = inp.peek()
    if (c in lo..hi) {
        val s = String(Character.toChars(c))
        var cur = inp; for (i in s.indices) cur = cur.adv()
        return okV(cur, s)
    }
    return fail(inp, "rng")
}

fun match_str(inp: Input, t: String): Result {
    val n = t.length
    if (inp.pos + n > inp.src.length) return fail(inp, "str")
    if (inp.src.substring(inp.pos, inp.pos + n) != t) return fail(inp, "str")
    var cur = inp; for (i in 0 until n) cur = cur.adv()
    return okV(cur, t)
}

fun mergeAsts(dst: MutableList<Ast>, r: Result) {
    r.ast?.let { dst.add(it) }
    r.astList?.let { dst.addAll(it) }
}

fun seq(inp: Input, fns: List<PFn>): Result {
    var cur = inp; val acc = StringBuilder(); val asts = mutableListOf<Ast>()
    for (f in fns) { val r = f(cur); if (r.fail) return r; acc.append(r.r_val); mergeAsts(asts, r); cur = r.rest }
    val res = okV(cur, acc.toString())
    if (asts.size == 1) res.ast = asts[0] else if (asts.size > 1) res.astList = asts
    return res
}

fun alt(inp: Input, fns: List<PFn>): Result {
    for (f in fns) { val r = f(inp); if (!r.fail) return r }
    return fail(inp, "alt")
}

fun star(inp: Input, f: PFn): Result {
    var cur = inp; val acc = StringBuilder(); val asts = mutableListOf<Ast>()
    while (true) { val r = f(cur); if (r.fail || r.rest.pos <= cur.pos) break; acc.append(r.r_val); mergeAsts(asts, r); cur = r.rest }
    val res = okV(cur, acc.toString())
    if (asts.isNotEmpty()) res.astList = asts
    return res
}

fun plus_(inp: Input, f: PFn): Result {
    val first = f(inp); if (first.fail) return first
    val rest = star(first.rest, f)
    val res = okV(rest.rest, first.r_val + rest.r_val)
    val asts = mutableListOf<Ast>(); mergeAsts(asts, first); mergeAsts(asts, rest)
    if (asts.isNotEmpty()) res.astList = asts
    return res
}

fun opt(inp: Input, f: PFn): Result { val r = f(inp); if (r.fail) return ok(inp); return r }
fun r_neg(inp: Input, f: PFn): Result { val r = f(inp); if (r.fail) return ok(inp); return fail(inp, "neg") }
fun minus(inp: Input, fa: PFn, fb: PFn): Result {
    val ra = fa(inp); if (ra.fail) return ra
    val rb = fb(inp); if (!rb.fail && rb.rest.pos == ra.rest.pos) return fail(inp, "excl"); return ra
}
fun rep(inp: Input, n: Int, f: PFn): Result {
    var cur = inp; val acc = StringBuilder()
    for (i in 0 until n) { val r = f(cur); if (r.fail) return r; acc.append(r.r_val); cur = r.rest }
    return okV(cur, acc.toString())
}
fun ahead(inp: Input, f: PFn): Result { val r = f(inp); if (r.fail) return r; return ok(inp) }
fun behind(inp: Input, f: PFn): Result {
    if (inp.pos == 0) return fail(inp, "bh")
    val t = Input(inp.src, inp.pos - 1, inp.line, maxOf(0, inp.col - 1))
    val r = f(t); if (r.fail) return fail(inp, "bh"); return ok(inp)
}
fun sol(inp: Input): Result { if (inp.col == 0) return ok(inp); return fail(inp, "sol") }
fun eof_ok(inp: Input): Result { if (inp.atEof()) return ok(inp); return fail(inp, "eof") }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
fun json_text(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> ws(inp) },
        { inp: Input -> value(inp) },
        { inp: Input -> ws(inp) },
        { inp: Input -> eof_ok(inp) }))
}

// [2] VALUE 
fun value(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> r_object(inp) },
        { inp: Input -> array(inp) },
        { inp: Input -> r_string(inp) },
        { inp: Input -> number(inp) },
        { inp: Input -> match_str(inp, "true") },
        { inp: Input -> match_str(inp, "false") },
        { inp: Input -> match_str(inp, "null") }))
}

// [3] OBJECT 
fun r_object(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 123) },
            { inp: Input -> ws(inp) },
            { inp: Input -> members(inp) },
            { inp: Input -> ws(inp) },
            { inp: Input -> match_cp(inp, 125) })) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 123) },
            { inp: Input -> ws(inp) },
            { inp: Input -> match_cp(inp, 125) })) }))
}

// [4] MEMBERS 
fun members(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> member(inp) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> ws(inp) },
            { inp: Input -> match_cp(inp, 44) },
            { inp: Input -> ws(inp) },
            { inp: Input -> member(inp) })) }) }))
}

// [5] MEMBER 
fun member(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> ws(inp) },
        { inp: Input -> r_string(inp) },
        { inp: Input -> ws(inp) },
        { inp: Input -> match_cp(inp, 58) },
        { inp: Input -> ws(inp) },
        { inp: Input -> value(inp) },
        { inp: Input -> ws(inp) }))
}

// [6] ARRAY 
fun array(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 91) },
            { inp: Input -> ws(inp) },
            { inp: Input -> elements(inp) },
            { inp: Input -> ws(inp) },
            { inp: Input -> match_cp(inp, 93) })) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 91) },
            { inp: Input -> ws(inp) },
            { inp: Input -> match_cp(inp, 93) })) }))
}

// [7] ELEMENTS 
fun elements(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> value(inp) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> ws(inp) },
            { inp: Input -> match_cp(inp, 44) },
            { inp: Input -> ws(inp) },
            { inp: Input -> value(inp) })) }) }))
}

// [8] STRING 
fun r_string(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 34) },
        { inp: Input -> star(inp, { inp: Input -> r_char(inp) }) },
        { inp: Input -> match_cp(inp, 34) }))
}

// [9] CHAR 
fun r_char(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> escaped(inp) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> r_neg(inp, { inp: Input -> match_cp(inp, 34) }) },
            { inp: Input -> r_neg(inp, { inp: Input -> match_cp(inp, 92) }) },
            { inp: Input -> r_neg(inp, { inp: Input -> match_cp(inp, 0x0) }) },
            { inp: Input -> r_neg(inp, { inp: Input -> match_range(inp, 0x0, 0x1F) }) },
            { inp: Input -> match_range(inp, 0x20, 0x10FFFF) })) }))
}

// [10] ESCAPED 
fun escaped(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 92) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> match_cp(inp, 34) },
            { inp: Input -> match_cp(inp, 92) },
            { inp: Input -> match_cp(inp, 47) },
            { inp: Input -> match_cp(inp, 98) },
            { inp: Input -> match_cp(inp, 102) },
            { inp: Input -> match_cp(inp, 110) },
            { inp: Input -> match_cp(inp, 114) },
            { inp: Input -> match_cp(inp, 116) },
            { inp: Input -> seq(inp, listOf({ inp: Input -> match_cp(inp, 117) }, { inp: Input -> hex4(inp) })) })) }))
}

// [11] HEX4 
fun hex4(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> hexdig(inp) },
        { inp: Input -> hexdig(inp) },
        { inp: Input -> hexdig(inp) },
        { inp: Input -> hexdig(inp) }))
}

// [12] HEXDIG 
fun hexdig(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> match_range(inp, 48, 57) },
        { inp: Input -> match_range(inp, 97, 102) },
        { inp: Input -> match_range(inp, 65, 70) }))
}

// [13] NUMBER 
fun number(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> match_cp(inp, 45) }) },
        { inp: Input -> integer(inp) },
        { inp: Input -> opt(inp, { inp: Input -> fraction(inp) }) },
        { inp: Input -> opt(inp, { inp: Input -> exponent(inp) }) }))
}

// [14] INTEGER 
fun integer(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> match_cp(inp, 48) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_range(inp, 49, 57) },
            { inp: Input -> star(inp, { inp: Input -> match_range(inp, 48, 57) }) })) }))
}

// [15] FRACTION 
fun fraction(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 46) },
        { inp: Input -> plus_(inp, { inp: Input -> match_range(inp, 48, 57) }) }))
}

// [16] EXPONENT 
fun exponent(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> alt(inp, listOf({ inp: Input -> match_cp(inp, 101) }, { inp: Input -> match_cp(inp, 69) })) },
        { inp: Input -> opt(inp, { inp: Input -> alt(inp, listOf({ inp: Input -> match_cp(inp, 43) }, { inp: Input -> match_cp(inp, 45) })) }) },
        { inp: Input -> plus_(inp, { inp: Input -> match_range(inp, 48, 57) }) }))
}

// [17] WS 
fun ws(inp: Input): Result {
    return star(inp, { inp: Input -> alt(inp, listOf(
        { inp: Input -> match_cp(inp, 0x20) },
        { inp: Input -> match_cp(inp, 0x9) },
        { inp: Input -> match_cp(inp, 0x0A) },
        { inp: Input -> match_cp(inp, 0x0D) })) })
}

// ── API ──

fun printAst(node: Ast, depth: Int) {
    val indent = "  ".repeat(depth)
    if (node.isLeaf) {
        println("${indent}SCALAR: \"${node.text}\"")
    } else {
        println("$indent${node.tag}")
        for (c in node.children) printAst(c, depth + 1)
    }
}

fun main(args: Array<String>) {
    val text = if (args.isNotEmpty()) {
        File(args[0]).readText()
    } else {
        System.`in`.bufferedReader().readText()
    }
    val inp = Input(text)
    val r = json_text(inp)
    if (!r.fail) {
        println("OK: ${r.rest.pos} chars")
        r.ast?.let { printAst(it, 0) }
    } else {
        System.err.println("FAIL @${r.rest.pos}: ${r.err}")
        kotlin.system.exitProcess(1)
    }
}
