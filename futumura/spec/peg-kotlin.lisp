;;;; peg-kotlin.lisp — Kotlin target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Kotlin")
(def-tgt "default-output" "yaml_reader.kt")

(def-tgt "keywords"
  '("as" "break" "class" "continue" "do" "else" "false" "for" "fun" "if"
    "in" "interface" "is" "null" "object" "package" "return" "super" "this"
    "throw" "true" "try" "typealias" "typeof" "val" "var" "when" "while"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "{ inp: Input -> ~A }" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "{ inp: Input -> ~A }" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "seq(inp, listOf(~{~A~^, ~}))" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "alt(inp, listOf(~{~A~^, ~}))" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (with-output-to-string (s)
      (format s "when (~A) {~%" param)
      (loop for (val body) in cases
            do (format s "        ~S -> ~A~%" val body))
      (format s "        else -> fail(inp, \"no case\")~%    }"))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "run { val r = ~A; if (r.fail) return@run r; val ~A = r.tagInt; val inp = r.rest; ~A }"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "run { val r = ~A; if (r.fail) return@run r; val ~A = r.tag; val inp = r.rest; ~A }"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "~A.toInt()" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "inFlow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seqSpaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(inp: Input~{, ~A~})" name
                (mapcar (lambda (p)
                          (let ((pn (symbol-name p)))
                            (if (member pn '("N" "M") :test #'string-equal)
                                (format nil "~A: Int" (peg-ident p))
                                (format nil "~A: String" (peg-ident p)))))
                        params))
        (format nil "~A(inp: Input)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "fun ~A: Result {~%    return ~A~%}" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"// ════════════════════════════════════════════════════════════════
import java.io.File

typealias PFn = (Input) -> Result")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Input ──

data class Input(val src: String, val pos: Int = 0, val line: Int = 1, val col: Int = 0) {
    fun atEof(): Boolean = pos >= src.length
    fun peek(): Int = if (atEof()) -1 else src.codePointAt(pos)
    fun adv(): Input {
        if (atEof()) return this
        val c = src[pos]
        return Input(src, pos + 1, if (c == '\\n') line + 1 else line, if (c == '\\n') 0 else col + 1)
    }
}"

"// ── AST ──

data class Ast(
    val tag: String = \"\",
    val text: String = \"\",
    val children: MutableList<Ast> = mutableListOf(),
    val isLeaf: Boolean = false
) {
    companion object {
        fun branch(tag: String) = Ast(tag = tag)
        fun leaf(text: String) = Ast(text = text, isLeaf = true)
    }
}"

"// ── Result ──

data class Result(
    val fail: Boolean = false,
    val rest: Input,
    val err: String = \"\",
    var tag: String = \"\",
    var tagInt: Int = 0,
    var ast: Ast? = null,
    var astList: MutableList<Ast>? = null,
    val r_val: String = \"\"
)

fun ok(inp: Input) = Result(rest = inp)
fun okV(inp: Input, v: String) = Result(rest = inp, r_val = v)
fun fail(inp: Input, msg: String) = Result(fail = true, rest = inp, err = msg)"

"// ── Context ──

fun inFlow(c: String): String = if (c == \"FLOW-OUT\" || c == \"FLOW-IN\") \"FLOW-IN\" else \"FLOW-KEY\"
fun seqSpaces(n: Int, c: String): Int = if (c == \"BLOCK-OUT\") n - 1 else n"

"// ── Combinators ──

fun match_cp(inp: Input, cp: Int): Result {
    val c = inp.peek()
    if (c == cp) {
        val s = String(Character.toChars(c))
        var cur = inp; for (i in s.indices) cur = cur.adv()
        return okV(cur, s)
    }
    return fail(inp, \"cp\")
}

fun match_range(inp: Input, lo: Int, hi: Int): Result {
    val c = inp.peek()
    if (c in lo..hi) {
        val s = String(Character.toChars(c))
        var cur = inp; for (i in s.indices) cur = cur.adv()
        return okV(cur, s)
    }
    return fail(inp, \"rng\")
}

fun match_str(inp: Input, t: String): Result {
    val n = t.length
    if (inp.pos + n > inp.src.length) return fail(inp, \"str\")
    if (inp.src.substring(inp.pos, inp.pos + n) != t) return fail(inp, \"str\")
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
    return fail(inp, \"alt\")
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
fun r_neg(inp: Input, f: PFn): Result { val r = f(inp); if (r.fail) return ok(inp); return fail(inp, \"neg\") }
fun minus(inp: Input, fa: PFn, fb: PFn): Result {
    val ra = fa(inp); if (ra.fail) return ra
    val rb = fb(inp); if (!rb.fail && rb.rest.pos == ra.rest.pos) return fail(inp, \"excl\"); return ra
}
fun rep(inp: Input, n: Int, f: PFn): Result {
    var cur = inp; val acc = StringBuilder()
    for (i in 0 until n) { val r = f(cur); if (r.fail) return r; acc.append(r.r_val); cur = r.rest }
    return okV(cur, acc.toString())
}
fun ahead(inp: Input, f: PFn): Result { val r = f(inp); if (r.fail) return r; return ok(inp) }
fun behind(inp: Input, f: PFn): Result {
    if (inp.pos == 0) return fail(inp, \"bh\")
    val t = Input(inp.src, inp.pos - 1, inp.line, maxOf(0, inp.col - 1))
    val r = f(t); if (r.fail) return fail(inp, \"bh\"); return ok(inp)
}
fun sol(inp: Input): Result { if (inp.col == 0) return ok(inp); return fail(inp, \"sol\") }
fun eof_ok(inp: Input): Result { if (inp.atEof()) return ok(inp); return fail(inp, \"eof\") }"

"// ── YAML extensions ──

fun build(inp: Input, typ: String, f: PFn): Result {
    val r = f(inp); if (r.fail) return r
    val node = Ast.branch(typ)
    r.ast?.let { node.children.add(it) }
    r.astList?.let { node.children.addAll(it) }
    r.ast = node; r.astList = null; return r
}

fun scalar(inp: Input, f: PFn): Result {
    val r = f(inp); if (r.fail) return r
    r.ast = Ast.leaf(r.r_val); return r
}

fun collect(inp: Input, f: PFn): Result = f(inp)

fun detect_indent(inp: Input, n: Int): Result {
    val s = inp.src; val l = s.length; var i = inp.pos
    var sp = 0; while (i + sp < l && s[i + sp] == ' ') sp++
    if (i + sp < l && s[i + sp] != '\\n') { val r = ok(inp); r.tagInt = maxOf(1, sp - n); return r }
    var j = i; while (j < l && s[j] != '\\n') j++
    while (j < l) {
        if (s[j] == '\\n') j++; if (j >= l) break
        sp = 0; while (j + sp < l && s[j + sp] == ' ') sp++
        val nx = j + sp; if (nx >= l || s[nx] == '\\n') { j = nx; continue }
        val r = ok(inp); r.tagInt = maxOf(1, sp - n); return r
    }
    val r = ok(inp); r.tagInt = 1; return r
}

fun parse_int(inp: Input, f: PFn): Result {
    val r = f(inp); if (r.fail) return r
    var v = 0; for (ch in r.r_val) { if (ch in '0'..'9') v = v * 10 + (ch - '0') }
    r.tagInt = v; return r
}

fun parse_sym(inp: Input, f: PFn, sym: String): Result {
    val r = f(inp); if (r.fail) return r; r.tag = sym; return r
}

fun r_val(inp: Input, v: String): Result { val r = ok(inp); r.tag = v; return r }"
))

;;; ── API ──

(def-tgt "api"
"// ── API ──

fun printAst(node: Ast, depth: Int) {
    val indent = \"  \".repeat(depth)
    if (node.isLeaf) {
        println(\"${indent}SCALAR: \\\"${node.text}\\\"\")
    } else {
        println(\"$indent${node.tag}\")
        for (c in node.children) printAst(c, depth + 1)
    }
}")

;;; ── Main ──

(def-tgt "main-fn"
"fun main(args: Array<String>) {
    val text = if (args.isNotEmpty()) {
        File(args[0]).readText()
    } else {
        System.`in`.bufferedReader().readText()
    }
    val inp = Input(text)
    val r = l_yaml_stream(inp)
    if (!r.fail) {
        println(\"OK: ${r.rest.pos} chars\")
        r.ast?.let { printAst(it, 0) }
    } else {
        System.err.println(\"FAIL @${r.rest.pos}: ${r.err}\")
        kotlin.system.exitProcess(1)
    }
}")

(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)

;;; ── Combinator name overrides ──

(def-tgt "comb-neg" "r_neg")
(def-tgt "comb-val" "r_val")
