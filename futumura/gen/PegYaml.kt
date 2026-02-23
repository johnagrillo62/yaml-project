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

// ── YAML extensions ──

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
    if (i + sp < l && s[i + sp] != '\n') { val r = ok(inp); r.tagInt = maxOf(1, sp - n); return r }
    var j = i; while (j < l && s[j] != '\n') j++
    while (j < l) {
        if (s[j] == '\n') j++; if (j >= l) break
        sp = 0; while (j + sp < l && s[j + sp] == ' ') sp++
        val nx = j + sp; if (nx >= l || s[nx] == '\n') { j = nx; continue }
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

fun r_val(inp: Input, v: String): Result { val r = ok(inp); r.tag = v; return r }

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] C-PRINTABLE 
fun c_printable(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> match_cp(inp, 0x9) },
        { inp: Input -> match_cp(inp, 0x0A) },
        { inp: Input -> match_cp(inp, 0x0D) },
        { inp: Input -> match_range(inp, 0x20, 0x7E) },
        { inp: Input -> match_cp(inp, 0x85) },
        { inp: Input -> match_range(inp, 0xA0, 0xD7FF) },
        { inp: Input -> match_range(inp, 0xE000, 0xFFFD) },
        { inp: Input -> match_range(inp, 0x10000, 0x10FFFF) }))
}

// [2] NB-JSON 
fun nb_json(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> match_cp(inp, 0x9) },
        { inp: Input -> match_range(inp, 0x20, 0x10FFFF) }))
}

// [3] C-BYTE-ORDER-MARK 
fun c_byte_order_mark(inp: Input): Result {
    return match_cp(inp, 0xFEFF)
}

// [4] C-SEQUENCE-ENTRY 
fun c_sequence_entry(inp: Input): Result {
    return match_cp(inp, 45)
}

// [5] C-MAPPING-KEY 
fun c_mapping_key(inp: Input): Result {
    return match_cp(inp, 63)
}

// [6] C-MAPPING-VALUE 
fun c_mapping_value(inp: Input): Result {
    return match_cp(inp, 58)
}

// [7] C-COLLECT-ENTRY 
fun c_collect_entry(inp: Input): Result {
    return match_cp(inp, 44)
}

// [8] C-SEQUENCE-START 
fun c_sequence_start(inp: Input): Result {
    return match_cp(inp, 91)
}

// [9] C-SEQUENCE-END 
fun c_sequence_end(inp: Input): Result {
    return match_cp(inp, 93)
}

// [10] C-MAPPING-START 
fun c_mapping_start(inp: Input): Result {
    return match_cp(inp, 123)
}

// [11] C-MAPPING-END 
fun c_mapping_end(inp: Input): Result {
    return match_cp(inp, 125)
}

// [12] C-COMMENT 
fun c_comment(inp: Input): Result {
    return match_cp(inp, 35)
}

// [13] C-ANCHOR 
fun c_anchor(inp: Input): Result {
    return match_cp(inp, 38)
}

// [14] C-ALIAS 
fun c_alias(inp: Input): Result {
    return match_cp(inp, 42)
}

// [15] C-TAG 
fun c_tag(inp: Input): Result {
    return match_cp(inp, 33)
}

// [16] C-LITERAL 
fun c_literal(inp: Input): Result {
    return match_cp(inp, 124)
}

// [17] C-FOLDED 
fun c_folded(inp: Input): Result {
    return match_cp(inp, 62)
}

// [18] C-SINGLE-QUOTE 
fun c_single_quote(inp: Input): Result {
    return match_cp(inp, 39)
}

// [19] C-DOUBLE-QUOTE 
fun c_double_quote(inp: Input): Result {
    return match_cp(inp, 34)
}

// [20] C-DIRECTIVE 
fun c_directive(inp: Input): Result {
    return match_cp(inp, 37)
}

// [21] C-RESERVED 
fun c_reserved(inp: Input): Result {
    return alt(inp, listOf({ inp: Input -> match_cp(inp, 64) }, { inp: Input -> match_cp(inp, 96) }))
}

// [22] C-INDICATOR 
fun c_indicator(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_sequence_entry(inp) },
        { inp: Input -> c_mapping_key(inp) },
        { inp: Input -> c_mapping_value(inp) },
        { inp: Input -> c_collect_entry(inp) },
        { inp: Input -> c_sequence_start(inp) },
        { inp: Input -> c_sequence_end(inp) },
        { inp: Input -> c_mapping_start(inp) },
        { inp: Input -> c_mapping_end(inp) },
        { inp: Input -> c_comment(inp) },
        { inp: Input -> c_anchor(inp) },
        { inp: Input -> c_alias(inp) },
        { inp: Input -> c_tag(inp) },
        { inp: Input -> c_literal(inp) },
        { inp: Input -> c_folded(inp) },
        { inp: Input -> c_single_quote(inp) },
        { inp: Input -> c_double_quote(inp) },
        { inp: Input -> c_directive(inp) },
        { inp: Input -> c_reserved(inp) }))
}

// [23] C-FLOW-INDICATOR 
fun c_flow_indicator(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_collect_entry(inp) },
        { inp: Input -> c_sequence_start(inp) },
        { inp: Input -> c_sequence_end(inp) },
        { inp: Input -> c_mapping_start(inp) },
        { inp: Input -> c_mapping_end(inp) }))
}

// [24] B-LINE-FEED 
fun b_line_feed(inp: Input): Result {
    return match_cp(inp, 0x0A)
}

// [25] B-CARRIAGE-RETURN 
fun b_carriage_return(inp: Input): Result {
    return match_cp(inp, 0x0D)
}

// [26] B-CHAR 
fun b_char(inp: Input): Result {
    return alt(inp, listOf({ inp: Input -> b_line_feed(inp) }, { inp: Input -> b_carriage_return(inp) }))
}

// [27] NB-CHAR 
fun nb_char(inp: Input): Result {
    return minus(inp, { inp: Input -> c_printable(inp) }, { inp: Input -> alt(inp, listOf({ inp: Input -> b_char(inp) }, { inp: Input -> c_byte_order_mark(inp) })) })
}

// [28] B-BREAK 
fun b_break(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf({ inp: Input -> b_carriage_return(inp) }, { inp: Input -> b_line_feed(inp) })) },
        { inp: Input -> b_carriage_return(inp) },
        { inp: Input -> b_line_feed(inp) }))
}

// [29] B-AS-LINE-FEED 
fun b_as_line_feed(inp: Input): Result {
    return b_break(inp)
}

// [30] B-NON-CONTENT 
fun b_non_content(inp: Input): Result {
    return b_break(inp)
}

// [31] S-SPACE 
fun s_space(inp: Input): Result {
    return match_cp(inp, 0x20)
}

// [32] S-TAB 
fun s_tab(inp: Input): Result {
    return match_cp(inp, 0x9)
}

// [33] S-WHITE 
fun s_white(inp: Input): Result {
    return alt(inp, listOf({ inp: Input -> s_space(inp) }, { inp: Input -> s_tab(inp) }))
}

// [34] NS-CHAR 
fun ns_char(inp: Input): Result {
    return minus(inp, { inp: Input -> nb_char(inp) }, { inp: Input -> s_white(inp) })
}

// [35] NS-DEC-DIGIT 
fun ns_dec_digit(inp: Input): Result {
    return match_range(inp, 0x30, 0x39)
}

// [36] NS-HEX-DIGIT 
fun ns_hex_digit(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> ns_dec_digit(inp) },
        { inp: Input -> match_range(inp, 0x41, 0x46) },
        { inp: Input -> match_range(inp, 0x61, 0x66) }))
}

// [37] NS-ASCII-LETTER 
fun ns_ascii_letter(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> match_range(inp, 0x41, 0x5A) },
        { inp: Input -> match_range(inp, 0x61, 0x7A) }))
}

// [38] NS-WORD-CHAR 
fun ns_word_char(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> ns_dec_digit(inp) },
        { inp: Input -> ns_ascii_letter(inp) },
        { inp: Input -> match_cp(inp, 45) }))
}

// [39] NS-URI-CHAR 
fun ns_uri_char(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 37) },
            { inp: Input -> ns_hex_digit(inp) },
            { inp: Input -> ns_hex_digit(inp) })) },
        { inp: Input -> ns_word_char(inp) },
        { inp: Input -> match_cp(inp, 35) },
        { inp: Input -> match_cp(inp, 59) },
        { inp: Input -> match_cp(inp, 47) },
        { inp: Input -> match_cp(inp, 63) },
        { inp: Input -> match_cp(inp, 58) },
        { inp: Input -> match_cp(inp, 64) },
        { inp: Input -> match_cp(inp, 38) },
        { inp: Input -> match_cp(inp, 61) },
        { inp: Input -> match_cp(inp, 43) },
        { inp: Input -> match_cp(inp, 36) },
        { inp: Input -> match_cp(inp, 44) },
        { inp: Input -> match_cp(inp, 95) },
        { inp: Input -> match_cp(inp, 46) },
        { inp: Input -> match_cp(inp, 33) },
        { inp: Input -> match_cp(inp, 126) },
        { inp: Input -> match_cp(inp, 42) },
        { inp: Input -> match_cp(inp, 39) },
        { inp: Input -> match_cp(inp, 40) },
        { inp: Input -> match_cp(inp, 41) },
        { inp: Input -> match_cp(inp, 91) },
        { inp: Input -> match_cp(inp, 93) }))
}

// [40] NS-TAG-CHAR 
fun ns_tag_char(inp: Input): Result {
    return minus(inp, { inp: Input -> ns_uri_char(inp) }, { inp: Input -> alt(inp, listOf({ inp: Input -> c_tag(inp) }, { inp: Input -> c_flow_indicator(inp) })) })
}

// [41] C-ESCAPE 
fun c_escape(inp: Input): Result {
    return match_cp(inp, 92)
}

// [42] NS-ESC-NULL 
fun ns_esc_null(inp: Input): Result {
    return match_cp(inp, 48)
}

// [43] NS-ESC-BELL 
fun ns_esc_bell(inp: Input): Result {
    return match_cp(inp, 97)
}

// [44] NS-ESC-BACKSPACE 
fun ns_esc_backspace(inp: Input): Result {
    return match_cp(inp, 98)
}

// [45] NS-ESC-HORIZONTAL-TAB 
fun ns_esc_horizontal_tab(inp: Input): Result {
    return match_cp(inp, 116)
}

// [46] NS-ESC-LINE-FEED 
fun ns_esc_line_feed(inp: Input): Result {
    return match_cp(inp, 110)
}

// [47] NS-ESC-VERTICAL-TAB 
fun ns_esc_vertical_tab(inp: Input): Result {
    return match_cp(inp, 118)
}

// [48] NS-ESC-FORM-FEED 
fun ns_esc_form_feed(inp: Input): Result {
    return match_cp(inp, 102)
}

// [49] NS-ESC-CARRIAGE-RETURN 
fun ns_esc_carriage_return(inp: Input): Result {
    return match_cp(inp, 114)
}

// [50] NS-ESC-ESCAPE 
fun ns_esc_escape(inp: Input): Result {
    return match_cp(inp, 101)
}

// [51] NS-ESC-SPACE 
fun ns_esc_space(inp: Input): Result {
    return match_cp(inp, 0x20)
}

// [52] NS-ESC-DOUBLE-QUOTE 
fun ns_esc_double_quote(inp: Input): Result {
    return match_cp(inp, 34)
}

// [53] NS-ESC-SLASH 
fun ns_esc_slash(inp: Input): Result {
    return match_cp(inp, 47)
}

// [54] NS-ESC-BACKSLASH 
fun ns_esc_backslash(inp: Input): Result {
    return match_cp(inp, 92)
}

// [55] NS-ESC-NEXT-LINE 
fun ns_esc_next_line(inp: Input): Result {
    return match_cp(inp, 78)
}

// [56] NS-ESC-NON-BREAKING-SPACE 
fun ns_esc_non_breaking_space(inp: Input): Result {
    return match_cp(inp, 95)
}

// [57] NS-ESC-LINE-SEPARATOR 
fun ns_esc_line_separator(inp: Input): Result {
    return match_cp(inp, 76)
}

// [58] NS-ESC-PARAGRAPH-SEPARATOR 
fun ns_esc_paragraph_separator(inp: Input): Result {
    return match_cp(inp, 80)
}

// [59] NS-ESC-8-BIT 
fun ns_esc_8_bit(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 120) },
        { inp: Input -> rep(inp, 2, { inp: Input -> ns_hex_digit(inp) }) }))
}

// [60] NS-ESC-16-BIT 
fun ns_esc_16_bit(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 117) },
        { inp: Input -> rep(inp, 4, { inp: Input -> ns_hex_digit(inp) }) }))
}

// [61] NS-ESC-32-BIT 
fun ns_esc_32_bit(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 85) },
        { inp: Input -> rep(inp, 8, { inp: Input -> ns_hex_digit(inp) }) }))
}

// [62] C-NS-ESC-CHAR 
fun c_ns_esc_char(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> c_escape(inp) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> ns_esc_null(inp) },
            { inp: Input -> ns_esc_bell(inp) },
            { inp: Input -> ns_esc_backspace(inp) },
            { inp: Input -> ns_esc_horizontal_tab(inp) },
            { inp: Input -> ns_esc_line_feed(inp) },
            { inp: Input -> ns_esc_vertical_tab(inp) },
            { inp: Input -> ns_esc_form_feed(inp) },
            { inp: Input -> ns_esc_carriage_return(inp) },
            { inp: Input -> ns_esc_escape(inp) },
            { inp: Input -> ns_esc_space(inp) },
            { inp: Input -> ns_esc_double_quote(inp) },
            { inp: Input -> ns_esc_slash(inp) },
            { inp: Input -> ns_esc_backslash(inp) },
            { inp: Input -> ns_esc_next_line(inp) },
            { inp: Input -> ns_esc_non_breaking_space(inp) },
            { inp: Input -> ns_esc_line_separator(inp) },
            { inp: Input -> ns_esc_paragraph_separator(inp) },
            { inp: Input -> ns_esc_8_bit(inp) },
            { inp: Input -> ns_esc_16_bit(inp) },
            { inp: Input -> ns_esc_32_bit(inp) })) }))
}

// [63] S-INDENT 
fun s_indent(inp: Input, n: Int): Result {
    return rep(inp, n, { inp: Input -> s_space(inp) })
}

// [64] S-INDENT-LT 
fun s_indent_lt(inp: Input, n: Int): Result {
    return star(inp, { inp: Input -> s_space(inp) })
}

// [65] S-INDENT-LE 
fun s_indent_le(inp: Input, n: Int): Result {
    return star(inp, { inp: Input -> s_space(inp) })
}

// [66] S-SEPARATE-IN-LINE 
fun s_separate_in_line(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> plus_(inp, { inp: Input -> s_white(inp) }) },
        { inp: Input -> ok(inp) }))
}

// [67] S-LINE-PREFIX 
fun s_line_prefix(inp: Input, n: Int, c: String): Result {
    return when (c) {
        "BLOCK-IN" -> s_block_line_prefix(inp, n)
        "BLOCK-OUT" -> s_block_line_prefix(inp, n)
        "FLOW-IN" -> s_flow_line_prefix(inp, n)
        "FLOW-OUT" -> s_flow_line_prefix(inp, n)
        else -> fail(inp, "no case")
    }
}

// [68] S-BLOCK-LINE-PREFIX 
fun s_block_line_prefix(inp: Input, n: Int): Result {
    return s_indent(inp, n)
}

// [69] S-FLOW-LINE-PREFIX 
fun s_flow_line_prefix(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_indent(inp, n) },
        { inp: Input -> opt(inp, { inp: Input -> s_separate_in_line(inp) }) }))
}

// [70] L-EMPTY 
fun l_empty(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> alt(inp, listOf({ inp: Input -> s_line_prefix(inp, n, c) }, { inp: Input -> s_indent_lt(inp, n) })) },
        { inp: Input -> b_as_line_feed(inp) }))
}

// [71] B-L-TRIMMED 
fun b_l_trimmed(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> b_non_content(inp) },
        { inp: Input -> plus_(inp, { inp: Input -> l_empty(inp, n, c) }) }))
}

// [72] B-AS-SPACE 
fun b_as_space(inp: Input): Result {
    return b_break(inp)
}

// [73] B-L-FOLDED 
fun b_l_folded(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf({ inp: Input -> b_l_trimmed(inp, n, c) }, { inp: Input -> b_as_space(inp) }))
}

// [74] S-FLOW-FOLDED 
fun s_flow_folded(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> s_separate_in_line(inp) }) },
        { inp: Input -> b_l_folded(inp, n, "FLOW-IN") },
        { inp: Input -> s_flow_line_prefix(inp, n) }))
}

// [75] C-NB-COMMENT-TEXT 
fun c_nb_comment_text(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> c_comment(inp) },
        { inp: Input -> star(inp, { inp: Input -> nb_char(inp) }) }))
}

// [76] B-COMMENT 
fun b_comment(inp: Input): Result {
    return alt(inp, listOf({ inp: Input -> b_non_content(inp) }, { inp: Input -> ok(inp) }))
}

// [77] S-B-COMMENT 
fun s_b_comment(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> s_separate_in_line(inp) },
            { inp: Input -> opt(inp, { inp: Input -> c_nb_comment_text(inp) }) })) }) },
        { inp: Input -> b_comment(inp) }))
}

// [78] L-COMMENT 
fun l_comment(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> s_separate_in_line(inp) },
        { inp: Input -> opt(inp, { inp: Input -> c_nb_comment_text(inp) }) },
        { inp: Input -> b_non_content(inp) }))
}

// [79] S-L-COMMENTS 
fun s_l_comments(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> alt(inp, listOf({ inp: Input -> s_b_comment(inp) }, { inp: Input -> ok(inp) })) },
        { inp: Input -> star(inp, { inp: Input -> l_comment(inp) }) }))
}

// [80] S-SEPARATE 
fun s_separate(inp: Input, n: Int, c: String): Result {
    return when (c) {
        "BLOCK-OUT" -> s_separate_lines(inp, n)
        "BLOCK-IN" -> s_separate_lines(inp, n)
        "FLOW-OUT" -> s_separate_lines(inp, n)
        "FLOW-IN" -> s_separate_lines(inp, n)
        "BLOCK-KEY" -> s_separate_in_line(inp)
        "FLOW-KEY" -> s_separate_in_line(inp)
        else -> fail(inp, "no case")
    }
}

// [81] S-SEPARATE-LINES 
fun s_separate_lines(inp: Input, n: Int): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf({ inp: Input -> s_l_comments(inp) }, { inp: Input -> s_flow_line_prefix(inp, n) })) },
        { inp: Input -> s_separate_in_line(inp) }))
}

// [82] L-DIRECTIVE 
fun l_directive(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> c_directive(inp) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> ns_yaml_directive(inp) },
            { inp: Input -> ns_tag_directive(inp) },
            { inp: Input -> ns_reserved_directive(inp) })) },
        { inp: Input -> s_l_comments(inp) }))
}

// [83] NS-RESERVED-DIRECTIVE 
fun ns_reserved_directive(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_directive_name(inp) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> s_separate_in_line(inp) },
            { inp: Input -> ns_directive_parameter(inp) })) }) }))
}

// [84] NS-DIRECTIVE-NAME 
fun ns_directive_name(inp: Input): Result {
    return plus_(inp, { inp: Input -> ns_char(inp) })
}

// [85] NS-DIRECTIVE-PARAMETER 
fun ns_directive_parameter(inp: Input): Result {
    return plus_(inp, { inp: Input -> ns_char(inp) })
}

// [86] NS-YAML-DIRECTIVE 
fun ns_yaml_directive(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_str(inp, "YAML") },
        { inp: Input -> s_separate_in_line(inp) },
        { inp: Input -> ns_yaml_version(inp) }))
}

// [87] NS-YAML-VERSION 
fun ns_yaml_version(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> plus_(inp, { inp: Input -> ns_dec_digit(inp) }) },
        { inp: Input -> match_cp(inp, 46) },
        { inp: Input -> plus_(inp, { inp: Input -> ns_dec_digit(inp) }) }))
}

// [88] NS-TAG-DIRECTIVE 
fun ns_tag_directive(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_str(inp, "TAG") },
        { inp: Input -> s_separate_in_line(inp) },
        { inp: Input -> c_tag_handle(inp) },
        { inp: Input -> s_separate_in_line(inp) },
        { inp: Input -> ns_tag_prefix(inp) }))
}

// [89] C-TAG-HANDLE 
fun c_tag_handle(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_named_tag_handle(inp) },
        { inp: Input -> c_secondary_tag_handle(inp) },
        { inp: Input -> c_primary_tag_handle(inp) }))
}

// [90] C-PRIMARY-TAG-HANDLE 
fun c_primary_tag_handle(inp: Input): Result {
    return match_cp(inp, 33)
}

// [91] C-SECONDARY-TAG-HANDLE 
fun c_secondary_tag_handle(inp: Input): Result {
    return match_str(inp, "!!")
}

// [92] C-NAMED-TAG-HANDLE 
fun c_named_tag_handle(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 33) },
        { inp: Input -> plus_(inp, { inp: Input -> ns_word_char(inp) }) },
        { inp: Input -> match_cp(inp, 33) }))
}

// [93] NS-TAG-PREFIX 
fun ns_tag_prefix(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_ns_local_tag_prefix(inp) },
        { inp: Input -> ns_global_tag_prefix(inp) }))
}

// [94] C-NS-LOCAL-TAG-PREFIX 
fun c_ns_local_tag_prefix(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 33) },
        { inp: Input -> star(inp, { inp: Input -> ns_uri_char(inp) }) }))
}

// [95] NS-GLOBAL-TAG-PREFIX 
fun ns_global_tag_prefix(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_tag_char(inp) },
        { inp: Input -> star(inp, { inp: Input -> ns_uri_char(inp) }) }))
}

// [96] C-NS-PROPERTIES 
fun c_ns_properties(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> c_ns_tag_property(inp) },
            { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
                { inp: Input -> s_separate(inp, n, c) },
                { inp: Input -> c_ns_anchor_property(inp) })) }) })) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> c_ns_anchor_property(inp) },
            { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf({ inp: Input -> s_separate(inp, n, c) }, { inp: Input -> c_ns_tag_property(inp) })) }) })) }))
}

// [97] C-NS-TAG-PROPERTY 
fun c_ns_tag_property(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_verbatim_tag(inp) },
        { inp: Input -> c_ns_shorthand_tag(inp) },
        { inp: Input -> c_non_specific_tag(inp) }))
}

// [98] C-VERBATIM-TAG 
fun c_verbatim_tag(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> match_str(inp, "!<") },
        { inp: Input -> plus_(inp, { inp: Input -> ns_uri_char(inp) }) },
        { inp: Input -> match_cp(inp, 62) }))
}

// [99] C-NS-SHORTHAND-TAG 
fun c_ns_shorthand_tag(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> c_tag_handle(inp) },
        { inp: Input -> plus_(inp, { inp: Input -> ns_tag_char(inp) }) }))
}

// [100] C-NON-SPECIFIC-TAG 
fun c_non_specific_tag(inp: Input): Result {
    return match_cp(inp, 33)
}

// [101] C-NS-ANCHOR-PROPERTY 
fun c_ns_anchor_property(inp: Input): Result {
    return build(inp, "ANCHOR", { inp: Input -> seq(inp, listOf(
        { inp: Input -> c_anchor(inp) },
        { inp: Input -> scalar(inp, { inp: Input -> ns_anchor_name(inp) }) })) })
}

// [102] NS-ANCHOR-CHAR 
fun ns_anchor_char(inp: Input): Result {
    return minus(inp, { inp: Input -> ns_char(inp) }, { inp: Input -> c_flow_indicator(inp) })
}

// [103] NS-ANCHOR-NAME 
fun ns_anchor_name(inp: Input): Result {
    return plus_(inp, { inp: Input -> ns_anchor_char(inp) })
}

// [104] C-NS-ALIAS-NODE 
fun c_ns_alias_node(inp: Input): Result {
    return build(inp, "ALIAS", { inp: Input -> seq(inp, listOf(
        { inp: Input -> c_alias(inp) },
        { inp: Input -> scalar(inp, { inp: Input -> ns_anchor_name(inp) }) })) })
}

// [105] E-SCALAR 
fun e_scalar(inp: Input): Result {
    return ok(inp)
}

// [106] E-NODE 
fun e_node(inp: Input): Result {
    return e_scalar(inp)
}

// [107] NB-DOUBLE-CHAR 
fun nb_double_char(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_ns_esc_char(inp) },
        { inp: Input -> minus(inp, { inp: Input -> nb_json(inp) }, { inp: Input -> alt(inp, listOf({ inp: Input -> match_cp(inp, 92) }, { inp: Input -> match_cp(inp, 34) })) }) }))
}

// [108] NS-DOUBLE-CHAR 
fun ns_double_char(inp: Input): Result {
    return minus(inp, { inp: Input -> nb_double_char(inp) }, { inp: Input -> s_white(inp) })
}

// [109] C-DOUBLE-QUOTED 
fun c_double_quoted(inp: Input, n: Int, c: String): Result {
    return scalar(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> match_cp(inp, 34) },
        { inp: Input -> nb_double_text(inp, n, c) },
        { inp: Input -> match_cp(inp, 34) })) })
}

// [110] NB-DOUBLE-TEXT 
fun nb_double_text(inp: Input, n: Int, c: String): Result {
    return when (c) {
        "FLOW-OUT" -> nb_double_multi_line(inp, n)
        "FLOW-IN" -> nb_double_multi_line(inp, n)
        "BLOCK-KEY" -> nb_double_one_line(inp)
        "FLOW-KEY" -> nb_double_one_line(inp)
        else -> fail(inp, "no case")
    }
}

// [111] NB-DOUBLE-ONE-LINE 
fun nb_double_one_line(inp: Input): Result {
    return star(inp, { inp: Input -> nb_double_char(inp) })
}

// [112] S-DOUBLE-ESCAPED 
fun s_double_escaped(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) },
        { inp: Input -> match_cp(inp, 92) },
        { inp: Input -> b_non_content(inp) },
        { inp: Input -> star(inp, { inp: Input -> l_empty(inp, n, "FLOW-IN") }) },
        { inp: Input -> s_flow_line_prefix(inp, n) }))
}

// [113] S-DOUBLE-BREAK 
fun s_double_break(inp: Input, n: Int): Result {
    return alt(inp, listOf(
        { inp: Input -> s_double_escaped(inp, n) },
        { inp: Input -> s_flow_folded(inp, n) }))
}

// [114] NB-NS-DOUBLE-IN-LINE 
fun nb_ns_double_in_line(inp: Input): Result {
    return star(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) },
        { inp: Input -> ns_double_char(inp) })) })
}

// [115] S-DOUBLE-NEXT-LINE 
fun s_double_next_line(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_double_break(inp, n) },
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> ns_double_char(inp) },
            { inp: Input -> nb_ns_double_in_line(inp) },
            { inp: Input -> alt(inp, listOf(
                { inp: Input -> s_double_next_line(inp, n) },
                { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) })) })) }) }))
}

// [116] NB-DOUBLE-MULTI-LINE 
fun nb_double_multi_line(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> nb_ns_double_in_line(inp) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> s_double_next_line(inp, n) },
            { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) })) }))
}

// [117] C-QUOTED-QUOTE 
fun c_quoted_quote(inp: Input): Result {
    return match_str(inp, "''")
}

// [118] NB-SINGLE-CHAR 
fun nb_single_char(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_quoted_quote(inp) },
        { inp: Input -> minus(inp, { inp: Input -> nb_json(inp) }, { inp: Input -> match_cp(inp, 39) }) }))
}

// [119] NS-SINGLE-CHAR 
fun ns_single_char(inp: Input): Result {
    return minus(inp, { inp: Input -> nb_single_char(inp) }, { inp: Input -> s_white(inp) })
}

// [120] C-SINGLE-QUOTED 
fun c_single_quoted(inp: Input, n: Int, c: String): Result {
    return scalar(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> match_cp(inp, 39) },
        { inp: Input -> nb_single_text(inp, n, c) },
        { inp: Input -> match_cp(inp, 39) })) })
}

// [121] NB-SINGLE-TEXT 
fun nb_single_text(inp: Input, n: Int, c: String): Result {
    return when (c) {
        "FLOW-OUT" -> nb_single_multi_line(inp, n)
        "FLOW-IN" -> nb_single_multi_line(inp, n)
        "BLOCK-KEY" -> nb_single_one_line(inp)
        "FLOW-KEY" -> nb_single_one_line(inp)
        else -> fail(inp, "no case")
    }
}

// [122] NB-SINGLE-ONE-LINE 
fun nb_single_one_line(inp: Input): Result {
    return star(inp, { inp: Input -> nb_single_char(inp) })
}

// [123] NS-SINGLE-IN-LINE 
fun ns_single_in_line(inp: Input): Result {
    return star(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) },
        { inp: Input -> ns_single_char(inp) })) })
}

// [124] S-SINGLE-NEXT-LINE 
fun s_single_next_line(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_flow_folded(inp, n) },
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> ns_single_char(inp) },
            { inp: Input -> ns_single_in_line(inp) },
            { inp: Input -> alt(inp, listOf(
                { inp: Input -> s_single_next_line(inp, n) },
                { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) })) })) }) }))
}

// [125] NB-SINGLE-MULTI-LINE 
fun nb_single_multi_line(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_single_in_line(inp) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> s_single_next_line(inp, n) },
            { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) })) }))
}

// [126] NS-PLAIN-FIRST 
fun ns_plain_first(inp: Input, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> minus(inp, { inp: Input -> ns_char(inp) }, { inp: Input -> c_indicator(inp) }) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> alt(inp, listOf(
                { inp: Input -> match_cp(inp, 63) },
                { inp: Input -> match_cp(inp, 58) },
                { inp: Input -> match_cp(inp, 45) })) },
            { inp: Input -> ahead(inp, { inp: Input -> ns_plain_safe(inp, c) }) })) }))
}

// [127] NS-PLAIN-SAFE 
fun ns_plain_safe(inp: Input, c: String): Result {
    return when (c) {
        "FLOW-OUT" -> ns_plain_safe_out(inp)
        "FLOW-IN" -> ns_plain_safe_in(inp)
        "BLOCK-KEY" -> ns_plain_safe_out(inp)
        "FLOW-KEY" -> ns_plain_safe_in(inp)
        else -> fail(inp, "no case")
    }
}

// [128] NS-PLAIN-SAFE-OUT 
fun ns_plain_safe_out(inp: Input): Result {
    return ns_char(inp)
}

// [129] NS-PLAIN-SAFE-IN 
fun ns_plain_safe_in(inp: Input): Result {
    return minus(inp, { inp: Input -> ns_char(inp) }, { inp: Input -> c_flow_indicator(inp) })
}

// [130] NS-PLAIN-CHAR 
fun ns_plain_char(inp: Input, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> minus(inp, { inp: Input -> ns_plain_safe(inp, c) }, { inp: Input -> alt(inp, listOf({ inp: Input -> match_cp(inp, 58) }, { inp: Input -> match_cp(inp, 35) })) }) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> behind(inp, { inp: Input -> ns_char(inp) }) },
            { inp: Input -> match_cp(inp, 35) })) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 58) },
            { inp: Input -> ahead(inp, { inp: Input -> ns_plain_safe(inp, c) }) })) }))
}

// [131] NS-PLAIN 
fun ns_plain(inp: Input, n: Int, c: String): Result {
    return scalar(inp, { inp: Input -> when (c) {
        "FLOW-OUT" -> ns_plain_multi_line(inp, n, c)
        "FLOW-IN" -> ns_plain_multi_line(inp, n, c)
        "BLOCK-KEY" -> ns_plain_one_line(inp, c)
        "FLOW-KEY" -> ns_plain_one_line(inp, c)
        else -> fail(inp, "no case")
    } })
}

// [132] NB-NS-PLAIN-IN-LINE 
fun nb_ns_plain_in_line(inp: Input, c: String): Result {
    return star(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> s_white(inp) }) },
        { inp: Input -> ns_plain_char(inp, c) })) })
}

// [133] NS-PLAIN-ONE-LINE 
fun ns_plain_one_line(inp: Input, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_plain_first(inp, c) },
        { inp: Input -> nb_ns_plain_in_line(inp, c) }))
}

// [134] S-NS-PLAIN-NEXT-LINE 
fun s_ns_plain_next_line(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> s_flow_folded(inp, n) },
        { inp: Input -> r_neg(inp, { inp: Input -> c_forbidden(inp) }) },
        { inp: Input -> ns_plain_char(inp, c) },
        { inp: Input -> nb_ns_plain_in_line(inp, c) }))
}

// [135] NS-PLAIN-MULTI-LINE 
fun ns_plain_multi_line(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_plain_one_line(inp, c) },
        { inp: Input -> star(inp, { inp: Input -> s_ns_plain_next_line(inp, n, c) }) }))
}

// [137] C-FLOW-SEQUENCE 
fun c_flow_sequence(inp: Input, n: Int, c: String): Result {
    return build(inp, "SEQUENCE", { inp: Input -> seq(inp, listOf(
        { inp: Input -> match_cp(inp, 91) },
        { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
        { inp: Input -> opt(inp, { inp: Input -> collect(inp, { inp: Input -> ns_s_flow_seq_entries(inp, n, inFlow(c)) }) }) },
        { inp: Input -> match_cp(inp, 93) })) })
}

// [138] NS-S-FLOW-SEQ-ENTRIES 
fun ns_s_flow_seq_entries(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_flow_seq_entry(inp, n, c) },
        { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 44) },
            { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
            { inp: Input -> opt(inp, { inp: Input -> ns_s_flow_seq_entries(inp, n, c) }) })) }) }))
}

// [139] NS-FLOW-SEQ-ENTRY 
fun ns_flow_seq_entry(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> ns_flow_pair(inp, n, c) },
        { inp: Input -> ns_flow_node(inp, n, c) }))
}

// [140] C-FLOW-MAPPING 
fun c_flow_mapping(inp: Input, n: Int, c: String): Result {
    return build(inp, "MAPPING", { inp: Input -> seq(inp, listOf(
        { inp: Input -> match_cp(inp, 123) },
        { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
        { inp: Input -> opt(inp, { inp: Input -> collect(inp, { inp: Input -> ns_s_flow_map_entries(inp, n, inFlow(c)) }) }) },
        { inp: Input -> match_cp(inp, 125) })) })
}

// [141] NS-S-FLOW-MAP-ENTRIES 
fun ns_s_flow_map_entries(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_flow_map_entry(inp, n, c) },
        { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 44) },
            { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
            { inp: Input -> opt(inp, { inp: Input -> ns_s_flow_map_entries(inp, n, c) }) })) }) }))
}

// [142] NS-FLOW-MAP-ENTRY 
fun ns_flow_map_entry(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 63) },
            { inp: Input -> s_separate(inp, n, c) },
            { inp: Input -> ns_flow_map_explicit_entry(inp, n, c) })) },
        { inp: Input -> ns_flow_map_implicit_entry(inp, n, c) }))
}

// [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
fun ns_flow_map_explicit_entry(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> ns_flow_map_implicit_entry(inp, n, c) },
        { inp: Input -> seq(inp, listOf({ inp: Input -> e_node(inp) }, { inp: Input -> e_node(inp) })) }))
}

// [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
fun ns_flow_map_implicit_entry(inp: Input, n: Int, c: String): Result {
    return build(inp, "PAIR", { inp: Input -> alt(inp, listOf(
        { inp: Input -> ns_flow_map_yaml_key_entry(inp, n, c) },
        { inp: Input -> c_ns_flow_map_empty_key_entry(inp, n, c) },
        { inp: Input -> c_ns_flow_map_json_key_entry(inp, n, c) })) })
}

// [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
fun ns_flow_map_yaml_key_entry(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_flow_yaml_node(inp, n, c) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> seq(inp, listOf(
                { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
                { inp: Input -> c_ns_flow_map_separate_value(inp, n, c) })) },
            { inp: Input -> e_node(inp) })) }))
}

// [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
fun c_ns_flow_map_empty_key_entry(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> e_node(inp) },
        { inp: Input -> c_ns_flow_map_separate_value(inp, n, c) }))
}

// [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
fun c_ns_flow_map_separate_value(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 58) },
        { inp: Input -> r_neg(inp, { inp: Input -> ns_plain_safe(inp, c) }) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> seq(inp, listOf({ inp: Input -> s_separate(inp, n, c) }, { inp: Input -> ns_flow_node(inp, n, c) })) },
            { inp: Input -> e_node(inp) })) }))
}

// [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
fun c_ns_flow_map_json_key_entry(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> c_flow_json_node(inp, n, c) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> seq(inp, listOf(
                { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
                { inp: Input -> c_ns_flow_map_adjacent_value(inp, n, c) })) },
            { inp: Input -> e_node(inp) })) }))
}

// [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
fun c_ns_flow_map_adjacent_value(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 58) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> seq(inp, listOf(
                { inp: Input -> opt(inp, { inp: Input -> s_separate(inp, n, c) }) },
                { inp: Input -> ns_flow_node(inp, n, c) })) },
            { inp: Input -> e_node(inp) })) }))
}

// [150] NS-FLOW-PAIR 
fun ns_flow_pair(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> match_cp(inp, 63) },
            { inp: Input -> s_separate(inp, n, c) },
            { inp: Input -> ns_flow_map_explicit_entry(inp, n, c) })) },
        { inp: Input -> ns_flow_pair_entry(inp, n, c) }))
}

// [151] NS-FLOW-PAIR-ENTRY 
fun ns_flow_pair_entry(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> ns_flow_pair_yaml_key_entry(inp, n, c) },
        { inp: Input -> c_ns_flow_map_empty_key_entry(inp, n, c) },
        { inp: Input -> c_ns_flow_pair_json_key_entry(inp, n, c) }))
}

// [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
fun ns_flow_pair_yaml_key_entry(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_s_implicit_yaml_key(inp, "FLOW-KEY") },
        { inp: Input -> c_ns_flow_map_separate_value(inp, n, c) }))
}

// [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
fun c_ns_flow_pair_json_key_entry(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> c_s_implicit_json_key(inp, "FLOW-KEY") },
        { inp: Input -> c_ns_flow_map_adjacent_value(inp, n, c) }))
}

// [154] NS-S-IMPLICIT-YAML-KEY 
fun ns_s_implicit_yaml_key(inp: Input, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_flow_yaml_node(inp, 0, c) },
        { inp: Input -> opt(inp, { inp: Input -> s_separate_in_line(inp) }) }))
}

// [155] C-S-IMPLICIT-JSON-KEY 
fun c_s_implicit_json_key(inp: Input, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> c_flow_json_node(inp, 0, c) },
        { inp: Input -> opt(inp, { inp: Input -> s_separate_in_line(inp) }) }))
}

// [156] NS-FLOW-YAML-CONTENT 
fun ns_flow_yaml_content(inp: Input, n: Int, c: String): Result {
    return ns_plain(inp, n, c)
}

// [157] C-FLOW-JSON-CONTENT 
fun c_flow_json_content(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> c_flow_sequence(inp, n, c) },
        { inp: Input -> c_flow_mapping(inp, n, c) },
        { inp: Input -> c_single_quoted(inp, n, c) },
        { inp: Input -> c_double_quoted(inp, n, c) }))
}

// [158] NS-FLOW-CONTENT 
fun ns_flow_content(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> ns_flow_yaml_content(inp, n, c) },
        { inp: Input -> c_flow_json_content(inp, n, c) }))
}

// [159] NS-FLOW-YAML-NODE 
fun ns_flow_yaml_node(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> c_ns_alias_node(inp) },
        { inp: Input -> ns_flow_yaml_content(inp, n, c) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> c_ns_properties(inp, n, c) },
            { inp: Input -> alt(inp, listOf(
                { inp: Input -> seq(inp, listOf(
                    { inp: Input -> s_separate(inp, n, c) },
                    { inp: Input -> ns_flow_yaml_content(inp, n, c) })) },
                { inp: Input -> e_scalar(inp) })) })) }))
}

// [160] C-FLOW-JSON-NODE 
fun c_flow_json_node(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> c_ns_properties(inp, n, c) },
            { inp: Input -> s_separate(inp, n, c) })) }) },
        { inp: Input -> c_flow_json_content(inp, n, c) }))
}

// [161] NS-FLOW-NODE 
fun ns_flow_node(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> c_ns_alias_node(inp) },
        { inp: Input -> ns_flow_content(inp, n, c) },
        { inp: Input -> seq(inp, listOf(
            { inp: Input -> c_ns_properties(inp, n, c) },
            { inp: Input -> alt(inp, listOf(
                { inp: Input -> seq(inp, listOf(
                    { inp: Input -> s_separate(inp, n, c) },
                    { inp: Input -> ns_flow_content(inp, n, c) })) },
                { inp: Input -> e_scalar(inp) })) })) }))
}

// [162] C-B-BLOCK-HEADER 
fun c_b_block_header(inp: Input, n: Int): Result {
    return alt(inp, listOf(
        { inp: Input -> run { val r = alt(inp, listOf(
            { inp: Input -> parse_int(inp, { inp: Input -> ns_dec_digit(inp) }) },
            { inp: Input -> detect_indent(inp, n) })); if (r.fail) return@run r; val m = r.tagInt; val inp = r.rest; run { val r = alt(inp, listOf(
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 45) }, "STRIP") },
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 43) }, "KEEP") },
            { inp: Input -> r_val(inp, "CLIP") })); if (r.fail) return@run r; val t = r.tag; val inp = r.rest; s_b_comment(inp) } } },
        { inp: Input -> run { val r = alt(inp, listOf(
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 45) }, "STRIP") },
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 43) }, "KEEP") },
            { inp: Input -> r_val(inp, "CLIP") })); if (r.fail) return@run r; val t = r.tag; val inp = r.rest; run { val r = alt(inp, listOf(
            { inp: Input -> parse_int(inp, { inp: Input -> ns_dec_digit(inp) }) },
            { inp: Input -> detect_indent(inp, n) })); if (r.fail) return@run r; val m = r.tagInt; val inp = r.rest; s_b_comment(inp) } } }))
}

// [163] C-INDENTATION-INDICATOR 
fun c_indentation_indicator(inp: Input, n: Int): Result {
    return alt(inp, listOf({ inp: Input -> ns_dec_digit(inp) }, { inp: Input -> ok(inp) }))
}

// [164] C-CHOMPING-INDICATOR 
fun c_chomping_indicator(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> match_cp(inp, 45) },
        { inp: Input -> match_cp(inp, 43) },
        { inp: Input -> ok(inp) }))
}

// [165] B-CHOMPED-LAST 
fun b_chomped_last(inp: Input, t: String): Result {
    return when (t) {
        "STRIP" -> b_non_content(inp)
        "CLIP" -> b_as_line_feed(inp)
        "KEEP" -> b_as_line_feed(inp)
        else -> fail(inp, "no case")
    }
}

// [166] L-CHOMPED-EMPTY 
fun l_chomped_empty(inp: Input, n: Int, t: String): Result {
    return when (t) {
        "STRIP" -> l_strip_empty(inp, n)
        "CLIP" -> l_strip_empty(inp, n)
        "KEEP" -> l_keep_empty(inp, n)
        else -> fail(inp, "no case")
    }
}

// [167] L-STRIP-EMPTY 
fun l_strip_empty(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf({ inp: Input -> s_indent_le(inp, n) }, { inp: Input -> b_non_content(inp) })) }) },
        { inp: Input -> opt(inp, { inp: Input -> l_trail_comments(inp, n) }) }))
}

// [168] L-KEEP-EMPTY 
fun l_keep_empty(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> l_empty(inp, n, "BLOCK-IN") }) },
        { inp: Input -> opt(inp, { inp: Input -> l_trail_comments(inp, n) }) }))
}

// [169] L-TRAIL-COMMENTS 
fun l_trail_comments(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_indent_lt(inp, n) },
        { inp: Input -> c_nb_comment_text(inp) },
        { inp: Input -> b_comment(inp) },
        { inp: Input -> star(inp, { inp: Input -> l_comment(inp) }) }))
}

// [170] C-L+LITERAL 
fun c_lliteral(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 124) },
        { inp: Input -> run { val r = alt(inp, listOf(
            { inp: Input -> parse_int(inp, { inp: Input -> ns_dec_digit(inp) }) },
            { inp: Input -> detect_indent(inp, n) })); if (r.fail) return@run r; val m = r.tagInt; val inp = r.rest; run { val r = alt(inp, listOf(
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 45) }, "STRIP") },
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 43) }, "KEEP") },
            { inp: Input -> r_val(inp, "CLIP") })); if (r.fail) return@run r; val t = r.tag; val inp = r.rest; seq(inp, listOf(
            { inp: Input -> s_b_comment(inp) },
            { inp: Input -> l_literal_content(inp, (n + m), t) })) } } }))
}

// [171] L-NB-LITERAL-TEXT 
fun l_nb_literal_text(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> l_empty(inp, n, "BLOCK-IN") }) },
        { inp: Input -> s_indent(inp, n) },
        { inp: Input -> plus_(inp, { inp: Input -> nb_char(inp) }) }))
}

// [172] B-NB-LITERAL-NEXT 
fun b_nb_literal_next(inp: Input, n: Int): Result {
    return seq(inp, listOf({ inp: Input -> b_as_line_feed(inp) }, { inp: Input -> l_nb_literal_text(inp, n) }))
}

// [173] L-LITERAL-CONTENT 
fun l_literal_content(inp: Input, n: Int, t: String): Result {
    return scalar(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> l_nb_literal_text(inp, n) },
            { inp: Input -> star(inp, { inp: Input -> b_nb_literal_next(inp, n) }) },
            { inp: Input -> b_chomped_last(inp, t) })) }) },
        { inp: Input -> l_chomped_empty(inp, n, t) })) })
}

// [174] C-L+FOLDED 
fun c_lfolded(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 62) },
        { inp: Input -> run { val r = alt(inp, listOf(
            { inp: Input -> parse_int(inp, { inp: Input -> ns_dec_digit(inp) }) },
            { inp: Input -> detect_indent(inp, n) })); if (r.fail) return@run r; val m = r.tagInt; val inp = r.rest; run { val r = alt(inp, listOf(
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 45) }, "STRIP") },
            { inp: Input -> parse_sym(inp, { inp: Input -> match_cp(inp, 43) }, "KEEP") },
            { inp: Input -> r_val(inp, "CLIP") })); if (r.fail) return@run r; val t = r.tag; val inp = r.rest; seq(inp, listOf(
            { inp: Input -> s_b_comment(inp) },
            { inp: Input -> l_folded_content(inp, (n + m), t) })) } } }))
}

// [175] S-NB-FOLDED-TEXT 
fun s_nb_folded_text(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_indent(inp, n) },
        { inp: Input -> ns_char(inp) },
        { inp: Input -> star(inp, { inp: Input -> nb_char(inp) }) }))
}

// [176] L-NB-FOLDED-LINES 
fun l_nb_folded_lines(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_nb_folded_text(inp, n) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> b_l_folded(inp, n, "BLOCK-IN") },
            { inp: Input -> s_nb_folded_text(inp, n) })) }) }))
}

// [177] S-NB-SPACED-TEXT 
fun s_nb_spaced_text(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_indent(inp, n) },
        { inp: Input -> s_white(inp) },
        { inp: Input -> star(inp, { inp: Input -> nb_char(inp) }) }))
}

// [178] B-L-SPACED 
fun b_l_spaced(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> b_as_line_feed(inp) },
        { inp: Input -> star(inp, { inp: Input -> l_empty(inp, n, "BLOCK-IN") }) }))
}

// [179] L-NB-SPACED-LINES 
fun l_nb_spaced_lines(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_nb_spaced_text(inp, n) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf({ inp: Input -> b_l_spaced(inp, n) }, { inp: Input -> s_nb_spaced_text(inp, n) })) }) }))
}

// [180] L-NB-SAME-LINES 
fun l_nb_same_lines(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> l_empty(inp, n, "BLOCK-IN") }) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> l_nb_folded_lines(inp, n) },
            { inp: Input -> l_nb_spaced_lines(inp, n) })) }))
}

// [181] L-NB-DIFF-LINES 
fun l_nb_diff_lines(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> l_nb_same_lines(inp, n) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf({ inp: Input -> b_as_line_feed(inp) }, { inp: Input -> l_nb_same_lines(inp, n) })) }) }))
}

// [182] L-FOLDED-CONTENT 
fun l_folded_content(inp: Input, n: Int, t: String): Result {
    return scalar(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> l_nb_diff_lines(inp, n) },
            { inp: Input -> b_chomped_last(inp, t) })) }) },
        { inp: Input -> l_chomped_empty(inp, n, t) })) })
}

// [183] L+BLOCK-SEQUENCE 
fun lblock_sequence(inp: Input, n: Int): Result {
    return build(inp, "SEQUENCE", { inp: Input -> run { val r = detect_indent(inp, n); if (r.fail) return@run r; val m = r.tagInt; val inp = r.rest; collect(inp, { inp: Input -> plus_(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> s_indent(inp, (n + m)) },
        { inp: Input -> c_l_block_seq_entry(inp, (n + m)) })) }) }) } })
}

// [184] C-L-BLOCK-SEQ-ENTRY 
fun c_l_block_seq_entry(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 45) },
        { inp: Input -> r_neg(inp, { inp: Input -> ns_char(inp) }) },
        { inp: Input -> s_lblock_indented(inp, n, "BLOCK-IN") }))
}

// [185] S-L+BLOCK-INDENTED 
fun s_lblock_indented(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> run { val r = detect_indent(inp, 0); if (r.fail) return@run r; val m = r.tagInt; val inp = r.rest; seq(inp, listOf(
            { inp: Input -> s_indent(inp, m) },
            { inp: Input -> alt(inp, listOf(
                { inp: Input -> ns_l_compact_sequence(inp, (n + 1 + m)) },
                { inp: Input -> ns_l_compact_mapping(inp, (n + 1 + m)) })) })) } },
        { inp: Input -> s_lblock_node(inp, n, c) },
        { inp: Input -> seq(inp, listOf({ inp: Input -> e_node(inp) }, { inp: Input -> s_l_comments(inp) })) }))
}

// [186] NS-L-COMPACT-SEQUENCE 
fun ns_l_compact_sequence(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> c_l_block_seq_entry(inp, n) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf({ inp: Input -> s_indent(inp, n) }, { inp: Input -> c_l_block_seq_entry(inp, n) })) }) }))
}

// [187] L+BLOCK-MAPPING 
fun lblock_mapping(inp: Input, n: Int): Result {
    return build(inp, "MAPPING", { inp: Input -> run { val r = detect_indent(inp, n); if (r.fail) return@run r; val m = r.tagInt; val inp = r.rest; collect(inp, { inp: Input -> plus_(inp, { inp: Input -> seq(inp, listOf(
        { inp: Input -> s_indent(inp, (n + m)) },
        { inp: Input -> ns_l_block_map_entry(inp, (n + m)) })) }) }) } })
}

// [188] NS-L-BLOCK-MAP-ENTRY 
fun ns_l_block_map_entry(inp: Input, n: Int): Result {
    return alt(inp, listOf(
        { inp: Input -> c_l_block_map_explicit_entry(inp, n) },
        { inp: Input -> ns_l_block_map_implicit_entry(inp, n) }))
}

// [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
fun c_l_block_map_explicit_entry(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> c_l_block_map_explicit_key(inp, n) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> l_block_map_explicit_value(inp, n) },
            { inp: Input -> e_node(inp) })) }))
}

// [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
fun c_l_block_map_explicit_key(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 63) },
        { inp: Input -> s_lblock_indented(inp, n, "BLOCK-OUT") }))
}

// [191] L-BLOCK-MAP-EXPLICIT-VALUE 
fun l_block_map_explicit_value(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_indent(inp, n) },
        { inp: Input -> match_cp(inp, 58) },
        { inp: Input -> s_lblock_indented(inp, n, "BLOCK-OUT") }))
}

// [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
fun ns_l_block_map_implicit_entry(inp: Input, n: Int): Result {
    return build(inp, "PAIR", { inp: Input -> seq(inp, listOf(
        { inp: Input -> scalar(inp, { inp: Input -> alt(inp, listOf({ inp: Input -> ns_s_block_map_implicit_key(inp) }, { inp: Input -> e_node(inp) })) }) },
        { inp: Input -> c_l_block_map_implicit_value(inp, n) })) })
}

// [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
fun ns_s_block_map_implicit_key(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> c_s_implicit_json_key(inp, "BLOCK-KEY") },
        { inp: Input -> ns_s_implicit_yaml_key(inp, "BLOCK-KEY") }))
}

// [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
fun c_l_block_map_implicit_value(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> match_cp(inp, 58) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> s_lblock_node(inp, n, "BLOCK-OUT") },
            { inp: Input -> scalar(inp, { inp: Input -> seq(inp, listOf({ inp: Input -> e_node(inp) }, { inp: Input -> s_l_comments(inp) })) }) })) }))
}

// [195] NS-L-COMPACT-MAPPING 
fun ns_l_compact_mapping(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> ns_l_block_map_entry(inp, n) },
        { inp: Input -> star(inp, { inp: Input -> seq(inp, listOf({ inp: Input -> s_indent(inp, n) }, { inp: Input -> ns_l_block_map_entry(inp, n) })) }) }))
}

// [196] S-L+BLOCK-NODE 
fun s_lblock_node(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> s_lblock_in_block(inp, n, c) },
        { inp: Input -> s_lflow_in_block(inp, n) }))
}

// [197] S-L+FLOW-IN-BLOCK 
fun s_lflow_in_block(inp: Input, n: Int): Result {
    return seq(inp, listOf(
        { inp: Input -> s_separate(inp, (n + 1), "FLOW-OUT") },
        { inp: Input -> ns_flow_node(inp, (n + 1), "FLOW-OUT") },
        { inp: Input -> s_l_comments(inp) }))
}

// [198] S-L+BLOCK-IN-BLOCK 
fun s_lblock_in_block(inp: Input, n: Int, c: String): Result {
    return alt(inp, listOf(
        { inp: Input -> s_lblock_scalar(inp, n, c) },
        { inp: Input -> s_lblock_collection(inp, n, c) }))
}

// [199] S-L+BLOCK-SCALAR 
fun s_lblock_scalar(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> s_separate(inp, (n + 1), c) },
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> c_ns_properties(inp, (n + 1), c) },
            { inp: Input -> s_separate(inp, (n + 1), c) })) }) },
        { inp: Input -> alt(inp, listOf({ inp: Input -> c_lliteral(inp, n) }, { inp: Input -> c_lfolded(inp, n) })) }))
}

// [200] S-L+BLOCK-COLLECTION 
fun s_lblock_collection(inp: Input, n: Int, c: String): Result {
    return seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> seq(inp, listOf(
            { inp: Input -> s_separate(inp, (n + 1), c) },
            { inp: Input -> c_ns_properties(inp, (n + 1), c) })) }) },
        { inp: Input -> s_l_comments(inp) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> lblock_sequence(inp, seqSpaces(n, c)) },
            { inp: Input -> lblock_mapping(inp, n) })) }))
}

// [202] L-DOCUMENT-PREFIX 
fun l_document_prefix(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> opt(inp, { inp: Input -> c_byte_order_mark(inp) }) },
        { inp: Input -> star(inp, { inp: Input -> l_comment(inp) }) }))
}

// [203] C-DIRECTIVES-END 
fun c_directives_end(inp: Input): Result {
    return match_str(inp, "---")
}

// [204] C-DOCUMENT-END 
fun c_document_end(inp: Input): Result {
    return match_str(inp, "...")
}

// [205] L-DOCUMENT-SUFFIX 
fun l_document_suffix(inp: Input): Result {
    return seq(inp, listOf({ inp: Input -> c_document_end(inp) }, { inp: Input -> s_l_comments(inp) }))
}

// [206] C-FORBIDDEN 
fun c_forbidden(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> sol(inp) },
        { inp: Input -> alt(inp, listOf({ inp: Input -> c_directives_end(inp) }, { inp: Input -> c_document_end(inp) })) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> b_char(inp) },
            { inp: Input -> s_white(inp) },
            { inp: Input -> eof_ok(inp) })) }))
}

// [207] L-BARE-DOCUMENT 
fun l_bare_document(inp: Input): Result {
    return build(inp, "DOC", { inp: Input -> s_lblock_node(inp, -1, "BLOCK-IN") })
}

// [208] L-EXPLICIT-DOCUMENT 
fun l_explicit_document(inp: Input): Result {
    return build(inp, "DOC", { inp: Input -> seq(inp, listOf(
        { inp: Input -> c_directives_end(inp) },
        { inp: Input -> alt(inp, listOf(
            { inp: Input -> l_bare_document(inp) },
            { inp: Input -> seq(inp, listOf({ inp: Input -> e_node(inp) }, { inp: Input -> s_l_comments(inp) })) })) })) })
}

// [209] L-DIRECTIVE-DOCUMENT 
fun l_directive_document(inp: Input): Result {
    return seq(inp, listOf(
        { inp: Input -> plus_(inp, { inp: Input -> l_directive(inp) }) },
        { inp: Input -> l_explicit_document(inp) }))
}

// [210] L-ANY-DOCUMENT 
fun l_any_document(inp: Input): Result {
    return alt(inp, listOf(
        { inp: Input -> l_directive_document(inp) },
        { inp: Input -> l_explicit_document(inp) },
        { inp: Input -> l_bare_document(inp) }))
}

// [211] L-YAML-STREAM 
fun l_yaml_stream(inp: Input): Result {
    return build(inp, "STREAM", { inp: Input -> seq(inp, listOf(
        { inp: Input -> star(inp, { inp: Input -> l_document_prefix(inp) }) },
        { inp: Input -> opt(inp, { inp: Input -> l_any_document(inp) }) },
        { inp: Input -> star(inp, { inp: Input -> alt(inp, listOf(
            { inp: Input -> seq(inp, listOf(
                { inp: Input -> plus_(inp, { inp: Input -> l_document_suffix(inp) }) },
                { inp: Input -> star(inp, { inp: Input -> l_document_prefix(inp) }) },
                { inp: Input -> opt(inp, { inp: Input -> l_any_document(inp) }) })) },
            { inp: Input -> seq(inp, listOf(
                { inp: Input -> star(inp, { inp: Input -> l_document_prefix(inp) }) },
                { inp: Input -> opt(inp, { inp: Input -> l_explicit_document(inp) }) })) })) }) })) })
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
    val r = l_yaml_stream(inp)
    if (!r.fail) {
        println("OK: ${r.rest.pos} chars")
        r.ast?.let { printAst(it, 0) }
    } else {
        System.err.println("FAIL @${r.rest.pos}: ${r.err}")
        kotlin.system.exitProcess(1)
    }
}
