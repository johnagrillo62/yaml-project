-- ════════════════════════════════════════════════════════════════
-- yaml_reader.lua — YAML 1.2 parser
-- ════════════════════════════════════════════════════════════════

-- ── Input ──

local function newInput(s)
    return { src = s, pos = 1, line = 1, col = 0 }
end

local function atEof(i) return i.pos > #i.src end

local function peekCp(i)
    if atEof(i) then return -1 end
    return string.byte(i.src, i.pos)
end

local function adv(i)
    if atEof(i) then return i end
    local c = string.byte(i.src, i.pos)
    local nl = c == 10
    return { src = i.src, pos = i.pos + 1, line = nl and i.line + 1 or i.line, col = nl and 0 or i.col + 1 }
end

-- ── AST ──

local function makeAst(tag) return { tag = tag, children = {}, isLeaf = false } end
local function leafAst(text) return { text = text, isLeaf = true } end

-- ── Result ──

local function ok(inp) return { fail = false, val = "", rest = inp, tag = "", tagInt = 0 } end
local function okV(inp, v) return { fail = false, val = v, rest = inp, tag = "", tagInt = 0 } end
local function fail(inp, msg) return { fail = true, val = "", rest = inp, err = msg, tag = "", tagInt = 0 } end

-- ── Context ──

local function inFlow(c)
    if c == "FLOW-OUT" or c == "FLOW-IN" then return "FLOW-IN" end
    return "FLOW-KEY"
end

local function seqSpaces(n, c)
    if c == "BLOCK-OUT" then return n - 1 end
    return n
end

-- ── Combinators ──

local function match_cp(inp, cp)
    local c = peekCp(inp)
    if c == cp then
        local s = string.char(c)
        return okV(adv(inp), s)
    end
    return fail(inp, "cp")
end

local function match_range(inp, lo, hi)
    local c = peekCp(inp)
    if c >= lo and c <= hi then
        local s = string.char(c)
        return okV(adv(inp), s)
    end
    return fail(inp, "rng")
end

local function match_str(inp, t)
    local n = #t
    if inp.pos + n - 1 > #inp.src then return fail(inp, "str") end
    if string.sub(inp.src, inp.pos, inp.pos + n - 1) ~= t then return fail(inp, "str") end
    local cur = inp
    for i = 1, n do cur = adv(cur) end
    return okV(cur, t)
end

local function mergeAsts(dst, r)
    if r.ast then table.insert(dst, r.ast) end
    if r.astList then for _, a in ipairs(r.astList) do table.insert(dst, a) end end
end

local function seq(inp, fns)
    local cur = inp; local acc = {}; local asts = {}
    for _, f in ipairs(fns) do
        local r = f(cur); if r.fail then return r end
        table.insert(acc, r.val); mergeAsts(asts, r); cur = r.rest
    end
    local res = okV(cur, table.concat(acc))
    if #asts == 1 then res.ast = asts[1] elseif #asts > 1 then res.astList = asts end
    return res
end

local function alt(inp, fns)
    for _, f in ipairs(fns) do local r = f(inp); if not r.fail then return r end end
    return fail(inp, "alt")
end

local function star(inp, f)
    local cur = inp; local acc = {}; local asts = {}
    while true do
        local r = f(cur); if r.fail or r.rest.pos <= cur.pos then break end
        table.insert(acc, r.val); mergeAsts(asts, r); cur = r.rest
    end
    local res = okV(cur, table.concat(acc))
    if #asts > 0 then res.astList = asts end
    return res
end

local function plus_(inp, f)
    local first = f(inp); if first.fail then return first end
    local rest = star(first.rest, f)
    local res = okV(rest.rest, first.val .. rest.val)
    local asts = {}; mergeAsts(asts, first); mergeAsts(asts, rest)
    if #asts > 0 then res.astList = asts end
    return res
end

local function opt(inp, f) local r = f(inp); if r.fail then return ok(inp) end; return r end
local function neg(inp, f) local r = f(inp); if r.fail then return ok(inp) end; return fail(inp, "neg") end
local function minus(inp, fa, fb)
    local ra = fa(inp); if ra.fail then return ra end
    local rb = fb(inp); if not rb.fail and rb.rest.pos == ra.rest.pos then return fail(inp, "excl") end; return ra
end
local function rep(inp, n, f)
    local cur = inp; local acc = {}
    for i = 1, n do local r = f(cur); if r.fail then return r end; table.insert(acc, r.val); cur = r.rest end
    return okV(cur, table.concat(acc))
end
local function ahead(inp, f) local r = f(inp); if r.fail then return r end; return ok(inp) end
local function behind(inp, f)
    if inp.pos <= 1 then return fail(inp, "bh") end
    local t = { src = inp.src, pos = inp.pos - 1, line = inp.line, col = math.max(0, inp.col - 1) }
    local r = f(t); if r.fail then return fail(inp, "bh") end; return ok(inp)
end
local function sol(inp) if inp.col == 0 then return ok(inp) end; return fail(inp, "sol") end
local function eof_ok(inp) if atEof(inp) then return ok(inp) end; return fail(inp, "eof") end

-- ── YAML extensions ──

local function build(inp, typ, f)
    local r = f(inp); if r.fail then return r end
    local node = makeAst(typ)
    if r.ast then table.insert(node.children, r.ast) end
    if r.astList then for _, a in ipairs(r.astList) do table.insert(node.children, a) end end
    r.ast = node; r.astList = nil; return r
end

local function scalar(inp, f)
    local r = f(inp); if r.fail then return r end
    r.ast = leafAst(r.val); return r
end

local function collect(inp, f) return f(inp) end

local function detect_indent(inp, n)
    local s = inp.src; local l = #s; local i = inp.pos
    local sp = 0; while i + sp <= l and string.byte(s, i + sp) == 32 do sp = sp + 1 end
    if i + sp <= l and string.byte(s, i + sp) ~= 10 then
        local r = ok(inp); r.tagInt = math.max(1, sp - n); return r
    end
    local j = i; while j <= l and string.byte(s, j) ~= 10 do j = j + 1 end
    while j <= l do
        if string.byte(s, j) == 10 then j = j + 1 end; if j > l then break end
        sp = 0; while j + sp <= l and string.byte(s, j + sp) == 32 do sp = sp + 1 end
        local nx = j + sp; if nx > l or string.byte(s, nx) == 10 then j = nx else
            local r = ok(inp); r.tagInt = math.max(1, sp - n); return r
        end
    end
    local r = ok(inp); r.tagInt = 1; return r
end

local function parse_int(inp, f)
    local r = f(inp); if r.fail then return r end
    local v = 0; for i = 1, #r.val do
        local c = string.byte(r.val, i)
        if c >= 48 and c <= 57 then v = v * 10 + (c - 48) end
    end
    r.tagInt = v; return r
end

local function parse_sym(inp, f, sym)
    local r = f(inp); if r.fail then return r end; r.tag = sym; return r
end

local function val(inp, v) local r = ok(inp); r.tag = v; return r end

-- ════════════════════════════════════════════════════════════════ 
-- YAML 1.2 Grammar — 211 rules 
-- ════════════════════════════════════════════════════════════════ 

-- [1] C-PRINTABLE 
function c_printable(inp)
    return alt(inp, {
        function(inp) return match_cp(inp, 0x9) end,
        function(inp) return match_cp(inp, 0x0A) end,
        function(inp) return match_cp(inp, 0x0D) end,
        function(inp) return match_range(inp, 0x20, 0x7E) end,
        function(inp) return match_cp(inp, 0x85) end,
        function(inp) return match_range(inp, 0xA0, 0xD7FF) end,
        function(inp) return match_range(inp, 0xE000, 0xFFFD) end,
        function(inp) return match_range(inp, 0x10000, 0x10FFFF) end})
end

-- [2] NB-JSON 
function nb_json(inp)
    return alt(inp, {
        function(inp) return match_cp(inp, 0x9) end,
        function(inp) return match_range(inp, 0x20, 0x10FFFF) end})
end

-- [3] C-BYTE-ORDER-MARK 
function c_byte_order_mark(inp)
    return match_cp(inp, 0xFEFF)
end

-- [4] C-SEQUENCE-ENTRY 
function c_sequence_entry(inp)
    return match_cp(inp, 45)
end

-- [5] C-MAPPING-KEY 
function c_mapping_key(inp)
    return match_cp(inp, 63)
end

-- [6] C-MAPPING-VALUE 
function c_mapping_value(inp)
    return match_cp(inp, 58)
end

-- [7] C-COLLECT-ENTRY 
function c_collect_entry(inp)
    return match_cp(inp, 44)
end

-- [8] C-SEQUENCE-START 
function c_sequence_start(inp)
    return match_cp(inp, 91)
end

-- [9] C-SEQUENCE-END 
function c_sequence_end(inp)
    return match_cp(inp, 93)
end

-- [10] C-MAPPING-START 
function c_mapping_start(inp)
    return match_cp(inp, 123)
end

-- [11] C-MAPPING-END 
function c_mapping_end(inp)
    return match_cp(inp, 125)
end

-- [12] C-COMMENT 
function c_comment(inp)
    return match_cp(inp, 35)
end

-- [13] C-ANCHOR 
function c_anchor(inp)
    return match_cp(inp, 38)
end

-- [14] C-ALIAS 
function c_alias(inp)
    return match_cp(inp, 42)
end

-- [15] C-TAG 
function c_tag(inp)
    return match_cp(inp, 33)
end

-- [16] C-LITERAL 
function c_literal(inp)
    return match_cp(inp, 124)
end

-- [17] C-FOLDED 
function c_folded(inp)
    return match_cp(inp, 62)
end

-- [18] C-SINGLE-QUOTE 
function c_single_quote(inp)
    return match_cp(inp, 39)
end

-- [19] C-DOUBLE-QUOTE 
function c_double_quote(inp)
    return match_cp(inp, 34)
end

-- [20] C-DIRECTIVE 
function c_directive(inp)
    return match_cp(inp, 37)
end

-- [21] C-RESERVED 
function c_reserved(inp)
    return alt(inp, {function(inp) return match_cp(inp, 64) end, function(inp) return match_cp(inp, 96) end})
end

-- [22] C-INDICATOR 
function c_indicator(inp)
    return alt(inp, {
        function(inp) return c_sequence_entry(inp) end,
        function(inp) return c_mapping_key(inp) end,
        function(inp) return c_mapping_value(inp) end,
        function(inp) return c_collect_entry(inp) end,
        function(inp) return c_sequence_start(inp) end,
        function(inp) return c_sequence_end(inp) end,
        function(inp) return c_mapping_start(inp) end,
        function(inp) return c_mapping_end(inp) end,
        function(inp) return c_comment(inp) end,
        function(inp) return c_anchor(inp) end,
        function(inp) return c_alias(inp) end,
        function(inp) return c_tag(inp) end,
        function(inp) return c_literal(inp) end,
        function(inp) return c_folded(inp) end,
        function(inp) return c_single_quote(inp) end,
        function(inp) return c_double_quote(inp) end,
        function(inp) return c_directive(inp) end,
        function(inp) return c_reserved(inp) end})
end

-- [23] C-FLOW-INDICATOR 
function c_flow_indicator(inp)
    return alt(inp, {
        function(inp) return c_collect_entry(inp) end,
        function(inp) return c_sequence_start(inp) end,
        function(inp) return c_sequence_end(inp) end,
        function(inp) return c_mapping_start(inp) end,
        function(inp) return c_mapping_end(inp) end})
end

-- [24] B-LINE-FEED 
function b_line_feed(inp)
    return match_cp(inp, 0x0A)
end

-- [25] B-CARRIAGE-RETURN 
function b_carriage_return(inp)
    return match_cp(inp, 0x0D)
end

-- [26] B-CHAR 
function b_char(inp)
    return alt(inp, {
        function(inp) return b_line_feed(inp) end,
        function(inp) return b_carriage_return(inp) end})
end

-- [27] NB-CHAR 
function nb_char(inp)
    return minus(inp, function(inp) return c_printable(inp) end, function(inp) return alt(inp, {function(inp) return b_char(inp) end, function(inp) return c_byte_order_mark(inp) end}) end)
end

-- [28] B-BREAK 
function b_break(inp)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return b_carriage_return(inp) end,
            function(inp) return b_line_feed(inp) end}) end,
        function(inp) return b_carriage_return(inp) end,
        function(inp) return b_line_feed(inp) end})
end

-- [29] B-AS-LINE-FEED 
function b_as_line_feed(inp)
    return b_break(inp)
end

-- [30] B-NON-CONTENT 
function b_non_content(inp)
    return b_break(inp)
end

-- [31] S-SPACE 
function s_space(inp)
    return match_cp(inp, 0x20)
end

-- [32] S-TAB 
function s_tab(inp)
    return match_cp(inp, 0x9)
end

-- [33] S-WHITE 
function s_white(inp)
    return alt(inp, {function(inp) return s_space(inp) end, function(inp) return s_tab(inp) end})
end

-- [34] NS-CHAR 
function ns_char(inp)
    return minus(inp, function(inp) return nb_char(inp) end, function(inp) return s_white(inp) end)
end

-- [35] NS-DEC-DIGIT 
function ns_dec_digit(inp)
    return match_range(inp, 0x30, 0x39)
end

-- [36] NS-HEX-DIGIT 
function ns_hex_digit(inp)
    return alt(inp, {
        function(inp) return ns_dec_digit(inp) end,
        function(inp) return match_range(inp, 0x41, 0x46) end,
        function(inp) return match_range(inp, 0x61, 0x66) end})
end

-- [37] NS-ASCII-LETTER 
function ns_ascii_letter(inp)
    return alt(inp, {
        function(inp) return match_range(inp, 0x41, 0x5A) end,
        function(inp) return match_range(inp, 0x61, 0x7A) end})
end

-- [38] NS-WORD-CHAR 
function ns_word_char(inp)
    return alt(inp, {
        function(inp) return ns_dec_digit(inp) end,
        function(inp) return ns_ascii_letter(inp) end,
        function(inp) return match_cp(inp, 45) end})
end

-- [39] NS-URI-CHAR 
function ns_uri_char(inp)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 37) end,
            function(inp) return ns_hex_digit(inp) end,
            function(inp) return ns_hex_digit(inp) end}) end,
        function(inp) return ns_word_char(inp) end,
        function(inp) return match_cp(inp, 35) end,
        function(inp) return match_cp(inp, 59) end,
        function(inp) return match_cp(inp, 47) end,
        function(inp) return match_cp(inp, 63) end,
        function(inp) return match_cp(inp, 58) end,
        function(inp) return match_cp(inp, 64) end,
        function(inp) return match_cp(inp, 38) end,
        function(inp) return match_cp(inp, 61) end,
        function(inp) return match_cp(inp, 43) end,
        function(inp) return match_cp(inp, 36) end,
        function(inp) return match_cp(inp, 44) end,
        function(inp) return match_cp(inp, 95) end,
        function(inp) return match_cp(inp, 46) end,
        function(inp) return match_cp(inp, 33) end,
        function(inp) return match_cp(inp, 126) end,
        function(inp) return match_cp(inp, 42) end,
        function(inp) return match_cp(inp, 39) end,
        function(inp) return match_cp(inp, 40) end,
        function(inp) return match_cp(inp, 41) end,
        function(inp) return match_cp(inp, 91) end,
        function(inp) return match_cp(inp, 93) end})
end

-- [40] NS-TAG-CHAR 
function ns_tag_char(inp)
    return minus(inp, function(inp) return ns_uri_char(inp) end, function(inp) return alt(inp, {function(inp) return c_tag(inp) end, function(inp) return c_flow_indicator(inp) end}) end)
end

-- [41] C-ESCAPE 
function c_escape(inp)
    return match_cp(inp, 92)
end

-- [42] NS-ESC-NULL 
function ns_esc_null(inp)
    return match_cp(inp, 48)
end

-- [43] NS-ESC-BELL 
function ns_esc_bell(inp)
    return match_cp(inp, 97)
end

-- [44] NS-ESC-BACKSPACE 
function ns_esc_backspace(inp)
    return match_cp(inp, 98)
end

-- [45] NS-ESC-HORIZONTAL-TAB 
function ns_esc_horizontal_tab(inp)
    return match_cp(inp, 116)
end

-- [46] NS-ESC-LINE-FEED 
function ns_esc_line_feed(inp)
    return match_cp(inp, 110)
end

-- [47] NS-ESC-VERTICAL-TAB 
function ns_esc_vertical_tab(inp)
    return match_cp(inp, 118)
end

-- [48] NS-ESC-FORM-FEED 
function ns_esc_form_feed(inp)
    return match_cp(inp, 102)
end

-- [49] NS-ESC-CARRIAGE-RETURN 
function ns_esc_carriage_return(inp)
    return match_cp(inp, 114)
end

-- [50] NS-ESC-ESCAPE 
function ns_esc_escape(inp)
    return match_cp(inp, 101)
end

-- [51] NS-ESC-SPACE 
function ns_esc_space(inp)
    return match_cp(inp, 0x20)
end

-- [52] NS-ESC-DOUBLE-QUOTE 
function ns_esc_double_quote(inp)
    return match_cp(inp, 34)
end

-- [53] NS-ESC-SLASH 
function ns_esc_slash(inp)
    return match_cp(inp, 47)
end

-- [54] NS-ESC-BACKSLASH 
function ns_esc_backslash(inp)
    return match_cp(inp, 92)
end

-- [55] NS-ESC-NEXT-LINE 
function ns_esc_next_line(inp)
    return match_cp(inp, 78)
end

-- [56] NS-ESC-NON-BREAKING-SPACE 
function ns_esc_non_breaking_space(inp)
    return match_cp(inp, 95)
end

-- [57] NS-ESC-LINE-SEPARATOR 
function ns_esc_line_separator(inp)
    return match_cp(inp, 76)
end

-- [58] NS-ESC-PARAGRAPH-SEPARATOR 
function ns_esc_paragraph_separator(inp)
    return match_cp(inp, 80)
end

-- [59] NS-ESC-8-BIT 
function ns_esc_8_bit(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 120) end,
        function(inp) return rep(inp, 2, function(inp) return ns_hex_digit(inp) end) end})
end

-- [60] NS-ESC-16-BIT 
function ns_esc_16_bit(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 117) end,
        function(inp) return rep(inp, 4, function(inp) return ns_hex_digit(inp) end) end})
end

-- [61] NS-ESC-32-BIT 
function ns_esc_32_bit(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 85) end,
        function(inp) return rep(inp, 8, function(inp) return ns_hex_digit(inp) end) end})
end

-- [62] C-NS-ESC-CHAR 
function c_ns_esc_char(inp)
    return seq(inp, {
        function(inp) return c_escape(inp) end,
        function(inp) return alt(inp, {
            function(inp) return ns_esc_null(inp) end,
            function(inp) return ns_esc_bell(inp) end,
            function(inp) return ns_esc_backspace(inp) end,
            function(inp) return ns_esc_horizontal_tab(inp) end,
            function(inp) return ns_esc_line_feed(inp) end,
            function(inp) return ns_esc_vertical_tab(inp) end,
            function(inp) return ns_esc_form_feed(inp) end,
            function(inp) return ns_esc_carriage_return(inp) end,
            function(inp) return ns_esc_escape(inp) end,
            function(inp) return ns_esc_space(inp) end,
            function(inp) return ns_esc_double_quote(inp) end,
            function(inp) return ns_esc_slash(inp) end,
            function(inp) return ns_esc_backslash(inp) end,
            function(inp) return ns_esc_next_line(inp) end,
            function(inp) return ns_esc_non_breaking_space(inp) end,
            function(inp) return ns_esc_line_separator(inp) end,
            function(inp) return ns_esc_paragraph_separator(inp) end,
            function(inp) return ns_esc_8_bit(inp) end,
            function(inp) return ns_esc_16_bit(inp) end,
            function(inp) return ns_esc_32_bit(inp) end}) end})
end

-- [63] S-INDENT 
function s_indent(inp, n)
    return rep(inp, n, function(inp) return s_space(inp) end)
end

-- [64] S-INDENT-LT 
function s_indent_lt(inp, n)
    return star(inp, function(inp) return s_space(inp) end)
end

-- [65] S-INDENT-LE 
function s_indent_le(inp, n)
    return star(inp, function(inp) return s_space(inp) end)
end

-- [66] S-SEPARATE-IN-LINE 
function s_separate_in_line(inp)
    return alt(inp, {
        function(inp) return plus_(inp, function(inp) return s_white(inp) end) end,
        function(inp) return ok(inp) end})
end

-- [67] S-LINE-PREFIX 
function s_line_prefix(inp, n, c)
    return (function()
        if c == "BLOCK-IN" then return s_block_line_prefix(inp, n)
        elseif c == "BLOCK-OUT" then return s_block_line_prefix(inp, n)
        elseif c == "FLOW-IN" then return s_flow_line_prefix(inp, n)
        elseif c == "FLOW-OUT" then return s_flow_line_prefix(inp, n)
        else return fail(inp, "no case")
        end
    end)()
end

-- [68] S-BLOCK-LINE-PREFIX 
function s_block_line_prefix(inp, n)
    return s_indent(inp, n)
end

-- [69] S-FLOW-LINE-PREFIX 
function s_flow_line_prefix(inp, n)
    return seq(inp, {
        function(inp) return s_indent(inp, n) end,
        function(inp) return opt(inp, function(inp) return s_separate_in_line(inp) end) end})
end

-- [70] L-EMPTY 
function l_empty(inp, n, c)
    return seq(inp, {
        function(inp) return alt(inp, {
            function(inp) return s_line_prefix(inp, n, c) end,
            function(inp) return s_indent_lt(inp, n) end}) end,
        function(inp) return b_as_line_feed(inp) end})
end

-- [71] B-L-TRIMMED 
function b_l_trimmed(inp, n, c)
    return seq(inp, {
        function(inp) return b_non_content(inp) end,
        function(inp) return plus_(inp, function(inp) return l_empty(inp, n, c) end) end})
end

-- [72] B-AS-SPACE 
function b_as_space(inp)
    return b_break(inp)
end

-- [73] B-L-FOLDED 
function b_l_folded(inp, n, c)
    return alt(inp, {
        function(inp) return b_l_trimmed(inp, n, c) end,
        function(inp) return b_as_space(inp) end})
end

-- [74] S-FLOW-FOLDED 
function s_flow_folded(inp, n)
    return seq(inp, {
        function(inp) return opt(inp, function(inp) return s_separate_in_line(inp) end) end,
        function(inp) return b_l_folded(inp, n, "FLOW-IN") end,
        function(inp) return s_flow_line_prefix(inp, n) end})
end

-- [75] C-NB-COMMENT-TEXT 
function c_nb_comment_text(inp)
    return seq(inp, {
        function(inp) return c_comment(inp) end,
        function(inp) return star(inp, function(inp) return nb_char(inp) end) end})
end

-- [76] B-COMMENT 
function b_comment(inp)
    return alt(inp, {function(inp) return b_non_content(inp) end, function(inp) return ok(inp) end})
end

-- [77] S-B-COMMENT 
function s_b_comment(inp)
    return seq(inp, {
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return s_separate_in_line(inp) end,
            function(inp) return opt(inp, function(inp) return c_nb_comment_text(inp) end) end}) end) end,
        function(inp) return b_comment(inp) end})
end

-- [78] L-COMMENT 
function l_comment(inp)
    return seq(inp, {
        function(inp) return s_separate_in_line(inp) end,
        function(inp) return opt(inp, function(inp) return c_nb_comment_text(inp) end) end,
        function(inp) return b_non_content(inp) end})
end

-- [79] S-L-COMMENTS 
function s_l_comments(inp)
    return seq(inp, {
        function(inp) return alt(inp, {function(inp) return s_b_comment(inp) end, function(inp) return ok(inp) end}) end,
        function(inp) return star(inp, function(inp) return l_comment(inp) end) end})
end

-- [80] S-SEPARATE 
function s_separate(inp, n, c)
    return (function()
        if c == "BLOCK-OUT" then return s_separate_lines(inp, n)
        elseif c == "BLOCK-IN" then return s_separate_lines(inp, n)
        elseif c == "FLOW-OUT" then return s_separate_lines(inp, n)
        elseif c == "FLOW-IN" then return s_separate_lines(inp, n)
        elseif c == "BLOCK-KEY" then return s_separate_in_line(inp)
        elseif c == "FLOW-KEY" then return s_separate_in_line(inp)
        else return fail(inp, "no case")
        end
    end)()
end

-- [81] S-SEPARATE-LINES 
function s_separate_lines(inp, n)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return s_l_comments(inp) end,
            function(inp) return s_flow_line_prefix(inp, n) end}) end,
        function(inp) return s_separate_in_line(inp) end})
end

-- [82] L-DIRECTIVE 
function l_directive(inp)
    return seq(inp, {
        function(inp) return c_directive(inp) end,
        function(inp) return alt(inp, {
            function(inp) return ns_yaml_directive(inp) end,
            function(inp) return ns_tag_directive(inp) end,
            function(inp) return ns_reserved_directive(inp) end}) end,
        function(inp) return s_l_comments(inp) end})
end

-- [83] NS-RESERVED-DIRECTIVE 
function ns_reserved_directive(inp)
    return seq(inp, {
        function(inp) return ns_directive_name(inp) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return s_separate_in_line(inp) end,
            function(inp) return ns_directive_parameter(inp) end}) end) end})
end

-- [84] NS-DIRECTIVE-NAME 
function ns_directive_name(inp)
    return plus_(inp, function(inp) return ns_char(inp) end)
end

-- [85] NS-DIRECTIVE-PARAMETER 
function ns_directive_parameter(inp)
    return plus_(inp, function(inp) return ns_char(inp) end)
end

-- [86] NS-YAML-DIRECTIVE 
function ns_yaml_directive(inp)
    return seq(inp, {
        function(inp) return match_str(inp, "YAML") end,
        function(inp) return s_separate_in_line(inp) end,
        function(inp) return ns_yaml_version(inp) end})
end

-- [87] NS-YAML-VERSION 
function ns_yaml_version(inp)
    return seq(inp, {
        function(inp) return plus_(inp, function(inp) return ns_dec_digit(inp) end) end,
        function(inp) return match_cp(inp, 46) end,
        function(inp) return plus_(inp, function(inp) return ns_dec_digit(inp) end) end})
end

-- [88] NS-TAG-DIRECTIVE 
function ns_tag_directive(inp)
    return seq(inp, {
        function(inp) return match_str(inp, "TAG") end,
        function(inp) return s_separate_in_line(inp) end,
        function(inp) return c_tag_handle(inp) end,
        function(inp) return s_separate_in_line(inp) end,
        function(inp) return ns_tag_prefix(inp) end})
end

-- [89] C-TAG-HANDLE 
function c_tag_handle(inp)
    return alt(inp, {
        function(inp) return c_named_tag_handle(inp) end,
        function(inp) return c_secondary_tag_handle(inp) end,
        function(inp) return c_primary_tag_handle(inp) end})
end

-- [90] C-PRIMARY-TAG-HANDLE 
function c_primary_tag_handle(inp)
    return match_cp(inp, 33)
end

-- [91] C-SECONDARY-TAG-HANDLE 
function c_secondary_tag_handle(inp)
    return match_str(inp, "!!")
end

-- [92] C-NAMED-TAG-HANDLE 
function c_named_tag_handle(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 33) end,
        function(inp) return plus_(inp, function(inp) return ns_word_char(inp) end) end,
        function(inp) return match_cp(inp, 33) end})
end

-- [93] NS-TAG-PREFIX 
function ns_tag_prefix(inp)
    return alt(inp, {
        function(inp) return c_ns_local_tag_prefix(inp) end,
        function(inp) return ns_global_tag_prefix(inp) end})
end

-- [94] C-NS-LOCAL-TAG-PREFIX 
function c_ns_local_tag_prefix(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 33) end,
        function(inp) return star(inp, function(inp) return ns_uri_char(inp) end) end})
end

-- [95] NS-GLOBAL-TAG-PREFIX 
function ns_global_tag_prefix(inp)
    return seq(inp, {
        function(inp) return ns_tag_char(inp) end,
        function(inp) return star(inp, function(inp) return ns_uri_char(inp) end) end})
end

-- [96] C-NS-PROPERTIES 
function c_ns_properties(inp, n, c)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return c_ns_tag_property(inp) end,
            function(inp) return opt(inp, function(inp) return seq(inp, {
                function(inp) return s_separate(inp, n, c) end,
                function(inp) return c_ns_anchor_property(inp) end}) end) end}) end,
        function(inp) return seq(inp, {
            function(inp) return c_ns_anchor_property(inp) end,
            function(inp) return opt(inp, function(inp) return seq(inp, {
                function(inp) return s_separate(inp, n, c) end,
                function(inp) return c_ns_tag_property(inp) end}) end) end}) end})
end

-- [97] C-NS-TAG-PROPERTY 
function c_ns_tag_property(inp)
    return alt(inp, {
        function(inp) return c_verbatim_tag(inp) end,
        function(inp) return c_ns_shorthand_tag(inp) end,
        function(inp) return c_non_specific_tag(inp) end})
end

-- [98] C-VERBATIM-TAG 
function c_verbatim_tag(inp)
    return seq(inp, {
        function(inp) return match_str(inp, "!<") end,
        function(inp) return plus_(inp, function(inp) return ns_uri_char(inp) end) end,
        function(inp) return match_cp(inp, 62) end})
end

-- [99] C-NS-SHORTHAND-TAG 
function c_ns_shorthand_tag(inp)
    return seq(inp, {
        function(inp) return c_tag_handle(inp) end,
        function(inp) return plus_(inp, function(inp) return ns_tag_char(inp) end) end})
end

-- [100] C-NON-SPECIFIC-TAG 
function c_non_specific_tag(inp)
    return match_cp(inp, 33)
end

-- [101] C-NS-ANCHOR-PROPERTY 
function c_ns_anchor_property(inp)
    return build(inp, "ANCHOR", function(inp) return seq(inp, {
        function(inp) return c_anchor(inp) end,
        function(inp) return scalar(inp, function(inp) return ns_anchor_name(inp) end) end}) end)
end

-- [102] NS-ANCHOR-CHAR 
function ns_anchor_char(inp)
    return minus(inp, function(inp) return ns_char(inp) end, function(inp) return c_flow_indicator(inp) end)
end

-- [103] NS-ANCHOR-NAME 
function ns_anchor_name(inp)
    return plus_(inp, function(inp) return ns_anchor_char(inp) end)
end

-- [104] C-NS-ALIAS-NODE 
function c_ns_alias_node(inp)
    return build(inp, "ALIAS", function(inp) return seq(inp, {
        function(inp) return c_alias(inp) end,
        function(inp) return scalar(inp, function(inp) return ns_anchor_name(inp) end) end}) end)
end

-- [105] E-SCALAR 
function e_scalar(inp)
    return ok(inp)
end

-- [106] E-NODE 
function e_node(inp)
    return e_scalar(inp)
end

-- [107] NB-DOUBLE-CHAR 
function nb_double_char(inp)
    return alt(inp, {
        function(inp) return c_ns_esc_char(inp) end,
        function(inp) return minus(inp, function(inp) return nb_json(inp) end, function(inp) return alt(inp, {function(inp) return match_cp(inp, 92) end, function(inp) return match_cp(inp, 34) end}) end) end})
end

-- [108] NS-DOUBLE-CHAR 
function ns_double_char(inp)
    return minus(inp, function(inp) return nb_double_char(inp) end, function(inp) return s_white(inp) end)
end

-- [109] C-DOUBLE-QUOTED 
function c_double_quoted(inp, n, c)
    return scalar(inp, function(inp) return seq(inp, {
        function(inp) return match_cp(inp, 34) end,
        function(inp) return nb_double_text(inp, n, c) end,
        function(inp) return match_cp(inp, 34) end}) end)
end

-- [110] NB-DOUBLE-TEXT 
function nb_double_text(inp, n, c)
    return (function()
        if c == "FLOW-OUT" then return nb_double_multi_line(inp, n)
        elseif c == "FLOW-IN" then return nb_double_multi_line(inp, n)
        elseif c == "BLOCK-KEY" then return nb_double_one_line(inp)
        elseif c == "FLOW-KEY" then return nb_double_one_line(inp)
        else return fail(inp, "no case")
        end
    end)()
end

-- [111] NB-DOUBLE-ONE-LINE 
function nb_double_one_line(inp)
    return star(inp, function(inp) return nb_double_char(inp) end)
end

-- [112] S-DOUBLE-ESCAPED 
function s_double_escaped(inp, n)
    return seq(inp, {
        function(inp) return star(inp, function(inp) return s_white(inp) end) end,
        function(inp) return match_cp(inp, 92) end,
        function(inp) return b_non_content(inp) end,
        function(inp) return star(inp, function(inp) return l_empty(inp, n, "FLOW-IN") end) end,
        function(inp) return s_flow_line_prefix(inp, n) end})
end

-- [113] S-DOUBLE-BREAK 
function s_double_break(inp, n)
    return alt(inp, {
        function(inp) return s_double_escaped(inp, n) end,
        function(inp) return s_flow_folded(inp, n) end})
end

-- [114] NB-NS-DOUBLE-IN-LINE 
function nb_ns_double_in_line(inp)
    return star(inp, function(inp) return seq(inp, {
        function(inp) return star(inp, function(inp) return s_white(inp) end) end,
        function(inp) return ns_double_char(inp) end}) end)
end

-- [115] S-DOUBLE-NEXT-LINE 
function s_double_next_line(inp, n)
    return seq(inp, {
        function(inp) return s_double_break(inp, n) end,
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return ns_double_char(inp) end,
            function(inp) return nb_ns_double_in_line(inp) end,
            function(inp) return alt(inp, {
                function(inp) return s_double_next_line(inp, n) end,
                function(inp) return star(inp, function(inp) return s_white(inp) end) end}) end}) end) end})
end

-- [116] NB-DOUBLE-MULTI-LINE 
function nb_double_multi_line(inp, n)
    return seq(inp, {
        function(inp) return nb_ns_double_in_line(inp) end,
        function(inp) return alt(inp, {
            function(inp) return s_double_next_line(inp, n) end,
            function(inp) return star(inp, function(inp) return s_white(inp) end) end}) end})
end

-- [117] C-QUOTED-QUOTE 
function c_quoted_quote(inp)
    return match_str(inp, "''")
end

-- [118] NB-SINGLE-CHAR 
function nb_single_char(inp)
    return alt(inp, {
        function(inp) return c_quoted_quote(inp) end,
        function(inp) return minus(inp, function(inp) return nb_json(inp) end, function(inp) return match_cp(inp, 39) end) end})
end

-- [119] NS-SINGLE-CHAR 
function ns_single_char(inp)
    return minus(inp, function(inp) return nb_single_char(inp) end, function(inp) return s_white(inp) end)
end

-- [120] C-SINGLE-QUOTED 
function c_single_quoted(inp, n, c)
    return scalar(inp, function(inp) return seq(inp, {
        function(inp) return match_cp(inp, 39) end,
        function(inp) return nb_single_text(inp, n, c) end,
        function(inp) return match_cp(inp, 39) end}) end)
end

-- [121] NB-SINGLE-TEXT 
function nb_single_text(inp, n, c)
    return (function()
        if c == "FLOW-OUT" then return nb_single_multi_line(inp, n)
        elseif c == "FLOW-IN" then return nb_single_multi_line(inp, n)
        elseif c == "BLOCK-KEY" then return nb_single_one_line(inp)
        elseif c == "FLOW-KEY" then return nb_single_one_line(inp)
        else return fail(inp, "no case")
        end
    end)()
end

-- [122] NB-SINGLE-ONE-LINE 
function nb_single_one_line(inp)
    return star(inp, function(inp) return nb_single_char(inp) end)
end

-- [123] NS-SINGLE-IN-LINE 
function ns_single_in_line(inp)
    return star(inp, function(inp) return seq(inp, {
        function(inp) return star(inp, function(inp) return s_white(inp) end) end,
        function(inp) return ns_single_char(inp) end}) end)
end

-- [124] S-SINGLE-NEXT-LINE 
function s_single_next_line(inp, n)
    return seq(inp, {
        function(inp) return s_flow_folded(inp, n) end,
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return ns_single_char(inp) end,
            function(inp) return ns_single_in_line(inp) end,
            function(inp) return alt(inp, {
                function(inp) return s_single_next_line(inp, n) end,
                function(inp) return star(inp, function(inp) return s_white(inp) end) end}) end}) end) end})
end

-- [125] NB-SINGLE-MULTI-LINE 
function nb_single_multi_line(inp, n)
    return seq(inp, {
        function(inp) return ns_single_in_line(inp) end,
        function(inp) return alt(inp, {
            function(inp) return s_single_next_line(inp, n) end,
            function(inp) return star(inp, function(inp) return s_white(inp) end) end}) end})
end

-- [126] NS-PLAIN-FIRST 
function ns_plain_first(inp, c)
    return alt(inp, {
        function(inp) return minus(inp, function(inp) return ns_char(inp) end, function(inp) return c_indicator(inp) end) end,
        function(inp) return seq(inp, {
            function(inp) return alt(inp, {
                function(inp) return match_cp(inp, 63) end,
                function(inp) return match_cp(inp, 58) end,
                function(inp) return match_cp(inp, 45) end}) end,
            function(inp) return ahead(inp, function(inp) return ns_plain_safe(inp, c) end) end}) end})
end

-- [127] NS-PLAIN-SAFE 
function ns_plain_safe(inp, c)
    return (function()
        if c == "FLOW-OUT" then return ns_plain_safe_out(inp)
        elseif c == "FLOW-IN" then return ns_plain_safe_in(inp)
        elseif c == "BLOCK-KEY" then return ns_plain_safe_out(inp)
        elseif c == "FLOW-KEY" then return ns_plain_safe_in(inp)
        else return fail(inp, "no case")
        end
    end)()
end

-- [128] NS-PLAIN-SAFE-OUT 
function ns_plain_safe_out(inp)
    return ns_char(inp)
end

-- [129] NS-PLAIN-SAFE-IN 
function ns_plain_safe_in(inp)
    return minus(inp, function(inp) return ns_char(inp) end, function(inp) return c_flow_indicator(inp) end)
end

-- [130] NS-PLAIN-CHAR 
function ns_plain_char(inp, c)
    return alt(inp, {
        function(inp) return minus(inp, function(inp) return ns_plain_safe(inp, c) end, function(inp) return alt(inp, {function(inp) return match_cp(inp, 58) end, function(inp) return match_cp(inp, 35) end}) end) end,
        function(inp) return seq(inp, {
            function(inp) return behind(inp, function(inp) return ns_char(inp) end) end,
            function(inp) return match_cp(inp, 35) end}) end,
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 58) end,
            function(inp) return ahead(inp, function(inp) return ns_plain_safe(inp, c) end) end}) end})
end

-- [131] NS-PLAIN 
function ns_plain(inp, n, c)
    return scalar(inp, function(inp) return (function()
        if c == "FLOW-OUT" then return ns_plain_multi_line(inp, n, c)
        elseif c == "FLOW-IN" then return ns_plain_multi_line(inp, n, c)
        elseif c == "BLOCK-KEY" then return ns_plain_one_line(inp, c)
        elseif c == "FLOW-KEY" then return ns_plain_one_line(inp, c)
        else return fail(inp, "no case")
        end
    end)() end)
end

-- [132] NB-NS-PLAIN-IN-LINE 
function nb_ns_plain_in_line(inp, c)
    return star(inp, function(inp) return seq(inp, {
        function(inp) return star(inp, function(inp) return s_white(inp) end) end,
        function(inp) return ns_plain_char(inp, c) end}) end)
end

-- [133] NS-PLAIN-ONE-LINE 
function ns_plain_one_line(inp, c)
    return seq(inp, {
        function(inp) return ns_plain_first(inp, c) end,
        function(inp) return nb_ns_plain_in_line(inp, c) end})
end

-- [134] S-NS-PLAIN-NEXT-LINE 
function s_ns_plain_next_line(inp, n, c)
    return seq(inp, {
        function(inp) return s_flow_folded(inp, n) end,
        function(inp) return neg(inp, function(inp) return c_forbidden(inp) end) end,
        function(inp) return ns_plain_char(inp, c) end,
        function(inp) return nb_ns_plain_in_line(inp, c) end})
end

-- [135] NS-PLAIN-MULTI-LINE 
function ns_plain_multi_line(inp, n, c)
    return seq(inp, {
        function(inp) return ns_plain_one_line(inp, c) end,
        function(inp) return star(inp, function(inp) return s_ns_plain_next_line(inp, n, c) end) end})
end

-- [137] C-FLOW-SEQUENCE 
function c_flow_sequence(inp, n, c)
    return build(inp, "SEQUENCE", function(inp) return seq(inp, {
        function(inp) return match_cp(inp, 91) end,
        function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
        function(inp) return opt(inp, function(inp) return collect(inp, function(inp) return ns_s_flow_seq_entries(inp, n, inFlow(c)) end) end) end,
        function(inp) return match_cp(inp, 93) end}) end)
end

-- [138] NS-S-FLOW-SEQ-ENTRIES 
function ns_s_flow_seq_entries(inp, n, c)
    return seq(inp, {
        function(inp) return ns_flow_seq_entry(inp, n, c) end,
        function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 44) end,
            function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
            function(inp) return opt(inp, function(inp) return ns_s_flow_seq_entries(inp, n, c) end) end}) end) end})
end

-- [139] NS-FLOW-SEQ-ENTRY 
function ns_flow_seq_entry(inp, n, c)
    return alt(inp, {
        function(inp) return ns_flow_pair(inp, n, c) end,
        function(inp) return ns_flow_node(inp, n, c) end})
end

-- [140] C-FLOW-MAPPING 
function c_flow_mapping(inp, n, c)
    return build(inp, "MAPPING", function(inp) return seq(inp, {
        function(inp) return match_cp(inp, 123) end,
        function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
        function(inp) return opt(inp, function(inp) return collect(inp, function(inp) return ns_s_flow_map_entries(inp, n, inFlow(c)) end) end) end,
        function(inp) return match_cp(inp, 125) end}) end)
end

-- [141] NS-S-FLOW-MAP-ENTRIES 
function ns_s_flow_map_entries(inp, n, c)
    return seq(inp, {
        function(inp) return ns_flow_map_entry(inp, n, c) end,
        function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 44) end,
            function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
            function(inp) return opt(inp, function(inp) return ns_s_flow_map_entries(inp, n, c) end) end}) end) end})
end

-- [142] NS-FLOW-MAP-ENTRY 
function ns_flow_map_entry(inp, n, c)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 63) end,
            function(inp) return s_separate(inp, n, c) end,
            function(inp) return ns_flow_map_explicit_entry(inp, n, c) end}) end,
        function(inp) return ns_flow_map_implicit_entry(inp, n, c) end})
end

-- [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
function ns_flow_map_explicit_entry(inp, n, c)
    return alt(inp, {
        function(inp) return ns_flow_map_implicit_entry(inp, n, c) end,
        function(inp) return seq(inp, {function(inp) return e_node(inp) end, function(inp) return e_node(inp) end}) end})
end

-- [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
function ns_flow_map_implicit_entry(inp, n, c)
    return build(inp, "PAIR", function(inp) return alt(inp, {
        function(inp) return ns_flow_map_yaml_key_entry(inp, n, c) end,
        function(inp) return c_ns_flow_map_empty_key_entry(inp, n, c) end,
        function(inp) return c_ns_flow_map_json_key_entry(inp, n, c) end}) end)
end

-- [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
function ns_flow_map_yaml_key_entry(inp, n, c)
    return seq(inp, {
        function(inp) return ns_flow_yaml_node(inp, n, c) end,
        function(inp) return alt(inp, {
            function(inp) return seq(inp, {
                function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
                function(inp) return c_ns_flow_map_separate_value(inp, n, c) end}) end,
            function(inp) return e_node(inp) end}) end})
end

-- [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
function c_ns_flow_map_empty_key_entry(inp, n, c)
    return seq(inp, {
        function(inp) return e_node(inp) end,
        function(inp) return c_ns_flow_map_separate_value(inp, n, c) end})
end

-- [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
function c_ns_flow_map_separate_value(inp, n, c)
    return seq(inp, {
        function(inp) return match_cp(inp, 58) end,
        function(inp) return neg(inp, function(inp) return ns_plain_safe(inp, c) end) end,
        function(inp) return alt(inp, {
            function(inp) return seq(inp, {
                function(inp) return s_separate(inp, n, c) end,
                function(inp) return ns_flow_node(inp, n, c) end}) end,
            function(inp) return e_node(inp) end}) end})
end

-- [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
function c_ns_flow_map_json_key_entry(inp, n, c)
    return seq(inp, {
        function(inp) return c_flow_json_node(inp, n, c) end,
        function(inp) return alt(inp, {
            function(inp) return seq(inp, {
                function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
                function(inp) return c_ns_flow_map_adjacent_value(inp, n, c) end}) end,
            function(inp) return e_node(inp) end}) end})
end

-- [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
function c_ns_flow_map_adjacent_value(inp, n, c)
    return seq(inp, {
        function(inp) return match_cp(inp, 58) end,
        function(inp) return alt(inp, {
            function(inp) return seq(inp, {
                function(inp) return opt(inp, function(inp) return s_separate(inp, n, c) end) end,
                function(inp) return ns_flow_node(inp, n, c) end}) end,
            function(inp) return e_node(inp) end}) end})
end

-- [150] NS-FLOW-PAIR 
function ns_flow_pair(inp, n, c)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 63) end,
            function(inp) return s_separate(inp, n, c) end,
            function(inp) return ns_flow_map_explicit_entry(inp, n, c) end}) end,
        function(inp) return ns_flow_pair_entry(inp, n, c) end})
end

-- [151] NS-FLOW-PAIR-ENTRY 
function ns_flow_pair_entry(inp, n, c)
    return alt(inp, {
        function(inp) return ns_flow_pair_yaml_key_entry(inp, n, c) end,
        function(inp) return c_ns_flow_map_empty_key_entry(inp, n, c) end,
        function(inp) return c_ns_flow_pair_json_key_entry(inp, n, c) end})
end

-- [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
function ns_flow_pair_yaml_key_entry(inp, n, c)
    return seq(inp, {
        function(inp) return ns_s_implicit_yaml_key(inp, "FLOW-KEY") end,
        function(inp) return c_ns_flow_map_separate_value(inp, n, c) end})
end

-- [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
function c_ns_flow_pair_json_key_entry(inp, n, c)
    return seq(inp, {
        function(inp) return c_s_implicit_json_key(inp, "FLOW-KEY") end,
        function(inp) return c_ns_flow_map_adjacent_value(inp, n, c) end})
end

-- [154] NS-S-IMPLICIT-YAML-KEY 
function ns_s_implicit_yaml_key(inp, c)
    return seq(inp, {
        function(inp) return ns_flow_yaml_node(inp, 0, c) end,
        function(inp) return opt(inp, function(inp) return s_separate_in_line(inp) end) end})
end

-- [155] C-S-IMPLICIT-JSON-KEY 
function c_s_implicit_json_key(inp, c)
    return seq(inp, {
        function(inp) return c_flow_json_node(inp, 0, c) end,
        function(inp) return opt(inp, function(inp) return s_separate_in_line(inp) end) end})
end

-- [156] NS-FLOW-YAML-CONTENT 
function ns_flow_yaml_content(inp, n, c)
    return ns_plain(inp, n, c)
end

-- [157] C-FLOW-JSON-CONTENT 
function c_flow_json_content(inp, n, c)
    return alt(inp, {
        function(inp) return c_flow_sequence(inp, n, c) end,
        function(inp) return c_flow_mapping(inp, n, c) end,
        function(inp) return c_single_quoted(inp, n, c) end,
        function(inp) return c_double_quoted(inp, n, c) end})
end

-- [158] NS-FLOW-CONTENT 
function ns_flow_content(inp, n, c)
    return alt(inp, {
        function(inp) return ns_flow_yaml_content(inp, n, c) end,
        function(inp) return c_flow_json_content(inp, n, c) end})
end

-- [159] NS-FLOW-YAML-NODE 
function ns_flow_yaml_node(inp, n, c)
    return alt(inp, {
        function(inp) return c_ns_alias_node(inp) end,
        function(inp) return ns_flow_yaml_content(inp, n, c) end,
        function(inp) return seq(inp, {
            function(inp) return c_ns_properties(inp, n, c) end,
            function(inp) return alt(inp, {
                function(inp) return seq(inp, {
                    function(inp) return s_separate(inp, n, c) end,
                    function(inp) return ns_flow_yaml_content(inp, n, c) end}) end,
                function(inp) return e_scalar(inp) end}) end}) end})
end

-- [160] C-FLOW-JSON-NODE 
function c_flow_json_node(inp, n, c)
    return seq(inp, {
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return c_ns_properties(inp, n, c) end,
            function(inp) return s_separate(inp, n, c) end}) end) end,
        function(inp) return c_flow_json_content(inp, n, c) end})
end

-- [161] NS-FLOW-NODE 
function ns_flow_node(inp, n, c)
    return alt(inp, {
        function(inp) return c_ns_alias_node(inp) end,
        function(inp) return ns_flow_content(inp, n, c) end,
        function(inp) return seq(inp, {
            function(inp) return c_ns_properties(inp, n, c) end,
            function(inp) return alt(inp, {
                function(inp) return seq(inp, {
                    function(inp) return s_separate(inp, n, c) end,
                    function(inp) return ns_flow_content(inp, n, c) end}) end,
                function(inp) return e_scalar(inp) end}) end}) end})
end

-- [162] C-B-BLOCK-HEADER 
function c_b_block_header(inp, n)
    return alt(inp, {
        function(inp) return (function() local r = alt(inp, {
            function(inp) return parse_int(inp, function(inp) return ns_dec_digit(inp) end) end,
            function(inp) return detect_indent(inp, n) end}); if r.fail then return r end; local m = r.tagInt; local inp = r.rest; return (function() local r = alt(inp, {
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 45) end, "STRIP") end,
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 43) end, "KEEP") end,
            function(inp) return val(inp, "CLIP") end}); if r.fail then return r end; local t = r.tag; local inp = r.rest; return s_b_comment(inp) end)() end)() end,
        function(inp) return (function() local r = alt(inp, {
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 45) end, "STRIP") end,
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 43) end, "KEEP") end,
            function(inp) return val(inp, "CLIP") end}); if r.fail then return r end; local t = r.tag; local inp = r.rest; return (function() local r = alt(inp, {
            function(inp) return parse_int(inp, function(inp) return ns_dec_digit(inp) end) end,
            function(inp) return detect_indent(inp, n) end}); if r.fail then return r end; local m = r.tagInt; local inp = r.rest; return s_b_comment(inp) end)() end)() end})
end

-- [163] C-INDENTATION-INDICATOR 
function c_indentation_indicator(inp, n)
    return alt(inp, {function(inp) return ns_dec_digit(inp) end, function(inp) return ok(inp) end})
end

-- [164] C-CHOMPING-INDICATOR 
function c_chomping_indicator(inp)
    return alt(inp, {
        function(inp) return match_cp(inp, 45) end,
        function(inp) return match_cp(inp, 43) end,
        function(inp) return ok(inp) end})
end

-- [165] B-CHOMPED-LAST 
function b_chomped_last(inp, t)
    return (function()
        if t == "STRIP" then return b_non_content(inp)
        elseif t == "CLIP" then return b_as_line_feed(inp)
        elseif t == "KEEP" then return b_as_line_feed(inp)
        else return fail(inp, "no case")
        end
    end)()
end

-- [166] L-CHOMPED-EMPTY 
function l_chomped_empty(inp, n, t)
    return (function()
        if t == "STRIP" then return l_strip_empty(inp, n)
        elseif t == "CLIP" then return l_strip_empty(inp, n)
        elseif t == "KEEP" then return l_keep_empty(inp, n)
        else return fail(inp, "no case")
        end
    end)()
end

-- [167] L-STRIP-EMPTY 
function l_strip_empty(inp, n)
    return seq(inp, {
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return s_indent_le(inp, n) end,
            function(inp) return b_non_content(inp) end}) end) end,
        function(inp) return opt(inp, function(inp) return l_trail_comments(inp, n) end) end})
end

-- [168] L-KEEP-EMPTY 
function l_keep_empty(inp, n)
    return seq(inp, {
        function(inp) return star(inp, function(inp) return l_empty(inp, n, "BLOCK-IN") end) end,
        function(inp) return opt(inp, function(inp) return l_trail_comments(inp, n) end) end})
end

-- [169] L-TRAIL-COMMENTS 
function l_trail_comments(inp, n)
    return seq(inp, {
        function(inp) return s_indent_lt(inp, n) end,
        function(inp) return c_nb_comment_text(inp) end,
        function(inp) return b_comment(inp) end,
        function(inp) return star(inp, function(inp) return l_comment(inp) end) end})
end

-- [170] C-L+LITERAL 
function c_lliteral(inp, n)
    return seq(inp, {
        function(inp) return match_cp(inp, 124) end,
        function(inp) return (function() local r = alt(inp, {
            function(inp) return parse_int(inp, function(inp) return ns_dec_digit(inp) end) end,
            function(inp) return detect_indent(inp, n) end}); if r.fail then return r end; local m = r.tagInt; local inp = r.rest; return (function() local r = alt(inp, {
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 45) end, "STRIP") end,
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 43) end, "KEEP") end,
            function(inp) return val(inp, "CLIP") end}); if r.fail then return r end; local t = r.tag; local inp = r.rest; return seq(inp, {
            function(inp) return s_b_comment(inp) end,
            function(inp) return l_literal_content(inp, (n + m), t) end}) end)() end)() end})
end

-- [171] L-NB-LITERAL-TEXT 
function l_nb_literal_text(inp, n)
    return seq(inp, {
        function(inp) return star(inp, function(inp) return l_empty(inp, n, "BLOCK-IN") end) end,
        function(inp) return s_indent(inp, n) end,
        function(inp) return plus_(inp, function(inp) return nb_char(inp) end) end})
end

-- [172] B-NB-LITERAL-NEXT 
function b_nb_literal_next(inp, n)
    return seq(inp, {
        function(inp) return b_as_line_feed(inp) end,
        function(inp) return l_nb_literal_text(inp, n) end})
end

-- [173] L-LITERAL-CONTENT 
function l_literal_content(inp, n, t)
    return scalar(inp, function(inp) return seq(inp, {
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return l_nb_literal_text(inp, n) end,
            function(inp) return star(inp, function(inp) return b_nb_literal_next(inp, n) end) end,
            function(inp) return b_chomped_last(inp, t) end}) end) end,
        function(inp) return l_chomped_empty(inp, n, t) end}) end)
end

-- [174] C-L+FOLDED 
function c_lfolded(inp, n)
    return seq(inp, {
        function(inp) return match_cp(inp, 62) end,
        function(inp) return (function() local r = alt(inp, {
            function(inp) return parse_int(inp, function(inp) return ns_dec_digit(inp) end) end,
            function(inp) return detect_indent(inp, n) end}); if r.fail then return r end; local m = r.tagInt; local inp = r.rest; return (function() local r = alt(inp, {
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 45) end, "STRIP") end,
            function(inp) return parse_sym(inp, function(inp) return match_cp(inp, 43) end, "KEEP") end,
            function(inp) return val(inp, "CLIP") end}); if r.fail then return r end; local t = r.tag; local inp = r.rest; return seq(inp, {
            function(inp) return s_b_comment(inp) end,
            function(inp) return l_folded_content(inp, (n + m), t) end}) end)() end)() end})
end

-- [175] S-NB-FOLDED-TEXT 
function s_nb_folded_text(inp, n)
    return seq(inp, {
        function(inp) return s_indent(inp, n) end,
        function(inp) return ns_char(inp) end,
        function(inp) return star(inp, function(inp) return nb_char(inp) end) end})
end

-- [176] L-NB-FOLDED-LINES 
function l_nb_folded_lines(inp, n)
    return seq(inp, {
        function(inp) return s_nb_folded_text(inp, n) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return b_l_folded(inp, n, "BLOCK-IN") end,
            function(inp) return s_nb_folded_text(inp, n) end}) end) end})
end

-- [177] S-NB-SPACED-TEXT 
function s_nb_spaced_text(inp, n)
    return seq(inp, {
        function(inp) return s_indent(inp, n) end,
        function(inp) return s_white(inp) end,
        function(inp) return star(inp, function(inp) return nb_char(inp) end) end})
end

-- [178] B-L-SPACED 
function b_l_spaced(inp, n)
    return seq(inp, {
        function(inp) return b_as_line_feed(inp) end,
        function(inp) return star(inp, function(inp) return l_empty(inp, n, "BLOCK-IN") end) end})
end

-- [179] L-NB-SPACED-LINES 
function l_nb_spaced_lines(inp, n)
    return seq(inp, {
        function(inp) return s_nb_spaced_text(inp, n) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return b_l_spaced(inp, n) end,
            function(inp) return s_nb_spaced_text(inp, n) end}) end) end})
end

-- [180] L-NB-SAME-LINES 
function l_nb_same_lines(inp, n)
    return seq(inp, {
        function(inp) return star(inp, function(inp) return l_empty(inp, n, "BLOCK-IN") end) end,
        function(inp) return alt(inp, {
            function(inp) return l_nb_folded_lines(inp, n) end,
            function(inp) return l_nb_spaced_lines(inp, n) end}) end})
end

-- [181] L-NB-DIFF-LINES 
function l_nb_diff_lines(inp, n)
    return seq(inp, {
        function(inp) return l_nb_same_lines(inp, n) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return b_as_line_feed(inp) end,
            function(inp) return l_nb_same_lines(inp, n) end}) end) end})
end

-- [182] L-FOLDED-CONTENT 
function l_folded_content(inp, n, t)
    return scalar(inp, function(inp) return seq(inp, {
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return l_nb_diff_lines(inp, n) end,
            function(inp) return b_chomped_last(inp, t) end}) end) end,
        function(inp) return l_chomped_empty(inp, n, t) end}) end)
end

-- [183] L+BLOCK-SEQUENCE 
function lblock_sequence(inp, n)
    return build(inp, "SEQUENCE", function(inp) return (function() local r = detect_indent(inp, n); if r.fail then return r end; local m = r.tagInt; local inp = r.rest; return collect(inp, function(inp) return plus_(inp, function(inp) return seq(inp, {
        function(inp) return s_indent(inp, (n + m)) end,
        function(inp) return c_l_block_seq_entry(inp, (n + m)) end}) end) end) end)() end)
end

-- [184] C-L-BLOCK-SEQ-ENTRY 
function c_l_block_seq_entry(inp, n)
    return seq(inp, {
        function(inp) return match_cp(inp, 45) end,
        function(inp) return neg(inp, function(inp) return ns_char(inp) end) end,
        function(inp) return s_lblock_indented(inp, n, "BLOCK-IN") end})
end

-- [185] S-L+BLOCK-INDENTED 
function s_lblock_indented(inp, n, c)
    return alt(inp, {
        function(inp) return (function() local r = detect_indent(inp, 0); if r.fail then return r end; local m = r.tagInt; local inp = r.rest; return seq(inp, {
            function(inp) return s_indent(inp, m) end,
            function(inp) return alt(inp, {
                function(inp) return ns_l_compact_sequence(inp, (n + 1 + m)) end,
                function(inp) return ns_l_compact_mapping(inp, (n + 1 + m)) end}) end}) end)() end,
        function(inp) return s_lblock_node(inp, n, c) end,
        function(inp) return seq(inp, {function(inp) return e_node(inp) end, function(inp) return s_l_comments(inp) end}) end})
end

-- [186] NS-L-COMPACT-SEQUENCE 
function ns_l_compact_sequence(inp, n)
    return seq(inp, {
        function(inp) return c_l_block_seq_entry(inp, n) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return s_indent(inp, n) end,
            function(inp) return c_l_block_seq_entry(inp, n) end}) end) end})
end

-- [187] L+BLOCK-MAPPING 
function lblock_mapping(inp, n)
    return build(inp, "MAPPING", function(inp) return (function() local r = detect_indent(inp, n); if r.fail then return r end; local m = r.tagInt; local inp = r.rest; return collect(inp, function(inp) return plus_(inp, function(inp) return seq(inp, {
        function(inp) return s_indent(inp, (n + m)) end,
        function(inp) return ns_l_block_map_entry(inp, (n + m)) end}) end) end) end)() end)
end

-- [188] NS-L-BLOCK-MAP-ENTRY 
function ns_l_block_map_entry(inp, n)
    return alt(inp, {
        function(inp) return c_l_block_map_explicit_entry(inp, n) end,
        function(inp) return ns_l_block_map_implicit_entry(inp, n) end})
end

-- [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
function c_l_block_map_explicit_entry(inp, n)
    return seq(inp, {
        function(inp) return c_l_block_map_explicit_key(inp, n) end,
        function(inp) return alt(inp, {
            function(inp) return l_block_map_explicit_value(inp, n) end,
            function(inp) return e_node(inp) end}) end})
end

-- [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
function c_l_block_map_explicit_key(inp, n)
    return seq(inp, {
        function(inp) return match_cp(inp, 63) end,
        function(inp) return s_lblock_indented(inp, n, "BLOCK-OUT") end})
end

-- [191] L-BLOCK-MAP-EXPLICIT-VALUE 
function l_block_map_explicit_value(inp, n)
    return seq(inp, {
        function(inp) return s_indent(inp, n) end,
        function(inp) return match_cp(inp, 58) end,
        function(inp) return s_lblock_indented(inp, n, "BLOCK-OUT") end})
end

-- [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
function ns_l_block_map_implicit_entry(inp, n)
    return build(inp, "PAIR", function(inp) return seq(inp, {
        function(inp) return scalar(inp, function(inp) return alt(inp, {
            function(inp) return ns_s_block_map_implicit_key(inp) end,
            function(inp) return e_node(inp) end}) end) end,
        function(inp) return c_l_block_map_implicit_value(inp, n) end}) end)
end

-- [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
function ns_s_block_map_implicit_key(inp)
    return alt(inp, {
        function(inp) return c_s_implicit_json_key(inp, "BLOCK-KEY") end,
        function(inp) return ns_s_implicit_yaml_key(inp, "BLOCK-KEY") end})
end

-- [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
function c_l_block_map_implicit_value(inp, n)
    return seq(inp, {
        function(inp) return match_cp(inp, 58) end,
        function(inp) return alt(inp, {
            function(inp) return s_lblock_node(inp, n, "BLOCK-OUT") end,
            function(inp) return scalar(inp, function(inp) return seq(inp, {function(inp) return e_node(inp) end, function(inp) return s_l_comments(inp) end}) end) end}) end})
end

-- [195] NS-L-COMPACT-MAPPING 
function ns_l_compact_mapping(inp, n)
    return seq(inp, {
        function(inp) return ns_l_block_map_entry(inp, n) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return s_indent(inp, n) end,
            function(inp) return ns_l_block_map_entry(inp, n) end}) end) end})
end

-- [196] S-L+BLOCK-NODE 
function s_lblock_node(inp, n, c)
    return alt(inp, {
        function(inp) return s_lblock_in_block(inp, n, c) end,
        function(inp) return s_lflow_in_block(inp, n) end})
end

-- [197] S-L+FLOW-IN-BLOCK 
function s_lflow_in_block(inp, n)
    return seq(inp, {
        function(inp) return s_separate(inp, (n + 1), "FLOW-OUT") end,
        function(inp) return ns_flow_node(inp, (n + 1), "FLOW-OUT") end,
        function(inp) return s_l_comments(inp) end})
end

-- [198] S-L+BLOCK-IN-BLOCK 
function s_lblock_in_block(inp, n, c)
    return alt(inp, {
        function(inp) return s_lblock_scalar(inp, n, c) end,
        function(inp) return s_lblock_collection(inp, n, c) end})
end

-- [199] S-L+BLOCK-SCALAR 
function s_lblock_scalar(inp, n, c)
    return seq(inp, {
        function(inp) return s_separate(inp, (n + 1), c) end,
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return c_ns_properties(inp, (n + 1), c) end,
            function(inp) return s_separate(inp, (n + 1), c) end}) end) end,
        function(inp) return alt(inp, {function(inp) return c_lliteral(inp, n) end, function(inp) return c_lfolded(inp, n) end}) end})
end

-- [200] S-L+BLOCK-COLLECTION 
function s_lblock_collection(inp, n, c)
    return seq(inp, {
        function(inp) return opt(inp, function(inp) return seq(inp, {
            function(inp) return s_separate(inp, (n + 1), c) end,
            function(inp) return c_ns_properties(inp, (n + 1), c) end}) end) end,
        function(inp) return s_l_comments(inp) end,
        function(inp) return alt(inp, {
            function(inp) return lblock_sequence(inp, seqSpaces(n, c)) end,
            function(inp) return lblock_mapping(inp, n) end}) end})
end

-- [202] L-DOCUMENT-PREFIX 
function l_document_prefix(inp)
    return seq(inp, {
        function(inp) return opt(inp, function(inp) return c_byte_order_mark(inp) end) end,
        function(inp) return star(inp, function(inp) return l_comment(inp) end) end})
end

-- [203] C-DIRECTIVES-END 
function c_directives_end(inp)
    return match_str(inp, "---")
end

-- [204] C-DOCUMENT-END 
function c_document_end(inp)
    return match_str(inp, "...")
end

-- [205] L-DOCUMENT-SUFFIX 
function l_document_suffix(inp)
    return seq(inp, {function(inp) return c_document_end(inp) end, function(inp) return s_l_comments(inp) end})
end

-- [206] C-FORBIDDEN 
function c_forbidden(inp)
    return seq(inp, {
        function(inp) return sol(inp) end,
        function(inp) return alt(inp, {
            function(inp) return c_directives_end(inp) end,
            function(inp) return c_document_end(inp) end}) end,
        function(inp) return alt(inp, {
            function(inp) return b_char(inp) end,
            function(inp) return s_white(inp) end,
            function(inp) return eof_ok(inp) end}) end})
end

-- [207] L-BARE-DOCUMENT 
function l_bare_document(inp)
    return build(inp, "DOC", function(inp) return s_lblock_node(inp, -1, "BLOCK-IN") end)
end

-- [208] L-EXPLICIT-DOCUMENT 
function l_explicit_document(inp)
    return build(inp, "DOC", function(inp) return seq(inp, {
        function(inp) return c_directives_end(inp) end,
        function(inp) return alt(inp, {
            function(inp) return l_bare_document(inp) end,
            function(inp) return seq(inp, {function(inp) return e_node(inp) end, function(inp) return s_l_comments(inp) end}) end}) end}) end)
end

-- [209] L-DIRECTIVE-DOCUMENT 
function l_directive_document(inp)
    return seq(inp, {
        function(inp) return plus_(inp, function(inp) return l_directive(inp) end) end,
        function(inp) return l_explicit_document(inp) end})
end

-- [210] L-ANY-DOCUMENT 
function l_any_document(inp)
    return alt(inp, {
        function(inp) return l_directive_document(inp) end,
        function(inp) return l_explicit_document(inp) end,
        function(inp) return l_bare_document(inp) end})
end

-- [211] L-YAML-STREAM 
function l_yaml_stream(inp)
    return build(inp, "STREAM", function(inp) return seq(inp, {
        function(inp) return star(inp, function(inp) return l_document_prefix(inp) end) end,
        function(inp) return opt(inp, function(inp) return l_any_document(inp) end) end,
        function(inp) return star(inp, function(inp) return alt(inp, {
            function(inp) return seq(inp, {
                function(inp) return plus_(inp, function(inp) return l_document_suffix(inp) end) end,
                function(inp) return star(inp, function(inp) return l_document_prefix(inp) end) end,
                function(inp) return opt(inp, function(inp) return l_any_document(inp) end) end}) end,
            function(inp) return seq(inp, {
                function(inp) return star(inp, function(inp) return l_document_prefix(inp) end) end,
                function(inp) return opt(inp, function(inp) return l_explicit_document(inp) end) end}) end}) end) end}) end)
end

-- ── API ──

local function printAst(node, depth)
    local indent = string.rep("  ", depth)
    if node.isLeaf then
        print(indent .. 'SCALAR: "' .. node.text .. '"')
    else
        print(indent .. node.tag)
        for _, c in ipairs(node.children) do printAst(c, depth + 1) end
    end
end

-- ── Main ──

local text
if arg[1] then
    local f = io.open(arg[1], "r")
    text = f:read("*a"); f:close()
else
    text = io.read("*a")
end
local inp = newInput(text)
local r = l_yaml_stream(inp)
if not r.fail then
    print("OK: " .. (r.rest.pos - 1) .. " chars")
    if r.ast then printAst(r.ast, 0) end
else
    io.stderr:write("FAIL @" .. r.rest.pos .. ": " .. (r.err or "") .. "\n")
    os.exit(1)
end
