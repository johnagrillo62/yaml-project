-- ════════════════════════════════════════════════════════════════
-- json_reader.lua — JSON (RFC 8259) parser
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

-- ════════════════════════════════════════════════════════════════ 
-- YAML 1.2 Grammar — 211 rules 
-- ════════════════════════════════════════════════════════════════ 

-- [1] JSON-TEXT 
function json_text(inp)
    return seq(inp, {
        function(inp) return ws(inp) end,
        function(inp) return value(inp) end,
        function(inp) return ws(inp) end,
        function(inp) return eof_ok(inp) end})
end

-- [2] VALUE 
function value(inp)
    return alt(inp, {
        function(inp) return object(inp) end,
        function(inp) return array(inp) end,
        function(inp) return r_string(inp) end,
        function(inp) return number(inp) end,
        function(inp) return match_str(inp, "true") end,
        function(inp) return match_str(inp, "false") end,
        function(inp) return match_str(inp, "null") end})
end

-- [3] OBJECT 
function object(inp)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 123) end,
            function(inp) return ws(inp) end,
            function(inp) return members(inp) end,
            function(inp) return ws(inp) end,
            function(inp) return match_cp(inp, 125) end}) end,
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 123) end,
            function(inp) return ws(inp) end,
            function(inp) return match_cp(inp, 125) end}) end})
end

-- [4] MEMBERS 
function members(inp)
    return seq(inp, {
        function(inp) return member(inp) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return ws(inp) end,
            function(inp) return match_cp(inp, 44) end,
            function(inp) return ws(inp) end,
            function(inp) return member(inp) end}) end) end})
end

-- [5] MEMBER 
function member(inp)
    return seq(inp, {
        function(inp) return ws(inp) end,
        function(inp) return r_string(inp) end,
        function(inp) return ws(inp) end,
        function(inp) return match_cp(inp, 58) end,
        function(inp) return ws(inp) end,
        function(inp) return value(inp) end,
        function(inp) return ws(inp) end})
end

-- [6] ARRAY 
function array(inp)
    return alt(inp, {
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 91) end,
            function(inp) return ws(inp) end,
            function(inp) return elements(inp) end,
            function(inp) return ws(inp) end,
            function(inp) return match_cp(inp, 93) end}) end,
        function(inp) return seq(inp, {
            function(inp) return match_cp(inp, 91) end,
            function(inp) return ws(inp) end,
            function(inp) return match_cp(inp, 93) end}) end})
end

-- [7] ELEMENTS 
function elements(inp)
    return seq(inp, {
        function(inp) return value(inp) end,
        function(inp) return star(inp, function(inp) return seq(inp, {
            function(inp) return ws(inp) end,
            function(inp) return match_cp(inp, 44) end,
            function(inp) return ws(inp) end,
            function(inp) return value(inp) end}) end) end})
end

-- [8] STRING 
function r_string(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 34) end,
        function(inp) return star(inp, function(inp) return char(inp) end) end,
        function(inp) return match_cp(inp, 34) end})
end

-- [9] CHAR 
function char(inp)
    return alt(inp, {
        function(inp) return escaped(inp) end,
        function(inp) return seq(inp, {
            function(inp) return neg(inp, function(inp) return match_cp(inp, 34) end) end,
            function(inp) return neg(inp, function(inp) return match_cp(inp, 92) end) end,
            function(inp) return neg(inp, function(inp) return match_cp(inp, 0x0) end) end,
            function(inp) return neg(inp, function(inp) return match_range(inp, 0x0, 0x1F) end) end,
            function(inp) return match_range(inp, 0x20, 0x10FFFF) end}) end})
end

-- [10] ESCAPED 
function escaped(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 92) end,
        function(inp) return alt(inp, {
            function(inp) return match_cp(inp, 34) end,
            function(inp) return match_cp(inp, 92) end,
            function(inp) return match_cp(inp, 47) end,
            function(inp) return match_cp(inp, 98) end,
            function(inp) return match_cp(inp, 102) end,
            function(inp) return match_cp(inp, 110) end,
            function(inp) return match_cp(inp, 114) end,
            function(inp) return match_cp(inp, 116) end,
            function(inp) return seq(inp, {function(inp) return match_cp(inp, 117) end, function(inp) return hex4(inp) end}) end}) end})
end

-- [11] HEX4 
function hex4(inp)
    return seq(inp, {
        function(inp) return hexdig(inp) end,
        function(inp) return hexdig(inp) end,
        function(inp) return hexdig(inp) end,
        function(inp) return hexdig(inp) end})
end

-- [12] HEXDIG 
function hexdig(inp)
    return alt(inp, {
        function(inp) return match_range(inp, 48, 57) end,
        function(inp) return match_range(inp, 97, 102) end,
        function(inp) return match_range(inp, 65, 70) end})
end

-- [13] NUMBER 
function number(inp)
    return seq(inp, {
        function(inp) return opt(inp, function(inp) return match_cp(inp, 45) end) end,
        function(inp) return integer(inp) end,
        function(inp) return opt(inp, function(inp) return fraction(inp) end) end,
        function(inp) return opt(inp, function(inp) return exponent(inp) end) end})
end

-- [14] INTEGER 
function integer(inp)
    return alt(inp, {
        function(inp) return match_cp(inp, 48) end,
        function(inp) return seq(inp, {
            function(inp) return match_range(inp, 49, 57) end,
            function(inp) return star(inp, function(inp) return match_range(inp, 48, 57) end) end}) end})
end

-- [15] FRACTION 
function fraction(inp)
    return seq(inp, {
        function(inp) return match_cp(inp, 46) end,
        function(inp) return plus_(inp, function(inp) return match_range(inp, 48, 57) end) end})
end

-- [16] EXPONENT 
function exponent(inp)
    return seq(inp, {
        function(inp) return alt(inp, {function(inp) return match_cp(inp, 101) end, function(inp) return match_cp(inp, 69) end}) end,
        function(inp) return opt(inp, function(inp) return alt(inp, {function(inp) return match_cp(inp, 43) end, function(inp) return match_cp(inp, 45) end}) end) end,
        function(inp) return plus_(inp, function(inp) return match_range(inp, 48, 57) end) end})
end

-- [17] WS 
function ws(inp)
    return star(inp, function(inp) return alt(inp, {
        function(inp) return match_cp(inp, 0x20) end,
        function(inp) return match_cp(inp, 0x9) end,
        function(inp) return match_cp(inp, 0x0A) end,
        function(inp) return match_cp(inp, 0x0D) end}) end)
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
local r = json_text(inp)
if not r.fail then
    print("OK: " .. (r.rest.pos - 1) .. " chars")
    if r.ast then printAst(r.ast, 0) end
else
    io.stderr:write("FAIL @" .. r.rest.pos .. ": " .. (r.err or "") .. "\n")
    os.exit(1)
end
