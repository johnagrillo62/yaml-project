;;;; peg-lua.lisp — Lua target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Lua")
(def-tgt "default-output" "yaml_reader.lua")

(def-tgt "keywords"
  '("and" "break" "do" "else" "elseif" "end" "false" "for" "function"
    "goto" "if" "in" "local" "nil" "not" "or" "repeat" "return"
    "then" "true" "until" "while"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "function(inp) return ~A end" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "function(inp) return ~A end" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "seq(inp, {~{~A~^, ~}})" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "alt(inp, {~{~A~^, ~}})" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (with-output-to-string (s)
      (format s "(function()~%")
      (loop for (val body) in cases
            for first = t then nil
            do (if first
                   (format s "        if ~A == ~S then return ~A~%" param val body)
                   (format s "        elseif ~A == ~S then return ~A~%" param val body)))
      (format s "        else return fail(inp, \"no case\")~%")
      (format s "        end~%    end)()"))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "(function() local r = ~A; if r.fail then return r end; local ~A = r.tagInt; local inp = r.rest; return ~A end)()"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "(function() local r = ~A; if r.fail then return r end; local ~A = r.tag; local inp = r.rest; return ~A end)()"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) name))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "inFlow(~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "seqSpaces(~A, ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(inp~{, ~A~})" name
                (mapcar #'peg-ident params))
        (format nil "~A(inp)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "function ~A~%    return ~A~%end" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"-- ════════════════════════════════════════════════════════════════
-- yaml_reader.lua — YAML 1.2 parser
-- ════════════════════════════════════════════════════════════════")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"-- ── Input ──

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
end"

"-- ── AST ──

local function makeAst(tag) return { tag = tag, children = {}, isLeaf = false } end
local function leafAst(text) return { text = text, isLeaf = true } end"

"-- ── Result ──

local function ok(inp) return { fail = false, val = \"\", rest = inp, tag = \"\", tagInt = 0 } end
local function okV(inp, v) return { fail = false, val = v, rest = inp, tag = \"\", tagInt = 0 } end
local function fail(inp, msg) return { fail = true, val = \"\", rest = inp, err = msg, tag = \"\", tagInt = 0 } end"

"-- ── Context ──

local function inFlow(c)
    if c == \"FLOW-OUT\" or c == \"FLOW-IN\" then return \"FLOW-IN\" end
    return \"FLOW-KEY\"
end

local function seqSpaces(n, c)
    if c == \"BLOCK-OUT\" then return n - 1 end
    return n
end"

"-- ── Combinators ──

local function match_cp(inp, cp)
    local c = peekCp(inp)
    if c == cp then
        local s = string.char(c)
        return okV(adv(inp), s)
    end
    return fail(inp, \"cp\")
end

local function match_range(inp, lo, hi)
    local c = peekCp(inp)
    if c >= lo and c <= hi then
        local s = string.char(c)
        return okV(adv(inp), s)
    end
    return fail(inp, \"rng\")
end

local function match_str(inp, t)
    local n = #t
    if inp.pos + n - 1 > #inp.src then return fail(inp, \"str\") end
    if string.sub(inp.src, inp.pos, inp.pos + n - 1) ~= t then return fail(inp, \"str\") end
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
    return fail(inp, \"alt\")
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
local function neg(inp, f) local r = f(inp); if r.fail then return ok(inp) end; return fail(inp, \"neg\") end
local function minus(inp, fa, fb)
    local ra = fa(inp); if ra.fail then return ra end
    local rb = fb(inp); if not rb.fail and rb.rest.pos == ra.rest.pos then return fail(inp, \"excl\") end; return ra
end
local function rep(inp, n, f)
    local cur = inp; local acc = {}
    for i = 1, n do local r = f(cur); if r.fail then return r end; table.insert(acc, r.val); cur = r.rest end
    return okV(cur, table.concat(acc))
end
local function ahead(inp, f) local r = f(inp); if r.fail then return r end; return ok(inp) end
local function behind(inp, f)
    if inp.pos <= 1 then return fail(inp, \"bh\") end
    local t = { src = inp.src, pos = inp.pos - 1, line = inp.line, col = math.max(0, inp.col - 1) }
    local r = f(t); if r.fail then return fail(inp, \"bh\") end; return ok(inp)
end
local function sol(inp) if inp.col == 0 then return ok(inp) end; return fail(inp, \"sol\") end
local function eof_ok(inp) if atEof(inp) then return ok(inp) end; return fail(inp, \"eof\") end"

"-- ── YAML extensions ──

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

local function val(inp, v) local r = ok(inp); r.tag = v; return r end"
))

;;; ── Comment prefix ──

(def-tgt "comment-prefix" "--")

;;; ── API ──

(def-tgt "api"
"-- ── API ──

local function printAst(node, depth)
    local indent = string.rep(\"  \", depth)
    if node.isLeaf then
        print(indent .. 'SCALAR: \"' .. node.text .. '\"')
    else
        print(indent .. node.tag)
        for _, c in ipairs(node.children) do printAst(c, depth + 1) end
    end
end")

;;; ── Main ──

(def-tgt "main-fn"
"-- ── Main ──

local text
if arg[1] then
    local f = io.open(arg[1], \"r\")
    text = f:read(\"*a\"); f:close()
else
    text = io.read(\"*a\")
end
local inp = newInput(text)
local r = l_yaml_stream(inp)
if not r.fail then
    print(\"OK: \" .. (r.rest.pos - 1) .. \" chars\")
    if r.ast then printAst(r.ast, 0) end
else
    io.stderr:write(\"FAIL @\" .. r.rest.pos .. \": \" .. (r.err or \"\") .. \"\\n\")
    os.exit(1)
end")

(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)
