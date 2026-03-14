# ════════════════════════════════════════════════════════════════
# json_reader.ps1 — JSON (RFC 8259) parser
# ════════════════════════════════════════════════════════════════
$ErrorActionPreference = 'Stop'

# ── Input ──

class Inp {
    [string]$src
    [int]$pos
    [int]$line
    [int]$col
    Inp([string]$s) { $this.src = $s; $this.pos = 0; $this.line = 1; $this.col = 0 }
    Inp([string]$s, [int]$p, [int]$l, [int]$c) { $this.src = $s; $this.pos = $p; $this.line = $l; $this.col = $c }
    [bool] atEof() { return $this.pos -ge $this.src.Length }
    [int] peek() { if ($this.atEof()) { return -1 }; return [int]$this.src[$this.pos] }
    [Inp] adv() {
        if ($this.atEof()) { return $this }
        $c = $this.src[$this.pos]
        $nl = $c -eq "`n"
        return [Inp]::new($this.src, $this.pos + 1, $(if ($nl) { $this.line + 1 } else { $this.line }), $(if ($nl) { 0 } else { $this.col + 1 }))
    }
}

# ── AST ──

class Ast {
    [string]$tag
    [string]$text
    [System.Collections.Generic.List[Ast]]$children
    [bool]$isLeaf
    Ast([string]$tag) { $this.tag = $tag; $this.text = ''; $this.children = [System.Collections.Generic.List[Ast]]::new(); $this.isLeaf = $false }
    static [Ast] Leaf([string]$text) { $a = [Ast]::new(''); $a.text = $text; $a.isLeaf = $true; return $a }
}

# ── Result ──

class Res {
    [bool]$fail = $false
    [string]$val = ''
    [Inp]$rest
    [string]$tag = ''
    [int]$tagInt = 0
    [Ast]$ast = $null
    [System.Collections.Generic.List[Ast]]$astList = $null
    [string]$err = ''
}

function ok($inp) { $r = [Res]::new(); $r.rest = $inp; return $r }
function okV($inp, $v) { $r = [Res]::new(); $r.val = $v; $r.rest = $inp; return $r }
function fail($inp, $msg) { $r = [Res]::new(); $r.fail = $true; $r.rest = $inp; $r.err = $msg; return $r }

# ── Context ──

function inFlow($c) { if ($c -eq 'FLOW-OUT' -or $c -eq 'FLOW-IN') { return 'FLOW-IN' }; return 'FLOW-KEY' }
function seqSpaces($n, $c) { if ($c -eq 'BLOCK-OUT') { return $n - 1 }; return $n }

# ── Combinators ──

function match_cp($inp, $cp) {
    $c = $inp.peek()
    if ($c -eq $cp) { $s = [char]$c; return okV $inp.adv() $s }
    return fail $inp 'cp'
}

function match_range($inp, $lo, $hi) {
    $c = $inp.peek()
    if ($c -ge $lo -and $c -le $hi) { $s = [char]$c; return okV $inp.adv() $s }
    return fail $inp 'rng'
}

function match_str($inp, $t) {
    $n = $t.Length
    if ($inp.pos + $n -gt $inp.src.Length) { return fail $inp 'str' }
    if ($inp.src.Substring($inp.pos, $n) -cne $t) { return fail $inp 'str' }
    $cur = $inp; for ($i = 0; $i -lt $n; $i++) { $cur = $cur.adv() }
    return okV $cur $t
}

function mergeAsts([System.Collections.Generic.List[Ast]]$dst, [Res]$r) {
    if ($null -ne $r.ast) { $dst.Add($r.ast) }
    if ($null -ne $r.astList) { foreach ($a in $r.astList) { $dst.Add($a) } }
}

function peg_seq($inp, $fns) {
    $cur = $inp; $acc = ''; $asts = [System.Collections.Generic.List[Ast]]::new()
    foreach ($f in $fns) { $r = & $f $cur; if ($r.fail) { return $r }; $acc += $r.val; mergeAsts $asts $r; $cur = $r.rest }
    $res = okV $cur $acc
    if ($asts.Count -eq 1) { $res.ast = $asts[0] } elseif ($asts.Count -gt 1) { $res.astList = $asts }
    return $res
}

function peg_alt($inp, $fns) {
    foreach ($f in $fns) { $r = & $f $inp; if (-not $r.fail) { return $r } }
    return fail $inp 'alt'
}

function peg_star($inp, $f) {
    $cur = $inp; $acc = ''; $asts = [System.Collections.Generic.List[Ast]]::new()
    while ($true) { $r = & $f $cur; if ($r.fail -or $r.rest.pos -le $cur.pos) { break }; $acc += $r.val; mergeAsts $asts $r; $cur = $r.rest }
    $res = okV $cur $acc
    if ($asts.Count -gt 0) { $res.astList = $asts }
    return $res
}

function plus_($inp, $f) {
    $first = & $f $inp; if ($first.fail) { return $first }
    $rest = peg_star $first.rest $f
    $res = okV $rest.rest ($first.val + $rest.val)
    $asts = [System.Collections.Generic.List[Ast]]::new(); mergeAsts $asts $first; mergeAsts $asts $rest
    if ($asts.Count -gt 0) { $res.astList = $asts }
    return $res
}

function opt($inp, $f) { $r = & $f $inp; if ($r.fail) { return ok $inp }; return $r }
function neg($inp, $f) { $r = & $f $inp; if ($r.fail) { return ok $inp }; return fail $inp 'neg' }
function minus($inp, $fa, $fb) {
    $ra = & $fa $inp; if ($ra.fail) { return $ra }
    $rb = & $fb $inp; if (-not $rb.fail -and $rb.rest.pos -eq $ra.rest.pos) { return fail $inp 'excl' }; return $ra
}
function rep($inp, $n, $f) {
    $cur = $inp; $acc = ''
    for ($i = 0; $i -lt $n; $i++) { $r = & $f $cur; if ($r.fail) { return $r }; $acc += $r.val; $cur = $r.rest }
    return okV $cur $acc
}
function ahead($inp, $f) { $r = & $f $inp; if ($r.fail) { return $r }; return ok $inp }
function behind($inp, $f) {
    if ($inp.pos -eq 0) { return fail $inp 'bh' }
    $t = [Inp]::new($inp.src, $inp.pos - 1, $inp.line, [Math]::Max(0, $inp.col - 1))
    $r = & $f $t; if ($r.fail) { return fail $inp 'bh' }; return ok $inp
}
function sol($inp) { if ($inp.col -eq 0) { return ok $inp }; return fail $inp 'sol' }
function eof_ok($inp) { if ($inp.atEof()) { return ok $inp }; return fail $inp 'eof' }

# ════════════════════════════════════════════════════════════════ 
# YAML 1.2 Grammar — 211 rules 
# ════════════════════════════════════════════════════════════════ 

# [1] JSON-TEXT 
function json_text($inp) {
    return (peg_seq $inp @(
        { param($inp) ws $inp }.GetNewClosure(),
        { param($inp) value $inp }.GetNewClosure(),
        { param($inp) ws $inp }.GetNewClosure(),
        { param($inp) eof_ok $inp }.GetNewClosure()))
}

# [2] VALUE 
function value($inp) {
    return (peg_alt $inp @(
        { param($inp) object $inp }.GetNewClosure(),
        { param($inp) array $inp }.GetNewClosure(),
        { param($inp) string $inp }.GetNewClosure(),
        { param($inp) number $inp }.GetNewClosure(),
        { param($inp) match_str $inp "true" }.GetNewClosure(),
        { param($inp) match_str $inp "false" }.GetNewClosure(),
        { param($inp) match_str $inp "null" }.GetNewClosure()))
}

# [3] OBJECT 
function object($inp) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 123 }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) members $inp }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) match_cp $inp 125 }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 123 }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) match_cp $inp 125 }.GetNewClosure()) }.GetNewClosure()))
}

# [4] MEMBERS 
function members($inp) {
    return (peg_seq $inp @(
        { param($inp) member $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) match_cp $inp 44 }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) member $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [5] MEMBER 
function member($inp) {
    return (peg_seq $inp @(
        { param($inp) ws $inp }.GetNewClosure(),
        { param($inp) string $inp }.GetNewClosure(),
        { param($inp) ws $inp }.GetNewClosure(),
        { param($inp) match_cp $inp 58 }.GetNewClosure(),
        { param($inp) ws $inp }.GetNewClosure(),
        { param($inp) value $inp }.GetNewClosure(),
        { param($inp) ws $inp }.GetNewClosure()))
}

# [6] ARRAY 
function array($inp) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 91 }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) elements $inp }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) match_cp $inp 93 }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 91 }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) match_cp $inp 93 }.GetNewClosure()) }.GetNewClosure()))
}

# [7] ELEMENTS 
function elements($inp) {
    return (peg_seq $inp @(
        { param($inp) value $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) match_cp $inp 44 }.GetNewClosure(),
            { param($inp) ws $inp }.GetNewClosure(),
            { param($inp) value $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [8] STRING 
function string($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 34 }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) char $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) match_cp $inp 34 }.GetNewClosure()))
}

# [9] CHAR 
function char($inp) {
    return (peg_alt $inp @(
        { param($inp) escaped $inp }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) neg $inp { param($inp) match_cp $inp 34 }.GetNewClosure() }.GetNewClosure(),
            { param($inp) neg $inp { param($inp) match_cp $inp 92 }.GetNewClosure() }.GetNewClosure(),
            { param($inp) neg $inp { param($inp) match_cp $inp 0x0 }.GetNewClosure() }.GetNewClosure(),
            { param($inp) neg $inp { param($inp) match_range $inp 0x0 0x1F }.GetNewClosure() }.GetNewClosure(),
            { param($inp) match_range $inp 0x20 0x10FFFF }.GetNewClosure()) }.GetNewClosure()))
}

# [10] ESCAPED 
function escaped($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 92 }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) match_cp $inp 34 }.GetNewClosure(),
            { param($inp) match_cp $inp 92 }.GetNewClosure(),
            { param($inp) match_cp $inp 47 }.GetNewClosure(),
            { param($inp) match_cp $inp 98 }.GetNewClosure(),
            { param($inp) match_cp $inp 102 }.GetNewClosure(),
            { param($inp) match_cp $inp 110 }.GetNewClosure(),
            { param($inp) match_cp $inp 114 }.GetNewClosure(),
            { param($inp) match_cp $inp 116 }.GetNewClosure(),
            { param($inp) peg_seq $inp @(
                { param($inp) match_cp $inp 117 }.GetNewClosure(),
                { param($inp) hex4 $inp }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure()))
}

# [11] HEX4 
function hex4($inp) {
    return (peg_seq $inp @(
        { param($inp) hexdig $inp }.GetNewClosure(),
        { param($inp) hexdig $inp }.GetNewClosure(),
        { param($inp) hexdig $inp }.GetNewClosure(),
        { param($inp) hexdig $inp }.GetNewClosure()))
}

# [12] HEXDIG 
function hexdig($inp) {
    return (peg_alt $inp @(
        { param($inp) match_range $inp 48 57 }.GetNewClosure(),
        { param($inp) match_range $inp 97 102 }.GetNewClosure(),
        { param($inp) match_range $inp 65 70 }.GetNewClosure()))
}

# [13] NUMBER 
function number($inp) {
    return (peg_seq $inp @(
        { param($inp) opt $inp { param($inp) match_cp $inp 45 }.GetNewClosure() }.GetNewClosure(),
        { param($inp) integer $inp }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) fraction $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) exponent $inp }.GetNewClosure() }.GetNewClosure()))
}

# [14] INTEGER 
function integer($inp) {
    return (peg_alt $inp @(
        { param($inp) match_cp $inp 48 }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) match_range $inp 49 57 }.GetNewClosure(),
            { param($inp) peg_star $inp { param($inp) match_range $inp 48 57 }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()))
}

# [15] FRACTION 
function fraction($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 46 }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) match_range $inp 48 57 }.GetNewClosure() }.GetNewClosure()))
}

# [16] EXPONENT 
function exponent($inp) {
    return (peg_seq $inp @(
        { param($inp) peg_alt $inp @(
            { param($inp) match_cp $inp 101 }.GetNewClosure(),
            { param($inp) match_cp $inp 69 }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) peg_alt $inp @(
            { param($inp) match_cp $inp 43 }.GetNewClosure(),
            { param($inp) match_cp $inp 45 }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) match_range $inp 48 57 }.GetNewClosure() }.GetNewClosure()))
}

# [17] WS 
function ws($inp) {
    return (peg_star $inp { param($inp) peg_alt $inp @(
        { param($inp) match_cp $inp 0x20 }.GetNewClosure(),
        { param($inp) match_cp $inp 0x9 }.GetNewClosure(),
        { param($inp) match_cp $inp 0x0A }.GetNewClosure(),
        { param($inp) match_cp $inp 0x0D }.GetNewClosure()) }.GetNewClosure())
}

# ── API ──

function printAst($node, $depth) {
    $indent = '  ' * $depth
    if ($node.isLeaf) {
        Write-Host "${indent}SCALAR: `"$($node.text)`""
    } else {
        Write-Host "${indent}$($node.tag)"
        foreach ($c in $node.children) { printAst $c ($depth + 1) }
    }
}

# ── Main ──

if ($args.Count -gt 0) {
    $text = [System.IO.File]::ReadAllText($args[0])
} else {
    $text = [Console]::In.ReadToEnd()
}
$inp = [Inp]::new($text)
$r = json_text $inp
if (-not $r.fail) {
    Write-Host "OK: $($r.rest.pos) chars"
    if ($null -ne $r.ast) { printAst $r.ast 0 }
} else {
    [Console]::Error.WriteLine("FAIL @$($r.rest.pos): $($r.err)")
    exit 1
}
