;;;; peg-powershell.lisp — PowerShell target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "PowerShell")
(def-tgt "default-output" "yaml_reader.ps1")

(def-tgt "keywords"
  '("begin" "break" "catch" "class" "continue" "data" "define" "do"
    "dynamicparam" "else" "elseif" "end" "enum" "exit" "filter" "finally"
    "for" "foreach" "from" "function" "hidden" "if" "in" "inlinescript"
    "parallel" "param" "process" "return" "sequence" "switch" "throw"
    "trap" "try" "until" "using" "var" "while" "workflow"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "{ param($inp) ~A }.GetNewClosure()" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "{ param($inp) ~A }.GetNewClosure()" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "peg_seq $inp @(~{~A~^, ~})" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "peg_alt $inp @(~{~A~^, ~})" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (with-output-to-string (s)
      (format s "switch (~A) {~%" param)
      (loop for (val body) in cases
            do (format s "        ~S { ~A }~%" val body))
      (format s "        default { fail $inp \"no case\" }~%    }"))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "& { $r = ~A; if ($r.fail) { return $r }; $~A = $r.tagInt; $inp = $r.rest; ~A }"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "& { $r = ~A; if ($r.fail) { return $r }; $~A = $r.tag; $inp = $r.rest; ~A }"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (format nil "$~A" (peg-ident sym))))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "[int]~A" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "(inFlow ~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "(seqSpaces ~A ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A(~{$~A~^, ~})" name
                (cons "inp" (mapcar #'peg-ident params)))
        (format nil "~A($inp)" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "function ~A {~%    return (~A)~%}" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Comment prefix ──

(def-tgt "comment-prefix" "#")

;;; ── Header ──

(def-tgt "header"
"# ════════════════════════════════════════════════════════════════
# yaml_reader.ps1 — YAML 1.2 parser
# ════════════════════════════════════════════════════════════════
$ErrorActionPreference = 'Stop'")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"# ── Input ──

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
        $nl = $c -eq \"`n\"
        return [Inp]::new($this.src, $this.pos + 1, $(if ($nl) { $this.line + 1 } else { $this.line }), $(if ($nl) { 0 } else { $this.col + 1 }))
    }
}"

"# ── AST ──

class Ast {
    [string]$tag
    [string]$text
    [System.Collections.Generic.List[Ast]]$children
    [bool]$isLeaf
    Ast([string]$tag) { $this.tag = $tag; $this.text = ''; $this.children = [System.Collections.Generic.List[Ast]]::new(); $this.isLeaf = $false }
    static [Ast] Leaf([string]$text) { $a = [Ast]::new(''); $a.text = $text; $a.isLeaf = $true; return $a }
}"

"# ── Result ──

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
function fail($inp, $msg) { $r = [Res]::new(); $r.fail = $true; $r.rest = $inp; $r.err = $msg; return $r }"

"# ── Context ──

function inFlow($c) { if ($c -eq 'FLOW-OUT' -or $c -eq 'FLOW-IN') { return 'FLOW-IN' }; return 'FLOW-KEY' }
function seqSpaces($n, $c) { if ($c -eq 'BLOCK-OUT') { return $n - 1 }; return $n }"

"# ── Combinators ──

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
function eof_ok($inp) { if ($inp.atEof()) { return ok $inp }; return fail $inp 'eof' }"

"# ── YAML extensions ──

function build($inp, $typ, $f) {
    $r = & $f $inp; if ($r.fail) { return $r }
    $node = [Ast]::new($typ)
    if ($null -ne $r.ast) { $node.children.Add($r.ast) }
    if ($null -ne $r.astList) { foreach ($a in $r.astList) { $node.children.Add($a) } }
    $r.ast = $node; $r.astList = $null; return $r
}

function scalar($inp, $f) {
    $r = & $f $inp; if ($r.fail) { return $r }
    $r.ast = [Ast]::Leaf($r.val); return $r
}

function collect($inp, $f) { return & $f $inp }

function detect_indent($inp, $n) {
    $s = $inp.src; $l = $s.Length; $i = $inp.pos
    $sp = 0; while ($i + $sp -lt $l -and [int]$s[$i + $sp] -eq 32) { $sp++ }
    if ($i + $sp -lt $l -and [int]$s[$i + $sp] -ne 10) { $r = ok $inp; $r.tagInt = [Math]::Max(1, $sp - $n); return $r }
    $j = $i; while ($j -lt $l -and [int]$s[$j] -ne 10) { $j++ }
    while ($j -lt $l) {
        if ([int]$s[$j] -eq 10) { $j++ }; if ($j -ge $l) { break }
        $sp = 0; while ($j + $sp -lt $l -and [int]$s[$j + $sp] -eq 32) { $sp++ }
        $nx = $j + $sp; if ($nx -ge $l -or [int]$s[$nx] -eq 10) { $j = $nx; continue }
        $r = ok $inp; $r.tagInt = [Math]::Max(1, $sp - $n); return $r
    }
    $r = ok $inp; $r.tagInt = 1; return $r
}

function parse_int($inp, $f) {
    $r = & $f $inp; if ($r.fail) { return $r }
    $v = 0; foreach ($ch in $r.val.ToCharArray()) { if ($ch -ge '0' -and $ch -le '9') { $v = $v * 10 + ([int]$ch - 48) } }
    $r.tagInt = $v; return $r
}

function parse_sym($inp, $f, $sym) {
    $r = & $f $inp; if ($r.fail) { return $r }; $r.tag = $sym; return $r
}

function val($inp, $v) { $r = ok $inp; $r.tag = $v; return $r }"
))

;;; ── API ──

(def-tgt "api"
"# ── API ──

function printAst($node, $depth) {
    $indent = '  ' * $depth
    if ($node.isLeaf) {
        Write-Host \"${indent}SCALAR: `\"$($node.text)`\"\"
    } else {
        Write-Host \"${indent}$($node.tag)\"
        foreach ($c in $node.children) { printAst $c ($depth + 1) }
    }
}")

;;; ── Main ──

(def-tgt "main-fn"
"# ── Main ──

if ($args.Count -gt 0) {
    $text = [System.IO.File]::ReadAllText($args[0])
} else {
    $text = [Console]::In.ReadToEnd()
}
$inp = [Inp]::new($text)
$r = l_yaml_stream $inp
if (-not $r.fail) {
    Write-Host \"OK: $($r.rest.pos) chars\"
    if ($null -ne $r.ast) { printAst $r.ast 0 }
} else {
    [Console]::Error.WriteLine(\"FAIL @$($r.rest.pos): $($r.err)\")
    exit 1
}")

(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)
