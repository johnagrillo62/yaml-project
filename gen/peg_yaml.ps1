# ════════════════════════════════════════════════════════════════
# yaml_reader.ps1 — YAML 1.2 parser
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

# ── YAML extensions ──

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

function val($inp, $v) { $r = ok $inp; $r.tag = $v; return $r }

# ════════════════════════════════════════════════════════════════ 
# YAML 1.2 Grammar — 211 rules 
# ════════════════════════════════════════════════════════════════ 

# [1] C-PRINTABLE 
function c_printable($inp) {
    return (peg_alt $inp @(
        { param($inp) match_cp $inp 0x9 }.GetNewClosure(),
        { param($inp) match_cp $inp 0x0A }.GetNewClosure(),
        { param($inp) match_cp $inp 0x0D }.GetNewClosure(),
        { param($inp) match_range $inp 0x20 0x7E }.GetNewClosure(),
        { param($inp) match_cp $inp 0x85 }.GetNewClosure(),
        { param($inp) match_range $inp 0xA0 0xD7FF }.GetNewClosure(),
        { param($inp) match_range $inp 0xE000 0xFFFD }.GetNewClosure(),
        { param($inp) match_range $inp 0x10000 0x10FFFF }.GetNewClosure()))
}

# [2] NB-JSON 
function nb_json($inp) {
    return (peg_alt $inp @(
        { param($inp) match_cp $inp 0x9 }.GetNewClosure(),
        { param($inp) match_range $inp 0x20 0x10FFFF }.GetNewClosure()))
}

# [3] C-BYTE-ORDER-MARK 
function c_byte_order_mark($inp) {
    return (match_cp $inp 0xFEFF)
}

# [4] C-SEQUENCE-ENTRY 
function c_sequence_entry($inp) {
    return (match_cp $inp 45)
}

# [5] C-MAPPING-KEY 
function c_mapping_key($inp) {
    return (match_cp $inp 63)
}

# [6] C-MAPPING-VALUE 
function c_mapping_value($inp) {
    return (match_cp $inp 58)
}

# [7] C-COLLECT-ENTRY 
function c_collect_entry($inp) {
    return (match_cp $inp 44)
}

# [8] C-SEQUENCE-START 
function c_sequence_start($inp) {
    return (match_cp $inp 91)
}

# [9] C-SEQUENCE-END 
function c_sequence_end($inp) {
    return (match_cp $inp 93)
}

# [10] C-MAPPING-START 
function c_mapping_start($inp) {
    return (match_cp $inp 123)
}

# [11] C-MAPPING-END 
function c_mapping_end($inp) {
    return (match_cp $inp 125)
}

# [12] C-COMMENT 
function c_comment($inp) {
    return (match_cp $inp 35)
}

# [13] C-ANCHOR 
function c_anchor($inp) {
    return (match_cp $inp 38)
}

# [14] C-ALIAS 
function c_alias($inp) {
    return (match_cp $inp 42)
}

# [15] C-TAG 
function c_tag($inp) {
    return (match_cp $inp 33)
}

# [16] C-LITERAL 
function c_literal($inp) {
    return (match_cp $inp 124)
}

# [17] C-FOLDED 
function c_folded($inp) {
    return (match_cp $inp 62)
}

# [18] C-SINGLE-QUOTE 
function c_single_quote($inp) {
    return (match_cp $inp 39)
}

# [19] C-DOUBLE-QUOTE 
function c_double_quote($inp) {
    return (match_cp $inp 34)
}

# [20] C-DIRECTIVE 
function c_directive($inp) {
    return (match_cp $inp 37)
}

# [21] C-RESERVED 
function c_reserved($inp) {
    return (peg_alt $inp @(
        { param($inp) match_cp $inp 64 }.GetNewClosure(),
        { param($inp) match_cp $inp 96 }.GetNewClosure()))
}

# [22] C-INDICATOR 
function c_indicator($inp) {
    return (peg_alt $inp @(
        { param($inp) c_sequence_entry $inp }.GetNewClosure(),
        { param($inp) c_mapping_key $inp }.GetNewClosure(),
        { param($inp) c_mapping_value $inp }.GetNewClosure(),
        { param($inp) c_collect_entry $inp }.GetNewClosure(),
        { param($inp) c_sequence_start $inp }.GetNewClosure(),
        { param($inp) c_sequence_end $inp }.GetNewClosure(),
        { param($inp) c_mapping_start $inp }.GetNewClosure(),
        { param($inp) c_mapping_end $inp }.GetNewClosure(),
        { param($inp) c_comment $inp }.GetNewClosure(),
        { param($inp) c_anchor $inp }.GetNewClosure(),
        { param($inp) c_alias $inp }.GetNewClosure(),
        { param($inp) c_tag $inp }.GetNewClosure(),
        { param($inp) c_literal $inp }.GetNewClosure(),
        { param($inp) c_folded $inp }.GetNewClosure(),
        { param($inp) c_single_quote $inp }.GetNewClosure(),
        { param($inp) c_double_quote $inp }.GetNewClosure(),
        { param($inp) c_directive $inp }.GetNewClosure(),
        { param($inp) c_reserved $inp }.GetNewClosure()))
}

# [23] C-FLOW-INDICATOR 
function c_flow_indicator($inp) {
    return (peg_alt $inp @(
        { param($inp) c_collect_entry $inp }.GetNewClosure(),
        { param($inp) c_sequence_start $inp }.GetNewClosure(),
        { param($inp) c_sequence_end $inp }.GetNewClosure(),
        { param($inp) c_mapping_start $inp }.GetNewClosure(),
        { param($inp) c_mapping_end $inp }.GetNewClosure()))
}

# [24] B-LINE-FEED 
function b_line_feed($inp) {
    return (match_cp $inp 0x0A)
}

# [25] B-CARRIAGE-RETURN 
function b_carriage_return($inp) {
    return (match_cp $inp 0x0D)
}

# [26] B-CHAR 
function b_char($inp) {
    return (peg_alt $inp @(
        { param($inp) b_line_feed $inp }.GetNewClosure(),
        { param($inp) b_carriage_return $inp }.GetNewClosure()))
}

# [27] NB-CHAR 
function nb_char($inp) {
    return (minus $inp { param($inp) c_printable $inp }.GetNewClosure() { param($inp) peg_alt $inp @(
        { param($inp) b_char $inp }.GetNewClosure(),
        { param($inp) c_byte_order_mark $inp }.GetNewClosure()) }.GetNewClosure())
}

# [28] B-BREAK 
function b_break($inp) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) b_carriage_return $inp }.GetNewClosure(),
            { param($inp) b_line_feed $inp }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) b_carriage_return $inp }.GetNewClosure(),
        { param($inp) b_line_feed $inp }.GetNewClosure()))
}

# [29] B-AS-LINE-FEED 
function b_as_line_feed($inp) {
    return (b_break $inp)
}

# [30] B-NON-CONTENT 
function b_non_content($inp) {
    return (b_break $inp)
}

# [31] S-SPACE 
function s_space($inp) {
    return (match_cp $inp 0x20)
}

# [32] S-TAB 
function s_tab($inp) {
    return (match_cp $inp 0x9)
}

# [33] S-WHITE 
function s_white($inp) {
    return (peg_alt $inp @(
        { param($inp) s_space $inp }.GetNewClosure(),
        { param($inp) s_tab $inp }.GetNewClosure()))
}

# [34] NS-CHAR 
function ns_char($inp) {
    return (minus $inp { param($inp) nb_char $inp }.GetNewClosure() { param($inp) s_white $inp }.GetNewClosure())
}

# [35] NS-DEC-DIGIT 
function ns_dec_digit($inp) {
    return (match_range $inp 0x30 0x39)
}

# [36] NS-HEX-DIGIT 
function ns_hex_digit($inp) {
    return (peg_alt $inp @(
        { param($inp) ns_dec_digit $inp }.GetNewClosure(),
        { param($inp) match_range $inp 0x41 0x46 }.GetNewClosure(),
        { param($inp) match_range $inp 0x61 0x66 }.GetNewClosure()))
}

# [37] NS-ASCII-LETTER 
function ns_ascii_letter($inp) {
    return (peg_alt $inp @(
        { param($inp) match_range $inp 0x41 0x5A }.GetNewClosure(),
        { param($inp) match_range $inp 0x61 0x7A }.GetNewClosure()))
}

# [38] NS-WORD-CHAR 
function ns_word_char($inp) {
    return (peg_alt $inp @(
        { param($inp) ns_dec_digit $inp }.GetNewClosure(),
        { param($inp) ns_ascii_letter $inp }.GetNewClosure(),
        { param($inp) match_cp $inp 45 }.GetNewClosure()))
}

# [39] NS-URI-CHAR 
function ns_uri_char($inp) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 37 }.GetNewClosure(),
            { param($inp) ns_hex_digit $inp }.GetNewClosure(),
            { param($inp) ns_hex_digit $inp }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) ns_word_char $inp }.GetNewClosure(),
        { param($inp) match_cp $inp 35 }.GetNewClosure(),
        { param($inp) match_cp $inp 59 }.GetNewClosure(),
        { param($inp) match_cp $inp 47 }.GetNewClosure(),
        { param($inp) match_cp $inp 63 }.GetNewClosure(),
        { param($inp) match_cp $inp 58 }.GetNewClosure(),
        { param($inp) match_cp $inp 64 }.GetNewClosure(),
        { param($inp) match_cp $inp 38 }.GetNewClosure(),
        { param($inp) match_cp $inp 61 }.GetNewClosure(),
        { param($inp) match_cp $inp 43 }.GetNewClosure(),
        { param($inp) match_cp $inp 36 }.GetNewClosure(),
        { param($inp) match_cp $inp 44 }.GetNewClosure(),
        { param($inp) match_cp $inp 95 }.GetNewClosure(),
        { param($inp) match_cp $inp 46 }.GetNewClosure(),
        { param($inp) match_cp $inp 33 }.GetNewClosure(),
        { param($inp) match_cp $inp 126 }.GetNewClosure(),
        { param($inp) match_cp $inp 42 }.GetNewClosure(),
        { param($inp) match_cp $inp 39 }.GetNewClosure(),
        { param($inp) match_cp $inp 40 }.GetNewClosure(),
        { param($inp) match_cp $inp 41 }.GetNewClosure(),
        { param($inp) match_cp $inp 91 }.GetNewClosure(),
        { param($inp) match_cp $inp 93 }.GetNewClosure()))
}

# [40] NS-TAG-CHAR 
function ns_tag_char($inp) {
    return (minus $inp { param($inp) ns_uri_char $inp }.GetNewClosure() { param($inp) peg_alt $inp @(
        { param($inp) c_tag $inp }.GetNewClosure(),
        { param($inp) c_flow_indicator $inp }.GetNewClosure()) }.GetNewClosure())
}

# [41] C-ESCAPE 
function c_escape($inp) {
    return (match_cp $inp 92)
}

# [42] NS-ESC-NULL 
function ns_esc_null($inp) {
    return (match_cp $inp 48)
}

# [43] NS-ESC-BELL 
function ns_esc_bell($inp) {
    return (match_cp $inp 97)
}

# [44] NS-ESC-BACKSPACE 
function ns_esc_backspace($inp) {
    return (match_cp $inp 98)
}

# [45] NS-ESC-HORIZONTAL-TAB 
function ns_esc_horizontal_tab($inp) {
    return (match_cp $inp 116)
}

# [46] NS-ESC-LINE-FEED 
function ns_esc_line_feed($inp) {
    return (match_cp $inp 110)
}

# [47] NS-ESC-VERTICAL-TAB 
function ns_esc_vertical_tab($inp) {
    return (match_cp $inp 118)
}

# [48] NS-ESC-FORM-FEED 
function ns_esc_form_feed($inp) {
    return (match_cp $inp 102)
}

# [49] NS-ESC-CARRIAGE-RETURN 
function ns_esc_carriage_return($inp) {
    return (match_cp $inp 114)
}

# [50] NS-ESC-ESCAPE 
function ns_esc_escape($inp) {
    return (match_cp $inp 101)
}

# [51] NS-ESC-SPACE 
function ns_esc_space($inp) {
    return (match_cp $inp 0x20)
}

# [52] NS-ESC-DOUBLE-QUOTE 
function ns_esc_double_quote($inp) {
    return (match_cp $inp 34)
}

# [53] NS-ESC-SLASH 
function ns_esc_slash($inp) {
    return (match_cp $inp 47)
}

# [54] NS-ESC-BACKSLASH 
function ns_esc_backslash($inp) {
    return (match_cp $inp 92)
}

# [55] NS-ESC-NEXT-LINE 
function ns_esc_next_line($inp) {
    return (match_cp $inp 78)
}

# [56] NS-ESC-NON-BREAKING-SPACE 
function ns_esc_non_breaking_space($inp) {
    return (match_cp $inp 95)
}

# [57] NS-ESC-LINE-SEPARATOR 
function ns_esc_line_separator($inp) {
    return (match_cp $inp 76)
}

# [58] NS-ESC-PARAGRAPH-SEPARATOR 
function ns_esc_paragraph_separator($inp) {
    return (match_cp $inp 80)
}

# [59] NS-ESC-8-BIT 
function ns_esc_8_bit($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 120 }.GetNewClosure(),
        { param($inp) rep $inp 2 { param($inp) ns_hex_digit $inp }.GetNewClosure() }.GetNewClosure()))
}

# [60] NS-ESC-16-BIT 
function ns_esc_16_bit($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 117 }.GetNewClosure(),
        { param($inp) rep $inp 4 { param($inp) ns_hex_digit $inp }.GetNewClosure() }.GetNewClosure()))
}

# [61] NS-ESC-32-BIT 
function ns_esc_32_bit($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 85 }.GetNewClosure(),
        { param($inp) rep $inp 8 { param($inp) ns_hex_digit $inp }.GetNewClosure() }.GetNewClosure()))
}

# [62] C-NS-ESC-CHAR 
function c_ns_esc_char($inp) {
    return (peg_seq $inp @(
        { param($inp) c_escape $inp }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) ns_esc_null $inp }.GetNewClosure(),
            { param($inp) ns_esc_bell $inp }.GetNewClosure(),
            { param($inp) ns_esc_backspace $inp }.GetNewClosure(),
            { param($inp) ns_esc_horizontal_tab $inp }.GetNewClosure(),
            { param($inp) ns_esc_line_feed $inp }.GetNewClosure(),
            { param($inp) ns_esc_vertical_tab $inp }.GetNewClosure(),
            { param($inp) ns_esc_form_feed $inp }.GetNewClosure(),
            { param($inp) ns_esc_carriage_return $inp }.GetNewClosure(),
            { param($inp) ns_esc_escape $inp }.GetNewClosure(),
            { param($inp) ns_esc_space $inp }.GetNewClosure(),
            { param($inp) ns_esc_double_quote $inp }.GetNewClosure(),
            { param($inp) ns_esc_slash $inp }.GetNewClosure(),
            { param($inp) ns_esc_backslash $inp }.GetNewClosure(),
            { param($inp) ns_esc_next_line $inp }.GetNewClosure(),
            { param($inp) ns_esc_non_breaking_space $inp }.GetNewClosure(),
            { param($inp) ns_esc_line_separator $inp }.GetNewClosure(),
            { param($inp) ns_esc_paragraph_separator $inp }.GetNewClosure(),
            { param($inp) ns_esc_8_bit $inp }.GetNewClosure(),
            { param($inp) ns_esc_16_bit $inp }.GetNewClosure(),
            { param($inp) ns_esc_32_bit $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [63] S-INDENT 
function s_indent($inp, $n) {
    return (rep $inp $n { param($inp) s_space $inp }.GetNewClosure())
}

# [64] S-INDENT-LT 
function s_indent_lt($inp, $n) {
    return (peg_star $inp { param($inp) s_space $inp }.GetNewClosure())
}

# [65] S-INDENT-LE 
function s_indent_le($inp, $n) {
    return (peg_star $inp { param($inp) s_space $inp }.GetNewClosure())
}

# [66] S-SEPARATE-IN-LINE 
function s_separate_in_line($inp) {
    return (peg_alt $inp @(
        { param($inp) plus_ $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) ok $inp }.GetNewClosure()))
}

# [67] S-LINE-PREFIX 
function s_line_prefix($inp, $n, $c) {
    return (& { if ($c -eq "BLOCK-IN") { return (s_block_line_prefix $inp $n) }
        elseif ($c -eq "BLOCK-OUT") { return (s_block_line_prefix $inp $n) }
        elseif ($c -eq "FLOW-IN") { return (s_flow_line_prefix $inp $n) }
        elseif ($c -eq "FLOW-OUT") { return (s_flow_line_prefix $inp $n) }
        else { return (fail $inp "no case") } })
}

# [68] S-BLOCK-LINE-PREFIX 
function s_block_line_prefix($inp, $n) {
    return (s_indent $inp $n)
}

# [69] S-FLOW-LINE-PREFIX 
function s_flow_line_prefix($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_indent $inp $n }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) s_separate_in_line $inp }.GetNewClosure() }.GetNewClosure()))
}

# [70] L-EMPTY 
function l_empty($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) peg_alt $inp @(
            { param($inp) s_line_prefix $inp $n $c }.GetNewClosure(),
            { param($inp) s_indent_lt $inp $n }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) b_as_line_feed $inp }.GetNewClosure()))
}

# [71] B-L-TRIMMED 
function b_l_trimmed($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) b_non_content $inp }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) l_empty $inp $n $c }.GetNewClosure() }.GetNewClosure()))
}

# [72] B-AS-SPACE 
function b_as_space($inp) {
    return (b_break $inp)
}

# [73] B-L-FOLDED 
function b_l_folded($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) b_l_trimmed $inp $n $c }.GetNewClosure(),
        { param($inp) b_as_space $inp }.GetNewClosure()))
}

# [74] S-FLOW-FOLDED 
function s_flow_folded($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) opt $inp { param($inp) s_separate_in_line $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) b_l_folded $inp $n "FLOW-IN" }.GetNewClosure(),
        { param($inp) s_flow_line_prefix $inp $n }.GetNewClosure()))
}

# [75] C-NB-COMMENT-TEXT 
function c_nb_comment_text($inp) {
    return (peg_seq $inp @(
        { param($inp) c_comment $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) nb_char $inp }.GetNewClosure() }.GetNewClosure()))
}

# [76] B-COMMENT 
function b_comment($inp) {
    return (peg_alt $inp @(
        { param($inp) b_non_content $inp }.GetNewClosure(),
        { param($inp) ok $inp }.GetNewClosure()))
}

# [77] S-B-COMMENT 
function s_b_comment($inp) {
    return (peg_seq $inp @(
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) s_separate_in_line $inp }.GetNewClosure(),
            { param($inp) opt $inp { param($inp) c_nb_comment_text $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) b_comment $inp }.GetNewClosure()))
}

# [78] L-COMMENT 
function l_comment($inp) {
    return (peg_seq $inp @(
        { param($inp) s_separate_in_line $inp }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) c_nb_comment_text $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) b_non_content $inp }.GetNewClosure()))
}

# [79] S-L-COMMENTS 
function s_l_comments($inp) {
    return (peg_seq $inp @(
        { param($inp) peg_alt $inp @(
            { param($inp) s_b_comment $inp }.GetNewClosure(),
            { param($inp) ok $inp }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) l_comment $inp }.GetNewClosure() }.GetNewClosure()))
}

# [80] S-SEPARATE 
function s_separate($inp, $n, $c) {
    return (& { if ($c -eq "BLOCK-OUT") { return (s_separate_lines $inp $n) }
        elseif ($c -eq "BLOCK-IN") { return (s_separate_lines $inp $n) }
        elseif ($c -eq "FLOW-OUT") { return (s_separate_lines $inp $n) }
        elseif ($c -eq "FLOW-IN") { return (s_separate_lines $inp $n) }
        elseif ($c -eq "BLOCK-KEY") { return (s_separate_in_line $inp) }
        elseif ($c -eq "FLOW-KEY") { return (s_separate_in_line $inp) }
        else { return (fail $inp "no case") } })
}

# [81] S-SEPARATE-LINES 
function s_separate_lines($inp, $n) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) s_l_comments $inp }.GetNewClosure(),
            { param($inp) s_flow_line_prefix $inp $n }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) s_separate_in_line $inp }.GetNewClosure()))
}

# [82] L-DIRECTIVE 
function l_directive($inp) {
    return (peg_seq $inp @(
        { param($inp) c_directive $inp }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) ns_yaml_directive $inp }.GetNewClosure(),
            { param($inp) ns_tag_directive $inp }.GetNewClosure(),
            { param($inp) ns_reserved_directive $inp }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) s_l_comments $inp }.GetNewClosure()))
}

# [83] NS-RESERVED-DIRECTIVE 
function ns_reserved_directive($inp) {
    return (peg_seq $inp @(
        { param($inp) ns_directive_name $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) s_separate_in_line $inp }.GetNewClosure(),
            { param($inp) ns_directive_parameter $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [84] NS-DIRECTIVE-NAME 
function ns_directive_name($inp) {
    return (plus_ $inp { param($inp) ns_char $inp }.GetNewClosure())
}

# [85] NS-DIRECTIVE-PARAMETER 
function ns_directive_parameter($inp) {
    return (plus_ $inp { param($inp) ns_char $inp }.GetNewClosure())
}

# [86] NS-YAML-DIRECTIVE 
function ns_yaml_directive($inp) {
    return (peg_seq $inp @(
        { param($inp) match_str $inp "YAML" }.GetNewClosure(),
        { param($inp) s_separate_in_line $inp }.GetNewClosure(),
        { param($inp) ns_yaml_version $inp }.GetNewClosure()))
}

# [87] NS-YAML-VERSION 
function ns_yaml_version($inp) {
    return (peg_seq $inp @(
        { param($inp) plus_ $inp { param($inp) ns_dec_digit $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) match_cp $inp 46 }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) ns_dec_digit $inp }.GetNewClosure() }.GetNewClosure()))
}

# [88] NS-TAG-DIRECTIVE 
function ns_tag_directive($inp) {
    return (peg_seq $inp @(
        { param($inp) match_str $inp "TAG" }.GetNewClosure(),
        { param($inp) s_separate_in_line $inp }.GetNewClosure(),
        { param($inp) c_tag_handle $inp }.GetNewClosure(),
        { param($inp) s_separate_in_line $inp }.GetNewClosure(),
        { param($inp) ns_tag_prefix $inp }.GetNewClosure()))
}

# [89] C-TAG-HANDLE 
function c_tag_handle($inp) {
    return (peg_alt $inp @(
        { param($inp) c_named_tag_handle $inp }.GetNewClosure(),
        { param($inp) c_secondary_tag_handle $inp }.GetNewClosure(),
        { param($inp) c_primary_tag_handle $inp }.GetNewClosure()))
}

# [90] C-PRIMARY-TAG-HANDLE 
function c_primary_tag_handle($inp) {
    return (match_cp $inp 33)
}

# [91] C-SECONDARY-TAG-HANDLE 
function c_secondary_tag_handle($inp) {
    return (match_str $inp "!!")
}

# [92] C-NAMED-TAG-HANDLE 
function c_named_tag_handle($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 33 }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) ns_word_char $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) match_cp $inp 33 }.GetNewClosure()))
}

# [93] NS-TAG-PREFIX 
function ns_tag_prefix($inp) {
    return (peg_alt $inp @(
        { param($inp) c_ns_local_tag_prefix $inp }.GetNewClosure(),
        { param($inp) ns_global_tag_prefix $inp }.GetNewClosure()))
}

# [94] C-NS-LOCAL-TAG-PREFIX 
function c_ns_local_tag_prefix($inp) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 33 }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) ns_uri_char $inp }.GetNewClosure() }.GetNewClosure()))
}

# [95] NS-GLOBAL-TAG-PREFIX 
function ns_global_tag_prefix($inp) {
    return (peg_seq $inp @(
        { param($inp) ns_tag_char $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) ns_uri_char $inp }.GetNewClosure() }.GetNewClosure()))
}

# [96] C-NS-PROPERTIES 
function c_ns_properties($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) c_ns_tag_property $inp }.GetNewClosure(),
            { param($inp) opt $inp { param($inp) peg_seq $inp @(
                { param($inp) s_separate $inp $n $c }.GetNewClosure(),
                { param($inp) c_ns_anchor_property $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) c_ns_anchor_property $inp }.GetNewClosure(),
            { param($inp) opt $inp { param($inp) peg_seq $inp @(
                { param($inp) s_separate $inp $n $c }.GetNewClosure(),
                { param($inp) c_ns_tag_property $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()))
}

# [97] C-NS-TAG-PROPERTY 
function c_ns_tag_property($inp) {
    return (peg_alt $inp @(
        { param($inp) c_verbatim_tag $inp }.GetNewClosure(),
        { param($inp) c_ns_shorthand_tag $inp }.GetNewClosure(),
        { param($inp) c_non_specific_tag $inp }.GetNewClosure()))
}

# [98] C-VERBATIM-TAG 
function c_verbatim_tag($inp) {
    return (peg_seq $inp @(
        { param($inp) match_str $inp "!<" }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) ns_uri_char $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) match_cp $inp 62 }.GetNewClosure()))
}

# [99] C-NS-SHORTHAND-TAG 
function c_ns_shorthand_tag($inp) {
    return (peg_seq $inp @(
        { param($inp) c_tag_handle $inp }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) ns_tag_char $inp }.GetNewClosure() }.GetNewClosure()))
}

# [100] C-NON-SPECIFIC-TAG 
function c_non_specific_tag($inp) {
    return (match_cp $inp 33)
}

# [101] C-NS-ANCHOR-PROPERTY 
function c_ns_anchor_property($inp) {
    return (build $inp "ANCHOR" { param($inp) peg_seq $inp @(
        { param($inp) c_anchor $inp }.GetNewClosure(),
        { param($inp) scalar $inp { param($inp) ns_anchor_name $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure())
}

# [102] NS-ANCHOR-CHAR 
function ns_anchor_char($inp) {
    return (minus $inp { param($inp) ns_char $inp }.GetNewClosure() { param($inp) c_flow_indicator $inp }.GetNewClosure())
}

# [103] NS-ANCHOR-NAME 
function ns_anchor_name($inp) {
    return (plus_ $inp { param($inp) ns_anchor_char $inp }.GetNewClosure())
}

# [104] C-NS-ALIAS-NODE 
function c_ns_alias_node($inp) {
    return (build $inp "ALIAS" { param($inp) peg_seq $inp @(
        { param($inp) c_alias $inp }.GetNewClosure(),
        { param($inp) scalar $inp { param($inp) ns_anchor_name $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure())
}

# [105] E-SCALAR 
function e_scalar($inp) {
    return (ok $inp)
}

# [106] E-NODE 
function e_node($inp) {
    return (e_scalar $inp)
}

# [107] NB-DOUBLE-CHAR 
function nb_double_char($inp) {
    return (peg_alt $inp @(
        { param($inp) c_ns_esc_char $inp }.GetNewClosure(),
        { param($inp) minus $inp { param($inp) nb_json $inp }.GetNewClosure() { param($inp) peg_alt $inp @(
            { param($inp) match_cp $inp 92 }.GetNewClosure(),
            { param($inp) match_cp $inp 34 }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [108] NS-DOUBLE-CHAR 
function ns_double_char($inp) {
    return (minus $inp { param($inp) nb_double_char $inp }.GetNewClosure() { param($inp) s_white $inp }.GetNewClosure())
}

# [109] C-DOUBLE-QUOTED 
function c_double_quoted($inp, $n, $c) {
    return (scalar $inp { param($inp) peg_seq $inp @(
        { param($inp) match_cp $inp 34 }.GetNewClosure(),
        { param($inp) nb_double_text $inp $n $c }.GetNewClosure(),
        { param($inp) match_cp $inp 34 }.GetNewClosure()) }.GetNewClosure())
}

# [110] NB-DOUBLE-TEXT 
function nb_double_text($inp, $n, $c) {
    return (& { if ($c -eq "FLOW-OUT") { return (nb_double_multi_line $inp $n) }
        elseif ($c -eq "FLOW-IN") { return (nb_double_multi_line $inp $n) }
        elseif ($c -eq "BLOCK-KEY") { return (nb_double_one_line $inp) }
        elseif ($c -eq "FLOW-KEY") { return (nb_double_one_line $inp) }
        else { return (fail $inp "no case") } })
}

# [111] NB-DOUBLE-ONE-LINE 
function nb_double_one_line($inp) {
    return (peg_star $inp { param($inp) nb_double_char $inp }.GetNewClosure())
}

# [112] S-DOUBLE-ESCAPED 
function s_double_escaped($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) match_cp $inp 92 }.GetNewClosure(),
        { param($inp) b_non_content $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) l_empty $inp $n "FLOW-IN" }.GetNewClosure() }.GetNewClosure(),
        { param($inp) s_flow_line_prefix $inp $n }.GetNewClosure()))
}

# [113] S-DOUBLE-BREAK 
function s_double_break($inp, $n) {
    return (peg_alt $inp @(
        { param($inp) s_double_escaped $inp $n }.GetNewClosure(),
        { param($inp) s_flow_folded $inp $n }.GetNewClosure()))
}

# [114] NB-NS-DOUBLE-IN-LINE 
function nb_ns_double_in_line($inp) {
    return (peg_star $inp { param($inp) peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) ns_double_char $inp }.GetNewClosure()) }.GetNewClosure())
}

# [115] S-DOUBLE-NEXT-LINE 
function s_double_next_line($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_double_break $inp $n }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) ns_double_char $inp }.GetNewClosure(),
            { param($inp) nb_ns_double_in_line $inp }.GetNewClosure(),
            { param($inp) peg_alt $inp @(
                { param($inp) s_double_next_line $inp $n }.GetNewClosure(),
                { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [116] NB-DOUBLE-MULTI-LINE 
function nb_double_multi_line($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) nb_ns_double_in_line $inp }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) s_double_next_line $inp $n }.GetNewClosure(),
            { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()))
}

# [117] C-QUOTED-QUOTE 
function c_quoted_quote($inp) {
    return (match_str $inp "''")
}

# [118] NB-SINGLE-CHAR 
function nb_single_char($inp) {
    return (peg_alt $inp @(
        { param($inp) c_quoted_quote $inp }.GetNewClosure(),
        { param($inp) minus $inp { param($inp) nb_json $inp }.GetNewClosure() { param($inp) match_cp $inp 39 }.GetNewClosure() }.GetNewClosure()))
}

# [119] NS-SINGLE-CHAR 
function ns_single_char($inp) {
    return (minus $inp { param($inp) nb_single_char $inp }.GetNewClosure() { param($inp) s_white $inp }.GetNewClosure())
}

# [120] C-SINGLE-QUOTED 
function c_single_quoted($inp, $n, $c) {
    return (scalar $inp { param($inp) peg_seq $inp @(
        { param($inp) match_cp $inp 39 }.GetNewClosure(),
        { param($inp) nb_single_text $inp $n $c }.GetNewClosure(),
        { param($inp) match_cp $inp 39 }.GetNewClosure()) }.GetNewClosure())
}

# [121] NB-SINGLE-TEXT 
function nb_single_text($inp, $n, $c) {
    return (& { if ($c -eq "FLOW-OUT") { return (nb_single_multi_line $inp $n) }
        elseif ($c -eq "FLOW-IN") { return (nb_single_multi_line $inp $n) }
        elseif ($c -eq "BLOCK-KEY") { return (nb_single_one_line $inp) }
        elseif ($c -eq "FLOW-KEY") { return (nb_single_one_line $inp) }
        else { return (fail $inp "no case") } })
}

# [122] NB-SINGLE-ONE-LINE 
function nb_single_one_line($inp) {
    return (peg_star $inp { param($inp) nb_single_char $inp }.GetNewClosure())
}

# [123] NS-SINGLE-IN-LINE 
function ns_single_in_line($inp) {
    return (peg_star $inp { param($inp) peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) ns_single_char $inp }.GetNewClosure()) }.GetNewClosure())
}

# [124] S-SINGLE-NEXT-LINE 
function s_single_next_line($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_flow_folded $inp $n }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) ns_single_char $inp }.GetNewClosure(),
            { param($inp) ns_single_in_line $inp }.GetNewClosure(),
            { param($inp) peg_alt $inp @(
                { param($inp) s_single_next_line $inp $n }.GetNewClosure(),
                { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [125] NB-SINGLE-MULTI-LINE 
function nb_single_multi_line($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) ns_single_in_line $inp }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) s_single_next_line $inp $n }.GetNewClosure(),
            { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()))
}

# [126] NS-PLAIN-FIRST 
function ns_plain_first($inp, $c) {
    return (peg_alt $inp @(
        { param($inp) minus $inp { param($inp) ns_char $inp }.GetNewClosure() { param($inp) c_indicator $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) peg_alt $inp @(
                { param($inp) match_cp $inp 63 }.GetNewClosure(),
                { param($inp) match_cp $inp 58 }.GetNewClosure(),
                { param($inp) match_cp $inp 45 }.GetNewClosure()) }.GetNewClosure(),
            { param($inp) ahead $inp { param($inp) ns_plain_safe $inp $c }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()))
}

# [127] NS-PLAIN-SAFE 
function ns_plain_safe($inp, $c) {
    return (& { if ($c -eq "FLOW-OUT") { return (ns_plain_safe_out $inp) }
        elseif ($c -eq "FLOW-IN") { return (ns_plain_safe_in $inp) }
        elseif ($c -eq "BLOCK-KEY") { return (ns_plain_safe_out $inp) }
        elseif ($c -eq "FLOW-KEY") { return (ns_plain_safe_in $inp) }
        else { return (fail $inp "no case") } })
}

# [128] NS-PLAIN-SAFE-OUT 
function ns_plain_safe_out($inp) {
    return (ns_char $inp)
}

# [129] NS-PLAIN-SAFE-IN 
function ns_plain_safe_in($inp) {
    return (minus $inp { param($inp) ns_char $inp }.GetNewClosure() { param($inp) c_flow_indicator $inp }.GetNewClosure())
}

# [130] NS-PLAIN-CHAR 
function ns_plain_char($inp, $c) {
    return (peg_alt $inp @(
        { param($inp) minus $inp { param($inp) ns_plain_safe $inp $c }.GetNewClosure() { param($inp) peg_alt $inp @(
            { param($inp) match_cp $inp 58 }.GetNewClosure(),
            { param($inp) match_cp $inp 35 }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) behind $inp { param($inp) ns_char $inp }.GetNewClosure() }.GetNewClosure(),
            { param($inp) match_cp $inp 35 }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 58 }.GetNewClosure(),
            { param($inp) ahead $inp { param($inp) ns_plain_safe $inp $c }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()))
}

# [131] NS-PLAIN 
function ns_plain($inp, $n, $c) {
    return (scalar $inp { param($inp) & { if ($c -eq "FLOW-OUT") { return (ns_plain_multi_line $inp $n $c) }
        elseif ($c -eq "FLOW-IN") { return (ns_plain_multi_line $inp $n $c) }
        elseif ($c -eq "BLOCK-KEY") { return (ns_plain_one_line $inp $c) }
        elseif ($c -eq "FLOW-KEY") { return (ns_plain_one_line $inp $c) }
        else { return (fail $inp "no case") } } }.GetNewClosure())
}

# [132] NB-NS-PLAIN-IN-LINE 
function nb_ns_plain_in_line($inp, $c) {
    return (peg_star $inp { param($inp) peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) s_white $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) ns_plain_char $inp $c }.GetNewClosure()) }.GetNewClosure())
}

# [133] NS-PLAIN-ONE-LINE 
function ns_plain_one_line($inp, $c) {
    return (peg_seq $inp @(
        { param($inp) ns_plain_first $inp $c }.GetNewClosure(),
        { param($inp) nb_ns_plain_in_line $inp $c }.GetNewClosure()))
}

# [134] S-NS-PLAIN-NEXT-LINE 
function s_ns_plain_next_line($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) s_flow_folded $inp $n }.GetNewClosure(),
        { param($inp) neg $inp { param($inp) c_forbidden $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) ns_plain_char $inp $c }.GetNewClosure(),
        { param($inp) nb_ns_plain_in_line $inp $c }.GetNewClosure()))
}

# [135] NS-PLAIN-MULTI-LINE 
function ns_plain_multi_line($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) ns_plain_one_line $inp $c }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) s_ns_plain_next_line $inp $n $c }.GetNewClosure() }.GetNewClosure()))
}

# [137] C-FLOW-SEQUENCE 
function c_flow_sequence($inp, $n, $c) {
    return (build $inp "SEQUENCE" { param($inp) peg_seq $inp @(
        { param($inp) match_cp $inp 91 }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) collect $inp { param($inp) ns_s_flow_seq_entries $inp $n (inFlow $c) }.GetNewClosure() }.GetNewClosure() }.GetNewClosure(),
        { param($inp) match_cp $inp 93 }.GetNewClosure()) }.GetNewClosure())
}

# [138] NS-S-FLOW-SEQ-ENTRIES 
function ns_s_flow_seq_entries($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) ns_flow_seq_entry $inp $n $c }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 44 }.GetNewClosure(),
            { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
            { param($inp) opt $inp { param($inp) ns_s_flow_seq_entries $inp $n $c }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [139] NS-FLOW-SEQ-ENTRY 
function ns_flow_seq_entry($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) ns_flow_pair $inp $n $c }.GetNewClosure(),
        { param($inp) ns_flow_node $inp $n $c }.GetNewClosure()))
}

# [140] C-FLOW-MAPPING 
function c_flow_mapping($inp, $n, $c) {
    return (build $inp "MAPPING" { param($inp) peg_seq $inp @(
        { param($inp) match_cp $inp 123 }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) collect $inp { param($inp) ns_s_flow_map_entries $inp $n (inFlow $c) }.GetNewClosure() }.GetNewClosure() }.GetNewClosure(),
        { param($inp) match_cp $inp 125 }.GetNewClosure()) }.GetNewClosure())
}

# [141] NS-S-FLOW-MAP-ENTRIES 
function ns_s_flow_map_entries($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) ns_flow_map_entry $inp $n $c }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 44 }.GetNewClosure(),
            { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
            { param($inp) opt $inp { param($inp) ns_s_flow_map_entries $inp $n $c }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [142] NS-FLOW-MAP-ENTRY 
function ns_flow_map_entry($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 63 }.GetNewClosure(),
            { param($inp) s_separate $inp $n $c }.GetNewClosure(),
            { param($inp) ns_flow_map_explicit_entry $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) ns_flow_map_implicit_entry $inp $n $c }.GetNewClosure()))
}

# [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
function ns_flow_map_explicit_entry($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) ns_flow_map_implicit_entry $inp $n $c }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) e_node $inp }.GetNewClosure(),
            { param($inp) e_node $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
function ns_flow_map_implicit_entry($inp, $n, $c) {
    return (build $inp "PAIR" { param($inp) peg_alt $inp @(
        { param($inp) ns_flow_map_yaml_key_entry $inp $n $c }.GetNewClosure(),
        { param($inp) c_ns_flow_map_empty_key_entry $inp $n $c }.GetNewClosure(),
        { param($inp) c_ns_flow_map_json_key_entry $inp $n $c }.GetNewClosure()) }.GetNewClosure())
}

# [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
function ns_flow_map_yaml_key_entry($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) ns_flow_yaml_node $inp $n $c }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) peg_seq $inp @(
                { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
                { param($inp) c_ns_flow_map_separate_value $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
            { param($inp) e_node $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
function c_ns_flow_map_empty_key_entry($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) e_node $inp }.GetNewClosure(),
        { param($inp) c_ns_flow_map_separate_value $inp $n $c }.GetNewClosure()))
}

# [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
function c_ns_flow_map_separate_value($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 58 }.GetNewClosure(),
        { param($inp) neg $inp { param($inp) ns_plain_safe $inp $c }.GetNewClosure() }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) peg_seq $inp @(
                { param($inp) s_separate $inp $n $c }.GetNewClosure(),
                { param($inp) ns_flow_node $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
            { param($inp) e_node $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
function c_ns_flow_map_json_key_entry($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) c_flow_json_node $inp $n $c }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) peg_seq $inp @(
                { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
                { param($inp) c_ns_flow_map_adjacent_value $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
            { param($inp) e_node $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
function c_ns_flow_map_adjacent_value($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 58 }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) peg_seq $inp @(
                { param($inp) opt $inp { param($inp) s_separate $inp $n $c }.GetNewClosure() }.GetNewClosure(),
                { param($inp) ns_flow_node $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
            { param($inp) e_node $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [150] NS-FLOW-PAIR 
function ns_flow_pair($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) peg_seq $inp @(
            { param($inp) match_cp $inp 63 }.GetNewClosure(),
            { param($inp) s_separate $inp $n $c }.GetNewClosure(),
            { param($inp) ns_flow_map_explicit_entry $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) ns_flow_pair_entry $inp $n $c }.GetNewClosure()))
}

# [151] NS-FLOW-PAIR-ENTRY 
function ns_flow_pair_entry($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) ns_flow_pair_yaml_key_entry $inp $n $c }.GetNewClosure(),
        { param($inp) c_ns_flow_map_empty_key_entry $inp $n $c }.GetNewClosure(),
        { param($inp) c_ns_flow_pair_json_key_entry $inp $n $c }.GetNewClosure()))
}

# [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
function ns_flow_pair_yaml_key_entry($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) ns_s_implicit_yaml_key $inp "FLOW-KEY" }.GetNewClosure(),
        { param($inp) c_ns_flow_map_separate_value $inp $n $c }.GetNewClosure()))
}

# [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
function c_ns_flow_pair_json_key_entry($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) c_s_implicit_json_key $inp "FLOW-KEY" }.GetNewClosure(),
        { param($inp) c_ns_flow_map_adjacent_value $inp $n $c }.GetNewClosure()))
}

# [154] NS-S-IMPLICIT-YAML-KEY 
function ns_s_implicit_yaml_key($inp, $c) {
    return (peg_seq $inp @(
        { param($inp) ns_flow_yaml_node $inp 0 $c }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) s_separate_in_line $inp }.GetNewClosure() }.GetNewClosure()))
}

# [155] C-S-IMPLICIT-JSON-KEY 
function c_s_implicit_json_key($inp, $c) {
    return (peg_seq $inp @(
        { param($inp) c_flow_json_node $inp 0 $c }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) s_separate_in_line $inp }.GetNewClosure() }.GetNewClosure()))
}

# [156] NS-FLOW-YAML-CONTENT 
function ns_flow_yaml_content($inp, $n, $c) {
    return (ns_plain $inp $n $c)
}

# [157] C-FLOW-JSON-CONTENT 
function c_flow_json_content($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) c_flow_sequence $inp $n $c }.GetNewClosure(),
        { param($inp) c_flow_mapping $inp $n $c }.GetNewClosure(),
        { param($inp) c_single_quoted $inp $n $c }.GetNewClosure(),
        { param($inp) c_double_quoted $inp $n $c }.GetNewClosure()))
}

# [158] NS-FLOW-CONTENT 
function ns_flow_content($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) ns_flow_yaml_content $inp $n $c }.GetNewClosure(),
        { param($inp) c_flow_json_content $inp $n $c }.GetNewClosure()))
}

# [159] NS-FLOW-YAML-NODE 
function ns_flow_yaml_node($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) c_ns_alias_node $inp }.GetNewClosure(),
        { param($inp) ns_flow_yaml_content $inp $n $c }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) c_ns_properties $inp $n $c }.GetNewClosure(),
            { param($inp) peg_alt $inp @(
                { param($inp) peg_seq $inp @(
                    { param($inp) s_separate $inp $n $c }.GetNewClosure(),
                    { param($inp) ns_flow_yaml_content $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
                { param($inp) e_scalar $inp }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure()))
}

# [160] C-FLOW-JSON-NODE 
function c_flow_json_node($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) c_ns_properties $inp $n $c }.GetNewClosure(),
            { param($inp) s_separate $inp $n $c }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) c_flow_json_content $inp $n $c }.GetNewClosure()))
}

# [161] NS-FLOW-NODE 
function ns_flow_node($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) c_ns_alias_node $inp }.GetNewClosure(),
        { param($inp) ns_flow_content $inp $n $c }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) c_ns_properties $inp $n $c }.GetNewClosure(),
            { param($inp) peg_alt $inp @(
                { param($inp) peg_seq $inp @(
                    { param($inp) s_separate $inp $n $c }.GetNewClosure(),
                    { param($inp) ns_flow_content $inp $n $c }.GetNewClosure()) }.GetNewClosure(),
                { param($inp) e_scalar $inp }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure()))
}

# [162] C-B-BLOCK-HEADER 
function c_b_block_header($inp, $n) {
    return (peg_alt $inp @(
        { param($inp) & { $r = peg_alt $inp @(
            { param($inp) parse_int $inp { param($inp) ns_dec_digit $inp }.GetNewClosure() }.GetNewClosure(),
            { param($inp) detect_indent $inp $n }.GetNewClosure()); if ($r.fail) { return $r }; $m = $r.tagInt; $inp = $r.rest; & { $r = peg_alt $inp @(
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 45 }.GetNewClosure() "STRIP" }.GetNewClosure(),
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 43 }.GetNewClosure() "KEEP" }.GetNewClosure(),
            { param($inp) val $inp "CLIP" }.GetNewClosure()); if ($r.fail) { return $r }; $t = $r.tag; $inp = $r.rest; s_b_comment $inp } } }.GetNewClosure(),
        { param($inp) & { $r = peg_alt $inp @(
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 45 }.GetNewClosure() "STRIP" }.GetNewClosure(),
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 43 }.GetNewClosure() "KEEP" }.GetNewClosure(),
            { param($inp) val $inp "CLIP" }.GetNewClosure()); if ($r.fail) { return $r }; $t = $r.tag; $inp = $r.rest; & { $r = peg_alt $inp @(
            { param($inp) parse_int $inp { param($inp) ns_dec_digit $inp }.GetNewClosure() }.GetNewClosure(),
            { param($inp) detect_indent $inp $n }.GetNewClosure()); if ($r.fail) { return $r }; $m = $r.tagInt; $inp = $r.rest; s_b_comment $inp } } }.GetNewClosure()))
}

# [163] C-INDENTATION-INDICATOR 
function c_indentation_indicator($inp, $n) {
    return (peg_alt $inp @(
        { param($inp) ns_dec_digit $inp }.GetNewClosure(),
        { param($inp) ok $inp }.GetNewClosure()))
}

# [164] C-CHOMPING-INDICATOR 
function c_chomping_indicator($inp) {
    return (peg_alt $inp @(
        { param($inp) match_cp $inp 45 }.GetNewClosure(),
        { param($inp) match_cp $inp 43 }.GetNewClosure(),
        { param($inp) ok $inp }.GetNewClosure()))
}

# [165] B-CHOMPED-LAST 
function b_chomped_last($inp, $t) {
    return (& { if ($t -eq "STRIP") { return (b_non_content $inp) }
        elseif ($t -eq "CLIP") { return (b_as_line_feed $inp) }
        elseif ($t -eq "KEEP") { return (b_as_line_feed $inp) }
        else { return (fail $inp "no case") } })
}

# [166] L-CHOMPED-EMPTY 
function l_chomped_empty($inp, $n, $t) {
    return (& { if ($t -eq "STRIP") { return (l_strip_empty $inp $n) }
        elseif ($t -eq "CLIP") { return (l_strip_empty $inp $n) }
        elseif ($t -eq "KEEP") { return (l_keep_empty $inp $n) }
        else { return (fail $inp "no case") } })
}

# [167] L-STRIP-EMPTY 
function l_strip_empty($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) s_indent_le $inp $n }.GetNewClosure(),
            { param($inp) b_non_content $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) l_trail_comments $inp $n }.GetNewClosure() }.GetNewClosure()))
}

# [168] L-KEEP-EMPTY 
function l_keep_empty($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) l_empty $inp $n "BLOCK-IN" }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) l_trail_comments $inp $n }.GetNewClosure() }.GetNewClosure()))
}

# [169] L-TRAIL-COMMENTS 
function l_trail_comments($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_indent_lt $inp $n }.GetNewClosure(),
        { param($inp) c_nb_comment_text $inp }.GetNewClosure(),
        { param($inp) b_comment $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) l_comment $inp }.GetNewClosure() }.GetNewClosure()))
}

# [170] C-L+LITERAL 
function c_lliteral($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 124 }.GetNewClosure(),
        { param($inp) & { $r = peg_alt $inp @(
            { param($inp) parse_int $inp { param($inp) ns_dec_digit $inp }.GetNewClosure() }.GetNewClosure(),
            { param($inp) detect_indent $inp $n }.GetNewClosure()); if ($r.fail) { return $r }; $m = $r.tagInt; $inp = $r.rest; & { $r = peg_alt $inp @(
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 45 }.GetNewClosure() "STRIP" }.GetNewClosure(),
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 43 }.GetNewClosure() "KEEP" }.GetNewClosure(),
            { param($inp) val $inp "CLIP" }.GetNewClosure()); if ($r.fail) { return $r }; $t = $r.tag; $inp = $r.rest; peg_seq $inp @(
            { param($inp) s_b_comment $inp }.GetNewClosure(),
            { param($inp) l_literal_content $inp ($n + $m) $t }.GetNewClosure()) } } }.GetNewClosure()))
}

# [171] L-NB-LITERAL-TEXT 
function l_nb_literal_text($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) l_empty $inp $n "BLOCK-IN" }.GetNewClosure() }.GetNewClosure(),
        { param($inp) s_indent $inp $n }.GetNewClosure(),
        { param($inp) plus_ $inp { param($inp) nb_char $inp }.GetNewClosure() }.GetNewClosure()))
}

# [172] B-NB-LITERAL-NEXT 
function b_nb_literal_next($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) b_as_line_feed $inp }.GetNewClosure(),
        { param($inp) l_nb_literal_text $inp $n }.GetNewClosure()))
}

# [173] L-LITERAL-CONTENT 
function l_literal_content($inp, $n, $t) {
    return (scalar $inp { param($inp) peg_seq $inp @(
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) l_nb_literal_text $inp $n }.GetNewClosure(),
            { param($inp) peg_star $inp { param($inp) b_nb_literal_next $inp $n }.GetNewClosure() }.GetNewClosure(),
            { param($inp) b_chomped_last $inp $t }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) l_chomped_empty $inp $n $t }.GetNewClosure()) }.GetNewClosure())
}

# [174] C-L+FOLDED 
function c_lfolded($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 62 }.GetNewClosure(),
        { param($inp) & { $r = peg_alt $inp @(
            { param($inp) parse_int $inp { param($inp) ns_dec_digit $inp }.GetNewClosure() }.GetNewClosure(),
            { param($inp) detect_indent $inp $n }.GetNewClosure()); if ($r.fail) { return $r }; $m = $r.tagInt; $inp = $r.rest; & { $r = peg_alt $inp @(
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 45 }.GetNewClosure() "STRIP" }.GetNewClosure(),
            { param($inp) parse_sym $inp { param($inp) match_cp $inp 43 }.GetNewClosure() "KEEP" }.GetNewClosure(),
            { param($inp) val $inp "CLIP" }.GetNewClosure()); if ($r.fail) { return $r }; $t = $r.tag; $inp = $r.rest; peg_seq $inp @(
            { param($inp) s_b_comment $inp }.GetNewClosure(),
            { param($inp) l_folded_content $inp ($n + $m) $t }.GetNewClosure()) } } }.GetNewClosure()))
}

# [175] S-NB-FOLDED-TEXT 
function s_nb_folded_text($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_indent $inp $n }.GetNewClosure(),
        { param($inp) ns_char $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) nb_char $inp }.GetNewClosure() }.GetNewClosure()))
}

# [176] L-NB-FOLDED-LINES 
function l_nb_folded_lines($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_nb_folded_text $inp $n }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) b_l_folded $inp $n "BLOCK-IN" }.GetNewClosure(),
            { param($inp) s_nb_folded_text $inp $n }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [177] S-NB-SPACED-TEXT 
function s_nb_spaced_text($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_indent $inp $n }.GetNewClosure(),
        { param($inp) s_white $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) nb_char $inp }.GetNewClosure() }.GetNewClosure()))
}

# [178] B-L-SPACED 
function b_l_spaced($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) b_as_line_feed $inp }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) l_empty $inp $n "BLOCK-IN" }.GetNewClosure() }.GetNewClosure()))
}

# [179] L-NB-SPACED-LINES 
function l_nb_spaced_lines($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_nb_spaced_text $inp $n }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) b_l_spaced $inp $n }.GetNewClosure(),
            { param($inp) s_nb_spaced_text $inp $n }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [180] L-NB-SAME-LINES 
function l_nb_same_lines($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) l_empty $inp $n "BLOCK-IN" }.GetNewClosure() }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) l_nb_folded_lines $inp $n }.GetNewClosure(),
            { param($inp) l_nb_spaced_lines $inp $n }.GetNewClosure()) }.GetNewClosure()))
}

# [181] L-NB-DIFF-LINES 
function l_nb_diff_lines($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) l_nb_same_lines $inp $n }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) b_as_line_feed $inp }.GetNewClosure(),
            { param($inp) l_nb_same_lines $inp $n }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [182] L-FOLDED-CONTENT 
function l_folded_content($inp, $n, $t) {
    return (scalar $inp { param($inp) peg_seq $inp @(
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) l_nb_diff_lines $inp $n }.GetNewClosure(),
            { param($inp) b_chomped_last $inp $t }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) l_chomped_empty $inp $n $t }.GetNewClosure()) }.GetNewClosure())
}

# [183] L+BLOCK-SEQUENCE 
function lblock_sequence($inp, $n) {
    return (build $inp "SEQUENCE" { param($inp) & { $r = detect_indent $inp $n; if ($r.fail) { return $r }; $m = $r.tagInt; $inp = $r.rest; collect $inp { param($inp) plus_ $inp { param($inp) peg_seq $inp @(
        { param($inp) s_indent $inp ($n + $m) }.GetNewClosure(),
        { param($inp) c_l_block_seq_entry $inp ($n + $m) }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure() } }.GetNewClosure())
}

# [184] C-L-BLOCK-SEQ-ENTRY 
function c_l_block_seq_entry($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 45 }.GetNewClosure(),
        { param($inp) neg $inp { param($inp) ns_char $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) s_lblock_indented $inp $n "BLOCK-IN" }.GetNewClosure()))
}

# [185] S-L+BLOCK-INDENTED 
function s_lblock_indented($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) & { $r = detect_indent $inp 0; if ($r.fail) { return $r }; $m = $r.tagInt; $inp = $r.rest; peg_seq $inp @(
            { param($inp) s_indent $inp $m }.GetNewClosure(),
            { param($inp) peg_alt $inp @(
                { param($inp) ns_l_compact_sequence $inp ($n + 1 + $m) }.GetNewClosure(),
                { param($inp) ns_l_compact_mapping $inp ($n + 1 + $m) }.GetNewClosure()) }.GetNewClosure()) } }.GetNewClosure(),
        { param($inp) s_lblock_node $inp $n $c }.GetNewClosure(),
        { param($inp) peg_seq $inp @(
            { param($inp) e_node $inp }.GetNewClosure(),
            { param($inp) s_l_comments $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [186] NS-L-COMPACT-SEQUENCE 
function ns_l_compact_sequence($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) c_l_block_seq_entry $inp $n }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) s_indent $inp $n }.GetNewClosure(),
            { param($inp) c_l_block_seq_entry $inp $n }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [187] L+BLOCK-MAPPING 
function lblock_mapping($inp, $n) {
    return (build $inp "MAPPING" { param($inp) & { $r = detect_indent $inp $n; if ($r.fail) { return $r }; $m = $r.tagInt; $inp = $r.rest; collect $inp { param($inp) plus_ $inp { param($inp) peg_seq $inp @(
        { param($inp) s_indent $inp ($n + $m) }.GetNewClosure(),
        { param($inp) ns_l_block_map_entry $inp ($n + $m) }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure() } }.GetNewClosure())
}

# [188] NS-L-BLOCK-MAP-ENTRY 
function ns_l_block_map_entry($inp, $n) {
    return (peg_alt $inp @(
        { param($inp) c_l_block_map_explicit_entry $inp $n }.GetNewClosure(),
        { param($inp) ns_l_block_map_implicit_entry $inp $n }.GetNewClosure()))
}

# [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
function c_l_block_map_explicit_entry($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) c_l_block_map_explicit_key $inp $n }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) l_block_map_explicit_value $inp $n }.GetNewClosure(),
            { param($inp) e_node $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
function c_l_block_map_explicit_key($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 63 }.GetNewClosure(),
        { param($inp) s_lblock_indented $inp $n "BLOCK-OUT" }.GetNewClosure()))
}

# [191] L-BLOCK-MAP-EXPLICIT-VALUE 
function l_block_map_explicit_value($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_indent $inp $n }.GetNewClosure(),
        { param($inp) match_cp $inp 58 }.GetNewClosure(),
        { param($inp) s_lblock_indented $inp $n "BLOCK-OUT" }.GetNewClosure()))
}

# [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
function ns_l_block_map_implicit_entry($inp, $n) {
    return (build $inp "PAIR" { param($inp) peg_seq $inp @(
        { param($inp) scalar $inp { param($inp) peg_alt $inp @(
            { param($inp) ns_s_block_map_implicit_key $inp }.GetNewClosure(),
            { param($inp) e_node $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) c_l_block_map_implicit_value $inp $n }.GetNewClosure()) }.GetNewClosure())
}

# [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
function ns_s_block_map_implicit_key($inp) {
    return (peg_alt $inp @(
        { param($inp) c_s_implicit_json_key $inp "BLOCK-KEY" }.GetNewClosure(),
        { param($inp) ns_s_implicit_yaml_key $inp "BLOCK-KEY" }.GetNewClosure()))
}

# [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
function c_l_block_map_implicit_value($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) match_cp $inp 58 }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) s_lblock_node $inp $n "BLOCK-OUT" }.GetNewClosure(),
            { param($inp) scalar $inp { param($inp) peg_seq $inp @(
                { param($inp) e_node $inp }.GetNewClosure(),
                { param($inp) s_l_comments $inp }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()))
}

# [195] NS-L-COMPACT-MAPPING 
function ns_l_compact_mapping($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) ns_l_block_map_entry $inp $n }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_seq $inp @(
            { param($inp) s_indent $inp $n }.GetNewClosure(),
            { param($inp) ns_l_block_map_entry $inp $n }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()))
}

# [196] S-L+BLOCK-NODE 
function s_lblock_node($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) s_lblock_in_block $inp $n $c }.GetNewClosure(),
        { param($inp) s_lflow_in_block $inp $n }.GetNewClosure()))
}

# [197] S-L+FLOW-IN-BLOCK 
function s_lflow_in_block($inp, $n) {
    return (peg_seq $inp @(
        { param($inp) s_separate $inp ($n + 1) "FLOW-OUT" }.GetNewClosure(),
        { param($inp) ns_flow_node $inp ($n + 1) "FLOW-OUT" }.GetNewClosure(),
        { param($inp) s_l_comments $inp }.GetNewClosure()))
}

# [198] S-L+BLOCK-IN-BLOCK 
function s_lblock_in_block($inp, $n, $c) {
    return (peg_alt $inp @(
        { param($inp) s_lblock_scalar $inp $n $c }.GetNewClosure(),
        { param($inp) s_lblock_collection $inp $n $c }.GetNewClosure()))
}

# [199] S-L+BLOCK-SCALAR 
function s_lblock_scalar($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) s_separate $inp ($n + 1) $c }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) c_ns_properties $inp ($n + 1) $c }.GetNewClosure(),
            { param($inp) s_separate $inp ($n + 1) $c }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) c_lliteral $inp $n }.GetNewClosure(),
            { param($inp) c_lfolded $inp $n }.GetNewClosure()) }.GetNewClosure()))
}

# [200] S-L+BLOCK-COLLECTION 
function s_lblock_collection($inp, $n, $c) {
    return (peg_seq $inp @(
        { param($inp) opt $inp { param($inp) peg_seq $inp @(
            { param($inp) s_separate $inp ($n + 1) $c }.GetNewClosure(),
            { param($inp) c_ns_properties $inp ($n + 1) $c }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure(),
        { param($inp) s_l_comments $inp }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) lblock_sequence $inp (seqSpaces $n $c) }.GetNewClosure(),
            { param($inp) lblock_mapping $inp $n }.GetNewClosure()) }.GetNewClosure()))
}

# [202] L-DOCUMENT-PREFIX 
function l_document_prefix($inp) {
    return (peg_seq $inp @(
        { param($inp) opt $inp { param($inp) c_byte_order_mark $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) l_comment $inp }.GetNewClosure() }.GetNewClosure()))
}

# [203] C-DIRECTIVES-END 
function c_directives_end($inp) {
    return (match_str $inp "---")
}

# [204] C-DOCUMENT-END 
function c_document_end($inp) {
    return (match_str $inp "...")
}

# [205] L-DOCUMENT-SUFFIX 
function l_document_suffix($inp) {
    return (peg_seq $inp @(
        { param($inp) c_document_end $inp }.GetNewClosure(),
        { param($inp) s_l_comments $inp }.GetNewClosure()))
}

# [206] C-FORBIDDEN 
function c_forbidden($inp) {
    return (peg_seq $inp @(
        { param($inp) sol $inp }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) c_directives_end $inp }.GetNewClosure(),
            { param($inp) c_document_end $inp }.GetNewClosure()) }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) b_char $inp }.GetNewClosure(),
            { param($inp) s_white $inp }.GetNewClosure(),
            { param($inp) eof_ok $inp }.GetNewClosure()) }.GetNewClosure()))
}

# [207] L-BARE-DOCUMENT 
function l_bare_document($inp) {
    return (build $inp "DOC" { param($inp) s_lblock_node $inp -1 "BLOCK-IN" }.GetNewClosure())
}

# [208] L-EXPLICIT-DOCUMENT 
function l_explicit_document($inp) {
    return (build $inp "DOC" { param($inp) peg_seq $inp @(
        { param($inp) c_directives_end $inp }.GetNewClosure(),
        { param($inp) peg_alt $inp @(
            { param($inp) l_bare_document $inp }.GetNewClosure(),
            { param($inp) peg_seq $inp @(
                { param($inp) e_node $inp }.GetNewClosure(),
                { param($inp) s_l_comments $inp }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure())
}

# [209] L-DIRECTIVE-DOCUMENT 
function l_directive_document($inp) {
    return (peg_seq $inp @(
        { param($inp) plus_ $inp { param($inp) l_directive $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) l_explicit_document $inp }.GetNewClosure()))
}

# [210] L-ANY-DOCUMENT 
function l_any_document($inp) {
    return (peg_alt $inp @(
        { param($inp) l_directive_document $inp }.GetNewClosure(),
        { param($inp) l_explicit_document $inp }.GetNewClosure(),
        { param($inp) l_bare_document $inp }.GetNewClosure()))
}

# [211] L-YAML-STREAM 
function l_yaml_stream($inp) {
    return (build $inp "STREAM" { param($inp) peg_seq $inp @(
        { param($inp) peg_star $inp { param($inp) l_document_prefix $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) opt $inp { param($inp) l_any_document $inp }.GetNewClosure() }.GetNewClosure(),
        { param($inp) peg_star $inp { param($inp) peg_alt $inp @(
            { param($inp) peg_seq $inp @(
                { param($inp) plus_ $inp { param($inp) l_document_suffix $inp }.GetNewClosure() }.GetNewClosure(),
                { param($inp) peg_star $inp { param($inp) l_document_prefix $inp }.GetNewClosure() }.GetNewClosure(),
                { param($inp) opt $inp { param($inp) l_any_document $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure(),
            { param($inp) peg_seq $inp @(
                { param($inp) peg_star $inp { param($inp) l_document_prefix $inp }.GetNewClosure() }.GetNewClosure(),
                { param($inp) opt $inp { param($inp) l_explicit_document $inp }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure()) }.GetNewClosure() }.GetNewClosure()) }.GetNewClosure())
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
$r = l_yaml_stream $inp
if (-not $r.fail) {
    Write-Host "OK: $($r.rest.pos) chars"
    if ($null -ne $r.ast) { printAst $r.ast 0 }
} else {
    [Console]::Error.WriteLine("FAIL @$($r.rest.pos): $($r.err)")
    exit 1
}
