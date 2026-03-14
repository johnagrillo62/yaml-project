#!/usr/bin/env bash
set -eo pipefail

# ── Input ──

SRC=""
POS=0; LINE=1; COL=0; LEN=0

init_input() {
    SRC="$1"; LEN=${#SRC}
    POS=0; LINE=1; COL=0
}

at_eof() { [[ $POS -ge $LEN ]]; }

peek_cp() {
    if [[ $POS -ge $LEN ]]; then PEEK=-1; return; fi
    LC_CTYPE=C printf -v PEEK '%d' "'${SRC:$POS:1}"
}

adv() {
    if [[ $POS -ge $LEN ]]; then return; fi
    local ch="${SRC:$POS:1}"
    POS=$((POS+1))
    if [[ "$ch" == $'\n' ]]; then
        LINE=$((LINE+1)); COL=0
    else
        COL=$((COL+1))
    fi
}

# Save/restore input position
save_inp()    { _SP=$POS; _SL=$LINE; _SC=$COL; }
restore_inp() { POS=$_SP; LINE=$_SL; COL=$_SC; }

# ── AST (flat parallel arrays) ──

AST_TAG=()    # tag or "SCALAR"
AST_TEXT=()   # leaf text (empty for branches)
AST_KIDS=()   # space-separated child indices
AST_LEAF=()   # 1=leaf, 0=branch
AST_N=0       # next free index

ast_branch() {
    local idx=$AST_N
    AST_TAG[$idx]="$1"; AST_TEXT[$idx]=""
    AST_KIDS[$idx]=""; AST_LEAF[$idx]=0
    AST_N=$((AST_N+1)); RESULT=$idx
}

ast_leaf() {
    local idx=$AST_N
    AST_TAG[$idx]="SCALAR"; AST_TEXT[$idx]="$1"
    AST_KIDS[$idx]=""; AST_LEAF[$idx]=1
    AST_N=$((AST_N+1)); RESULT=$idx
}

ast_add_child() {
    # ast_add_child parent child
    if [[ -z "${AST_KIDS[$1]}" ]]; then
        AST_KIDS[$1]="$2"
    else
        AST_KIDS[$1]="${AST_KIDS[$1]} $2"
    fi
}

# ── Result ──

FAILED=0
RVAL=""
RPOS=0; RLINE=0; RCOL=0
RTAG=""; RTAGINT=0
RAST=-1        # index into AST arrays, -1 = none
RAST_LIST=""   # space-separated AST indices

ok_r() {
    FAILED=0; RVAL=""; RPOS=$POS; RLINE=$LINE; RCOL=$COL
    RTAG=""; RTAGINT=0; RAST=-1; RAST_LIST=""
}

ok_v() {
    FAILED=0; RVAL="$1"; RPOS=$POS; RLINE=$LINE; RCOL=$COL
    RTAG=""; RTAGINT=0; RAST=-1; RAST_LIST=""
}

fail_r() {
    FAILED=1; RVAL=""; RPOS=$POS; RLINE=$LINE; RCOL=$COL
    RTAG=""; RTAGINT=0; RAST=-1; RAST_LIST=""
}

# ── Context ──

in_flow() {
    case "$1" in
        FLOW-OUT|FLOW-IN) echo "FLOW-IN" ;;
        *) echo "FLOW-KEY" ;;
    esac
}

seq_spaces() {
    if [[ "$2" == "BLOCK-OUT" ]]; then
        echo $(($1 - 1))
    else
        echo "$1"
    fi
}

# ── Combinators ──

match_cp() {
    peek_cp
    if [[ $PEEK -eq $1 ]]; then
        local ch="${SRC:$POS:1}"
        adv; ok_v "$ch"
    else
        fail_r "cp"
    fi
}

match_range() {
    peek_cp
    if [[ $PEEK -ge $1 && $PEEK -le $2 ]]; then
        local ch="${SRC:$POS:1}"
        adv; ok_v "$ch"
    else
        fail_r "rng"
    fi
}

match_str() {
    local t="$1" tl=${#1}
    if [[ "${SRC:$POS:$tl}" == "$t" ]]; then
        local i=0
        while [[ $i -lt $tl ]]; do adv; i=$((i+1)); done
        ok_v "$t"
    else
        fail_r "str"
    fi
}

peg_seq() {
    local acc="" sp=$POS sl=$LINE sc=$COL
    local asts="" _fn
    for _fn in "$@"; do
        "$_fn"
        if [[ $FAILED -eq 1 ]]; then
            POS=$sp; LINE=$sl; COL=$sc; return
        fi
        acc+="$RVAL"
        [[ $RAST -ge 0 ]] && asts+="$RAST "
        [[ -n "$RAST_LIST" ]] && asts+="$RAST_LIST "
    done
    ok_v "$acc"
    RAST_LIST="$asts"
}

peg_alt() {
    local sp=$POS sl=$LINE sc=$COL _fn
    for _fn in "$@"; do
        POS=$sp; LINE=$sl; COL=$sc
        "$_fn"
        if [[ $FAILED -eq 0 ]]; then return; fi
    done
    fail_r "alt"
}

star_p() {
    local fn="$1" acc="" asts=""
    local sp=$POS
    while true; do
        "$fn"
        if [[ $FAILED -eq 1 || $POS -le $sp ]]; then
            FAILED=0; break
        fi
        acc+="$RVAL"
        [[ $RAST -ge 0 ]] && asts+="$RAST "
        [[ -n "$RAST_LIST" ]] && asts+="$RAST_LIST "
        sp=$POS
    done
    ok_v "$acc"
    RAST_LIST="$asts"
}

plus_p() {
    local fn="$1"
    "$fn"
    if [[ $FAILED -eq 1 ]]; then return; fi
    local first_val="$RVAL" first_ast=""
    [[ $RAST -ge 0 ]] && first_ast="$RAST "
    [[ -n "$RAST_LIST" ]] && first_ast+="$RAST_LIST "
    star_p "$fn"
    RVAL="${first_val}${RVAL}"
    RAST_LIST="${first_ast}${RAST_LIST}"
}

opt_p() {
    local fn="$1" sp=$POS sl=$LINE sc=$COL
    "$fn"
    if [[ $FAILED -eq 1 ]]; then
        POS=$sp; LINE=$sl; COL=$sc; ok_r
    fi
}

neg_p() {
    local fn="$1" sp=$POS sl=$LINE sc=$COL
    "$fn"
    POS=$sp; LINE=$sl; COL=$sc
    if [[ $FAILED -eq 1 ]]; then ok_r
    else fail_r "neg"; fi
}

minus_p() {
    local fa="$1" fb="$2" sp=$POS sl=$LINE sc=$COL
    "$fa"
    if [[ $FAILED -eq 1 ]]; then return; fi
    local sv="$RVAL" spos=$POS sline=$LINE scol=$COL
    local sast=$RAST sal="$RAST_LIST"
    POS=$sp; LINE=$sl; COL=$sc
    "$fb"
    if [[ $FAILED -eq 0 ]]; then
        POS=$sp; LINE=$sl; COL=$sc
        fail_r "minus"
    else
        POS=$spos; LINE=$sline; COL=$scol
        ok_v "$sv"; RAST=$sast; RAST_LIST="$sal"
    fi
}

rep_p() {
    local lo=$1 hi=$2; shift 2; local fn="$1"
    local acc="" asts="" count=0
    while [[ $hi -eq -1 || $count -lt $hi ]]; do
        local sp=$POS
        "$fn"
        if [[ $FAILED -eq 1 || $POS -le $sp ]]; then FAILED=0; break; fi
        acc+="$RVAL"
        [[ $RAST -ge 0 ]] && asts+="$RAST "
        [[ -n "$RAST_LIST" ]] && asts+="$RAST_LIST "
        count=$((count+1))
    done
    if [[ $count -lt $lo ]]; then fail_r "rep"; return; fi
    ok_v "$acc"; RAST_LIST="$asts"
}

ahead_p() {
    local fn="$1" sp=$POS sl=$LINE sc=$COL
    "$fn"
    POS=$sp; LINE=$sl; COL=$sc
    if [[ $FAILED -eq 0 ]]; then ok_r; fi
}

behind_p() {
    local fn="$1"
    if [[ $POS -eq 0 ]]; then fail_r "bh"; return; fi
    local sp=$POS sl=$LINE sc=$COL
    POS=$((POS-1)); COL=$((COL > 0 ? COL-1 : 0))
    "$fn"
    POS=$sp; LINE=$sl; COL=$sc
    if [[ $FAILED -eq 0 ]]; then ok_r
    else fail_r "bh"; fi
}

sol_p() {
    if [[ $COL -eq 0 ]]; then ok_r
    else fail_r "sol"; fi
}

eof_p() {
    if at_eof; then ok_r
    else fail_r "eof"; fi
}

# ════════════════════════════════════════════════════════════════ 
# Wrapper functions (bash has no closures) 
# ════════════════════════════════════════════════════════════════ 

_w1() { ws; }
_w2() { r_value; }
_w3() { ws; }
_w4() { eof_p; }
_w5() { r_object; }
_w6() { r_array; }
_w7() { r_string; }
_w8() { r_number; }
_w9() { match_str "true"; }
_w10() { match_str "false"; }
_w11() { match_str "null"; }
_w12() { match_cp 123; }
_w13() { ws; }
_w14() { members; }
_w15() { ws; }
_w16() { match_cp 125; }
_w17() { peg_seq _w12 _w13 _w14 _w15 _w16; }
_w18() { match_cp 123; }
_w19() { ws; }
_w20() { match_cp 125; }
_w21() { peg_seq _w18 _w19 _w20; }
_w22() { member; }
_w23() { ws; }
_w24() { match_cp 44; }
_w25() { ws; }
_w26() { member; }
_w27() { peg_seq _w23 _w24 _w25 _w26; }
_w28() { star_p _w27; }
_w29() { ws; }
_w30() { r_string; }
_w31() { ws; }
_w32() { match_cp 58; }
_w33() { ws; }
_w34() { r_value; }
_w35() { ws; }
_w36() { match_cp 91; }
_w37() { ws; }
_w38() { elements; }
_w39() { ws; }
_w40() { match_cp 93; }
_w41() { peg_seq _w36 _w37 _w38 _w39 _w40; }
_w42() { match_cp 91; }
_w43() { ws; }
_w44() { match_cp 93; }
_w45() { peg_seq _w42 _w43 _w44; }
_w46() { r_value; }
_w47() { ws; }
_w48() { match_cp 44; }
_w49() { ws; }
_w50() { r_value; }
_w51() { peg_seq _w47 _w48 _w49 _w50; }
_w52() { star_p _w51; }
_w53() { match_cp 34; }
_w54() { r_char; }
_w55() { star_p _w54; }
_w56() { match_cp 34; }
_w57() { escaped; }
_w58() { match_cp 34; }
_w59() { neg_p _w58; }
_w60() { match_cp 92; }
_w61() { neg_p _w60; }
_w62() { match_cp 0x0; }
_w63() { neg_p _w62; }
_w64() { match_range 0x0 0x1F; }
_w65() { neg_p _w64; }
_w66() { match_range 0x20 0x10FFFF; }
_w67() { peg_seq _w59 _w61 _w63 _w65 _w66; }
_w68() { match_cp 92; }
_w69() { match_cp 34; }
_w70() { match_cp 92; }
_w71() { match_cp 47; }
_w72() { match_cp 98; }
_w73() { match_cp 102; }
_w74() { match_cp 110; }
_w75() { match_cp 114; }
_w76() { match_cp 116; }
_w77() { match_cp 117; }
_w78() { hex4; }
_w79() { peg_seq _w77 _w78; }
_w80() { peg_alt _w69 _w70 _w71 _w72 _w73 _w74 _w75 _w76 _w79; }
_w81() { hexdig; }
_w82() { hexdig; }
_w83() { hexdig; }
_w84() { hexdig; }
_w85() { match_range 48 57; }
_w86() { match_range 97 102; }
_w87() { match_range 65 70; }
_w88() { match_cp 45; }
_w89() { opt_p _w88; }
_w90() { r_integer; }
_w91() { fraction; }
_w92() { opt_p _w91; }
_w93() { exponent; }
_w94() { opt_p _w93; }
_w95() { match_cp 48; }
_w96() { match_range 49 57; }
_w97() { match_range 48 57; }
_w98() { star_p _w97; }
_w99() { peg_seq _w96 _w98; }
_w100() { match_cp 46; }
_w101() { match_range 48 57; }
_w102() { plus_p _w101; }
_w103() { match_cp 101; }
_w104() { match_cp 69; }
_w105() { peg_alt _w103 _w104; }
_w106() { match_cp 43; }
_w107() { match_cp 45; }
_w108() { peg_alt _w106 _w107; }
_w109() { opt_p _w108; }
_w110() { match_range 48 57; }
_w111() { plus_p _w110; }
_w112() { match_cp 0x20; }
_w113() { match_cp 0x9; }
_w114() { match_cp 0x0A; }
_w115() { match_cp 0x0D; }
_w116() { peg_alt _w112 _w113 _w114 _w115; }

# ════════════════════════════════════════════════════════════════ 
# YAML 1.2 Grammar — 211 rules 
# ════════════════════════════════════════════════════════════════ 

# [1] JSON-TEXT 
json_text() {
    peg_seq _w1 _w2 _w3 _w4
}

# [2] VALUE 
r_value() {
    peg_alt _w5 _w6 _w7 _w8 _w9 _w10 _w11
}

# [3] OBJECT 
r_object() {
    peg_alt _w17 _w21
}

# [4] MEMBERS 
members() {
    peg_seq _w22 _w28
}

# [5] MEMBER 
member() {
    peg_seq _w29 _w30 _w31 _w32 _w33 _w34 _w35
}

# [6] ARRAY 
r_array() {
    peg_alt _w41 _w45
}

# [7] ELEMENTS 
elements() {
    peg_seq _w46 _w52
}

# [8] STRING 
r_string() {
    peg_seq _w53 _w55 _w56
}

# [9] CHAR 
r_char() {
    peg_alt _w57 _w67
}

# [10] ESCAPED 
escaped() {
    peg_seq _w68 _w80
}

# [11] HEX4 
hex4() {
    peg_seq _w81 _w82 _w83 _w84
}

# [12] HEXDIG 
hexdig() {
    peg_alt _w85 _w86 _w87
}

# [13] NUMBER 
r_number() {
    peg_seq _w89 _w90 _w92 _w94
}

# [14] INTEGER 
r_integer() {
    peg_alt _w95 _w99
}

# [15] FRACTION 
fraction() {
    peg_seq _w100 _w102
}

# [16] EXPONENT 
exponent() {
    peg_seq _w105 _w109 _w111
}

# [17] WS 
ws() {
    star_p _w116
}

# ── API ──

print_ast() {
    local idx=$1 depth=$2
    local indent=""
    local d=0; while [[ $d -lt $depth ]]; do indent+="  "; d=$((d+1)); done
    if [[ ${AST_LEAF[$idx]} -eq 1 ]]; then
        echo "${indent}SCALAR: \"${AST_TEXT[$idx]}\""
    else
        echo "${indent}${AST_TAG[$idx]}"
        for child in ${AST_KIDS[$idx]}; do
            print_ast $child $((depth+1))
        done
    fi
}

# ── Main ──

usage() {
    echo "Usage: $0 [file]" >&2
    echo "  Reads JSON from file or stdin." >&2
    echo "  If no file given and stdin is a terminal, shows this help." >&2
    exit 1
}

main() {
    local text
    if [[ $# -gt 0 ]]; then
        [[ ! -f "$1" ]] && { echo "Error: file '$1' not found" >&2; exit 1; }
        text=$(<"$1")
    elif [[ -t 0 ]]; then
        # stdin is a terminal — no piped input
        usage
    else
        text=$(cat)
    fi

    init_input "$text"
    json_text

    if [[ $FAILED -eq 0 ]]; then
        echo "OK: $POS chars"
        [[ $RAST -ge 0 ]] && print_ast $RAST 0
        exit 0
    else
        echo "FAIL @$POS: $RTAG" >&2
        exit 1
    fi
}

main "$@"
