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

# ── YAML extensions ──

build_p() {
    local typ="$1"; shift; local fn="$1"
    "$fn"
    if [[ $FAILED -eq 1 ]]; then return; fi
    ast_branch "$typ"; local node=$RESULT
    for ci in $RAST_LIST; do
        [[ $ci -ge 0 ]] && ast_add_child $node $ci
    done
    if [[ $RAST -ge 0 ]]; then ast_add_child $node $RAST; fi
    RAST=$node; RAST_LIST=""
}

scalar_p() {
    local fn="$1"
    "$fn"
    if [[ $FAILED -eq 1 ]]; then return; fi
    ast_leaf "$RVAL"; RAST=$RESULT
}

collect_p() { "$1"; }

detect_indent() {
    local n=$1 i=$POS sp=0
    while [[ $((i+sp)) -lt $LEN && "${SRC:$((i+sp)):1}" == " " ]]; do
        sp=$((sp+1))
    done
    if [[ $((i+sp)) -lt $LEN && "${SRC:$((i+sp)):1}" != $'\n' ]]; then
        ok_r; RTAGINT=$(( sp - n > 1 ? sp - n : 1 ))
        return
    fi
    local j=$((i+sp))
    while [[ $j -lt $LEN ]]; do
        if [[ "${SRC:$j:1}" == $'\n' ]]; then
            j=$((j+1))
            [[ $j -ge $LEN ]] && { ok_r; RTAGINT=1; return; }
            sp=0
            while [[ $((j+sp)) -lt $LEN && "${SRC:$((j+sp)):1}" == " " ]]; do sp=$((sp+1)); done
            local nx=$((j+sp))
            if [[ $nx -ge $LEN || "${SRC:$nx:1}" == $'\n' ]]; then
                j=$nx; continue
            fi
            ok_r; RTAGINT=$(( sp - n > 1 ? sp - n : 1 ))
            return
        fi
        break
    done
    ok_r; RTAGINT=1
}

parse_int_p() {
    local fn="$1"
    "$fn"
    if [[ $FAILED -eq 1 ]]; then return; fi
    local digits="${RVAL//[!0-9]/}"
    RTAGINT=${digits:-0}
}

parse_sym_p() {
    local fn="$1" sym="$2"
    "$fn"
    if [[ $FAILED -eq 1 ]]; then return; fi
    RTAG="$sym"
}

val_p() { ok_r; RTAG="$1"; }

# ════════════════════════════════════════════════════════════════ 
# Wrapper functions (bash has no closures) 
# ════════════════════════════════════════════════════════════════ 

_w1() { match_cp 0x9; }
_w2() { match_cp 0x0A; }
_w3() { match_cp 0x0D; }
_w4() { match_range 0x20 0x7E; }
_w5() { match_cp 0x85; }
_w6() { match_range 0xA0 0xD7FF; }
_w7() { match_range 0xE000 0xFFFD; }
_w8() { match_range 0x10000 0x10FFFF; }
_w9() { match_cp 0x9; }
_w10() { match_range 0x20 0x10FFFF; }
_w11() { match_cp 64; }
_w12() { match_cp 96; }
_w13() { c_sequence_entry; }
_w14() { c_mapping_key; }
_w15() { c_mapping_value; }
_w16() { c_collect_entry; }
_w17() { c_sequence_start; }
_w18() { c_sequence_end; }
_w19() { c_mapping_start; }
_w20() { c_mapping_end; }
_w21() { c_comment; }
_w22() { c_anchor; }
_w23() { c_alias; }
_w24() { c_tag; }
_w25() { c_literal; }
_w26() { c_folded; }
_w27() { c_single_quote; }
_w28() { c_double_quote; }
_w29() { c_directive; }
_w30() { c_reserved; }
_w31() { c_collect_entry; }
_w32() { c_sequence_start; }
_w33() { c_sequence_end; }
_w34() { c_mapping_start; }
_w35() { c_mapping_end; }
_w36() { b_line_feed; }
_w37() { b_carriage_return; }
_w38() { c_printable; }
_w39() { b_char; }
_w40() { c_byte_order_mark; }
_w41() { peg_alt _w39 _w40; }
_w42() { b_carriage_return; }
_w43() { b_line_feed; }
_w44() { peg_seq _w42 _w43; }
_w45() { b_carriage_return; }
_w46() { b_line_feed; }
_w47() { s_space; }
_w48() { s_tab; }
_w49() { nb_char; }
_w50() { s_white; }
_w51() { ns_dec_digit; }
_w52() { match_range 0x41 0x46; }
_w53() { match_range 0x61 0x66; }
_w54() { match_range 0x41 0x5A; }
_w55() { match_range 0x61 0x7A; }
_w56() { ns_dec_digit; }
_w57() { ns_ascii_letter; }
_w58() { match_cp 45; }
_w59() { match_cp 37; }
_w60() { ns_hex_digit; }
_w61() { ns_hex_digit; }
_w62() { peg_seq _w59 _w60 _w61; }
_w63() { ns_word_char; }
_w64() { match_cp 35; }
_w65() { match_cp 59; }
_w66() { match_cp 47; }
_w67() { match_cp 63; }
_w68() { match_cp 58; }
_w69() { match_cp 64; }
_w70() { match_cp 38; }
_w71() { match_cp 61; }
_w72() { match_cp 43; }
_w73() { match_cp 36; }
_w74() { match_cp 44; }
_w75() { match_cp 95; }
_w76() { match_cp 46; }
_w77() { match_cp 33; }
_w78() { match_cp 126; }
_w79() { match_cp 42; }
_w80() { match_cp 39; }
_w81() { match_cp 40; }
_w82() { match_cp 41; }
_w83() { match_cp 91; }
_w84() { match_cp 93; }
_w85() { match_cp 37; }
_w86() { ns_hex_digit; }
_w87() { ns_hex_digit; }
_w88() { peg_seq _w85 _w86 _w87; }
_w89() { ns_word_char; }
_w90() { match_cp 35; }
_w91() { match_cp 59; }
_w92() { match_cp 47; }
_w93() { match_cp 63; }
_w94() { match_cp 58; }
_w95() { match_cp 64; }
_w96() { match_cp 38; }
_w97() { match_cp 61; }
_w98() { match_cp 43; }
_w99() { match_cp 36; }
_w100() { match_cp 44; }
_w101() { match_cp 95; }
_w102() { match_cp 46; }
_w103() { match_cp 33; }
_w104() { match_cp 126; }
_w105() { match_cp 42; }
_w106() { match_cp 39; }
_w107() { match_cp 40; }
_w108() { match_cp 41; }
_w109() { match_cp 91; }
_w110() { match_cp 93; }
_w111() { ns_uri_char; }
_w112() { c_tag; }
_w113() { c_flow_indicator; }
_w114() { peg_alt _w112 _w113; }
_w115() { match_cp 120; }
_w116() { ns_hex_digit; }
_w117() { rep_p 2 _w116; }
_w118() { match_cp 117; }
_w119() { ns_hex_digit; }
_w120() { rep_p 4 _w119; }
_w121() { match_cp 85; }
_w122() { ns_hex_digit; }
_w123() { rep_p 8 _w122; }
_w124() { c_escape; }
_w125() { ns_esc_null; }
_w126() { ns_esc_bell; }
_w127() { ns_esc_backspace; }
_w128() { ns_esc_horizontal_tab; }
_w129() { ns_esc_line_feed; }
_w130() { ns_esc_vertical_tab; }
_w131() { ns_esc_form_feed; }
_w132() { ns_esc_carriage_return; }
_w133() { ns_esc_escape; }
_w134() { ns_esc_space; }
_w135() { ns_esc_double_quote; }
_w136() { ns_esc_slash; }
_w137() { ns_esc_backslash; }
_w138() { ns_esc_next_line; }
_w139() { ns_esc_non_breaking_space; }
_w140() { ns_esc_line_separator; }
_w141() { ns_esc_paragraph_separator; }
_w142() { ns_esc_8_bit; }
_w143() { ns_esc_16_bit; }
_w144() { ns_esc_32_bit; }
_w145() { ns_esc_null; }
_w146() { ns_esc_bell; }
_w147() { ns_esc_backspace; }
_w148() { ns_esc_horizontal_tab; }
_w149() { ns_esc_line_feed; }
_w150() { ns_esc_vertical_tab; }
_w151() { ns_esc_form_feed; }
_w152() { ns_esc_carriage_return; }
_w153() { ns_esc_escape; }
_w154() { ns_esc_space; }
_w155() { ns_esc_double_quote; }
_w156() { ns_esc_slash; }
_w157() { ns_esc_backslash; }
_w158() { ns_esc_next_line; }
_w159() { ns_esc_non_breaking_space; }
_w160() { ns_esc_line_separator; }
_w161() { ns_esc_paragraph_separator; }
_w162() { ns_esc_8_bit; }
_w163() { ns_esc_16_bit; }
_w164() { ns_esc_32_bit; }
_w165() { peg_alt 
        _w145
        _w146
        _w147
        _w148
        _w149
        _w150
        _w151
        _w152
        _w153
        _w154
        _w155
        _w156
        _w157
        _w158
        _w159
        _w160
        _w161
        _w162
        _w163
        _w164; }
_w166() { s_space; }
_w167() { s_space; }
_w168() { s_space; }
_w169() { s_white; }
_w170() { plus_p _w169; }
_w171() { ok_r; }
_w172() { s_indent $n; }
_w173() { s_separate_in_line; }
_w174() { opt_p _w173; }
_w175() { s_line_prefix $n $c; }
_w176() { s_indent_lt $n; }
_w177() { peg_alt _w175 _w176; }
_w178() { b_as_line_feed; }
_w179() { b_non_content; }
_w180() { l_empty $n $c; }
_w181() { plus_p _w180; }
_w182() { b_l_trimmed $n $c; }
_w183() { b_as_space; }
_w184() { s_separate_in_line; }
_w185() { opt_p _w184; }
_w186() { b_l_folded $n "FLOW-IN"; }
_w187() { s_flow_line_prefix $n; }
_w188() { c_comment; }
_w189() { nb_char; }
_w190() { star_p _w189; }
_w191() { b_non_content; }
_w192() { ok_r; }
_w193() { s_separate_in_line; }
_w194() { c_nb_comment_text; }
_w195() { opt_p _w194; }
_w196() { peg_seq _w193 _w195; }
_w197() { opt_p _w196; }
_w198() { b_comment; }
_w199() { s_separate_in_line; }
_w200() { c_nb_comment_text; }
_w201() { opt_p _w200; }
_w202() { b_non_content; }
_w203() { s_b_comment; }
_w204() { ok_r; }
_w205() { peg_alt _w203 _w204; }
_w206() { l_comment; }
_w207() { star_p _w206; }
_w208() { s_l_comments; }
_w209() { s_flow_line_prefix $n; }
_w210() { peg_seq _w208 _w209; }
_w211() { s_separate_in_line; }
_w212() { c_directive; }
_w213() { ns_yaml_directive; }
_w214() { ns_tag_directive; }
_w215() { ns_reserved_directive; }
_w216() { peg_alt _w213 _w214 _w215; }
_w217() { s_l_comments; }
_w218() { ns_directive_name; }
_w219() { s_separate_in_line; }
_w220() { ns_directive_parameter; }
_w221() { peg_seq _w219 _w220; }
_w222() { star_p _w221; }
_w223() { ns_char; }
_w224() { ns_char; }
_w225() { match_str "YAML"; }
_w226() { s_separate_in_line; }
_w227() { ns_yaml_version; }
_w228() { ns_dec_digit; }
_w229() { plus_p _w228; }
_w230() { match_cp 46; }
_w231() { ns_dec_digit; }
_w232() { plus_p _w231; }
_w233() { match_str "TAG"; }
_w234() { s_separate_in_line; }
_w235() { c_tag_handle; }
_w236() { s_separate_in_line; }
_w237() { ns_tag_prefix; }
_w238() { c_named_tag_handle; }
_w239() { c_secondary_tag_handle; }
_w240() { c_primary_tag_handle; }
_w241() { match_cp 33; }
_w242() { ns_word_char; }
_w243() { plus_p _w242; }
_w244() { match_cp 33; }
_w245() { c_ns_local_tag_prefix; }
_w246() { ns_global_tag_prefix; }
_w247() { match_cp 33; }
_w248() { ns_uri_char; }
_w249() { star_p _w248; }
_w250() { ns_tag_char; }
_w251() { ns_uri_char; }
_w252() { star_p _w251; }
_w253() { c_ns_tag_property; }
_w254() { s_separate $n $c; }
_w255() { c_ns_anchor_property; }
_w256() { peg_seq _w254 _w255; }
_w257() { opt_p _w256; }
_w258() { peg_seq _w253 _w257; }
_w259() { c_ns_anchor_property; }
_w260() { s_separate $n $c; }
_w261() { c_ns_tag_property; }
_w262() { peg_seq _w260 _w261; }
_w263() { opt_p _w262; }
_w264() { peg_seq _w259 _w263; }
_w265() { c_verbatim_tag; }
_w266() { c_ns_shorthand_tag; }
_w267() { c_non_specific_tag; }
_w268() { match_str "!<"; }
_w269() { ns_uri_char; }
_w270() { plus_p _w269; }
_w271() { match_cp 62; }
_w272() { c_tag_handle; }
_w273() { ns_tag_char; }
_w274() { plus_p _w273; }
_w275() { c_anchor; }
_w276() { ns_anchor_name; }
_w277() { scalar_p _w276; }
_w278() { peg_seq _w275 _w277; }
_w279() { ns_char; }
_w280() { c_flow_indicator; }
_w281() { ns_anchor_char; }
_w282() { c_alias; }
_w283() { ns_anchor_name; }
_w284() { scalar_p _w283; }
_w285() { peg_seq _w282 _w284; }
_w286() { c_ns_esc_char; }
_w287() { nb_json; }
_w288() { match_cp 92; }
_w289() { match_cp 34; }
_w290() { peg_alt _w288 _w289; }
_w291() { minus_p _w287 _w290; }
_w292() { nb_double_char; }
_w293() { s_white; }
_w294() { match_cp 34; }
_w295() { nb_double_text $n $c; }
_w296() { match_cp 34; }
_w297() { peg_seq _w294 _w295 _w296; }
_w298() { nb_double_char; }
_w299() { s_white; }
_w300() { star_p _w299; }
_w301() { match_cp 92; }
_w302() { b_non_content; }
_w303() { l_empty $n "FLOW-IN"; }
_w304() { star_p _w303; }
_w305() { s_flow_line_prefix $n; }
_w306() { s_double_escaped $n; }
_w307() { s_flow_folded $n; }
_w308() { s_white; }
_w309() { star_p _w308; }
_w310() { ns_double_char; }
_w311() { peg_seq _w309 _w310; }
_w312() { s_double_break $n; }
_w313() { ns_double_char; }
_w314() { nb_ns_double_in_line; }
_w315() { s_double_next_line $n; }
_w316() { s_white; }
_w317() { star_p _w316; }
_w318() { peg_alt _w315 _w317; }
_w319() { peg_seq _w313 _w314 _w318; }
_w320() { opt_p _w319; }
_w321() { nb_ns_double_in_line; }
_w322() { s_double_next_line $n; }
_w323() { s_white; }
_w324() { star_p _w323; }
_w325() { peg_alt _w322 _w324; }
_w326() { c_quoted_quote; }
_w327() { nb_json; }
_w328() { match_cp 39; }
_w329() { minus_p _w327 _w328; }
_w330() { nb_single_char; }
_w331() { s_white; }
_w332() { match_cp 39; }
_w333() { nb_single_text $n $c; }
_w334() { match_cp 39; }
_w335() { peg_seq _w332 _w333 _w334; }
_w336() { nb_single_char; }
_w337() { s_white; }
_w338() { star_p _w337; }
_w339() { ns_single_char; }
_w340() { peg_seq _w338 _w339; }
_w341() { s_flow_folded $n; }
_w342() { ns_single_char; }
_w343() { ns_single_in_line; }
_w344() { s_single_next_line $n; }
_w345() { s_white; }
_w346() { star_p _w345; }
_w347() { peg_alt _w344 _w346; }
_w348() { peg_seq _w342 _w343 _w347; }
_w349() { opt_p _w348; }
_w350() { ns_single_in_line; }
_w351() { s_single_next_line $n; }
_w352() { s_white; }
_w353() { star_p _w352; }
_w354() { peg_alt _w351 _w353; }
_w355() { ns_char; }
_w356() { c_indicator; }
_w357() { minus_p _w355 _w356; }
_w358() { match_cp 63; }
_w359() { match_cp 58; }
_w360() { match_cp 45; }
_w361() { peg_alt _w358 _w359 _w360; }
_w362() { ns_plain_safe $c; }
_w363() { ahead_p _w362; }
_w364() { peg_seq _w361 _w363; }
_w365() { ns_char; }
_w366() { c_flow_indicator; }
_w367() { ns_plain_safe $c; }
_w368() { match_cp 58; }
_w369() { match_cp 35; }
_w370() { peg_alt _w368 _w369; }
_w371() { minus_p _w367 _w370; }
_w372() { ns_char; }
_w373() { behind_p _w372; }
_w374() { match_cp 35; }
_w375() { peg_seq _w373 _w374; }
_w376() { match_cp 58; }
_w377() { ns_plain_safe $c; }
_w378() { ahead_p _w377; }
_w379() { peg_seq _w376 _w378; }
_w380() { case "$c" in
        FLOW-OUT) ns_plain_multi_line $n $c ;;
        FLOW-IN) ns_plain_multi_line $n $c ;;
        BLOCK-KEY) ns_plain_one_line $c ;;
        FLOW-KEY) ns_plain_one_line $c ;;
        *) fail_r "no case" ;;
    esac; }
_w381() { s_white; }
_w382() { star_p _w381; }
_w383() { ns_plain_char $c; }
_w384() { peg_seq _w382 _w383; }
_w385() { ns_plain_first $c; }
_w386() { nb_ns_plain_in_line $c; }
_w387() { s_flow_folded $n; }
_w388() { c_forbidden; }
_w389() { neg_p _w388; }
_w390() { ns_plain_char $c; }
_w391() { nb_ns_plain_in_line $c; }
_w392() { ns_plain_one_line $c; }
_w393() { s_ns_plain_next_line $n $c; }
_w394() { star_p _w393; }
_w395() { match_cp 91; }
_w396() { s_separate $n $c; }
_w397() { opt_p _w396; }
_w398() { ns_s_flow_seq_entries $n $(in_flow $c); }
_w399() { collect_p _w398; }
_w400() { opt_p _w399; }
_w401() { match_cp 93; }
_w402() { peg_seq _w395 _w397 _w400 _w401; }
_w403() { ns_flow_seq_entry $n $c; }
_w404() { s_separate $n $c; }
_w405() { opt_p _w404; }
_w406() { match_cp 44; }
_w407() { s_separate $n $c; }
_w408() { opt_p _w407; }
_w409() { ns_s_flow_seq_entries $n $c; }
_w410() { opt_p _w409; }
_w411() { peg_seq _w406 _w408 _w410; }
_w412() { opt_p _w411; }
_w413() { ns_flow_pair $n $c; }
_w414() { ns_flow_node $n $c; }
_w415() { match_cp 123; }
_w416() { s_separate $n $c; }
_w417() { opt_p _w416; }
_w418() { ns_s_flow_map_entries $n $(in_flow $c); }
_w419() { collect_p _w418; }
_w420() { opt_p _w419; }
_w421() { match_cp 125; }
_w422() { peg_seq _w415 _w417 _w420 _w421; }
_w423() { ns_flow_map_entry $n $c; }
_w424() { s_separate $n $c; }
_w425() { opt_p _w424; }
_w426() { match_cp 44; }
_w427() { s_separate $n $c; }
_w428() { opt_p _w427; }
_w429() { ns_s_flow_map_entries $n $c; }
_w430() { opt_p _w429; }
_w431() { peg_seq _w426 _w428 _w430; }
_w432() { opt_p _w431; }
_w433() { match_cp 63; }
_w434() { s_separate $n $c; }
_w435() { ns_flow_map_explicit_entry $n $c; }
_w436() { peg_seq _w433 _w434 _w435; }
_w437() { ns_flow_map_implicit_entry $n $c; }
_w438() { ns_flow_map_implicit_entry $n $c; }
_w439() { e_node; }
_w440() { e_node; }
_w441() { peg_seq _w439 _w440; }
_w442() { ns_flow_map_yaml_key_entry $n $c; }
_w443() { c_ns_flow_map_empty_key_entry $n $c; }
_w444() { c_ns_flow_map_json_key_entry $n $c; }
_w445() { peg_alt _w442 _w443 _w444; }
_w446() { ns_flow_yaml_node $n $c; }
_w447() { s_separate $n $c; }
_w448() { opt_p _w447; }
_w449() { c_ns_flow_map_separate_value $n $c; }
_w450() { peg_seq _w448 _w449; }
_w451() { e_node; }
_w452() { peg_alt _w450 _w451; }
_w453() { e_node; }
_w454() { c_ns_flow_map_separate_value $n $c; }
_w455() { match_cp 58; }
_w456() { ns_plain_safe $c; }
_w457() { neg_p _w456; }
_w458() { s_separate $n $c; }
_w459() { ns_flow_node $n $c; }
_w460() { peg_seq _w458 _w459; }
_w461() { e_node; }
_w462() { peg_alt _w460 _w461; }
_w463() { c_flow_json_node $n $c; }
_w464() { s_separate $n $c; }
_w465() { opt_p _w464; }
_w466() { c_ns_flow_map_adjacent_value $n $c; }
_w467() { peg_seq _w465 _w466; }
_w468() { e_node; }
_w469() { peg_alt _w467 _w468; }
_w470() { match_cp 58; }
_w471() { s_separate $n $c; }
_w472() { opt_p _w471; }
_w473() { ns_flow_node $n $c; }
_w474() { peg_seq _w472 _w473; }
_w475() { e_node; }
_w476() { peg_alt _w474 _w475; }
_w477() { match_cp 63; }
_w478() { s_separate $n $c; }
_w479() { ns_flow_map_explicit_entry $n $c; }
_w480() { peg_seq _w477 _w478 _w479; }
_w481() { ns_flow_pair_entry $n $c; }
_w482() { ns_flow_pair_yaml_key_entry $n $c; }
_w483() { c_ns_flow_map_empty_key_entry $n $c; }
_w484() { c_ns_flow_pair_json_key_entry $n $c; }
_w485() { ns_s_implicit_yaml_key "FLOW-KEY"; }
_w486() { c_ns_flow_map_separate_value $n $c; }
_w487() { c_s_implicit_json_key "FLOW-KEY"; }
_w488() { c_ns_flow_map_adjacent_value $n $c; }
_w489() { ns_flow_yaml_node 0 $c; }
_w490() { s_separate_in_line; }
_w491() { opt_p _w490; }
_w492() { c_flow_json_node 0 $c; }
_w493() { s_separate_in_line; }
_w494() { opt_p _w493; }
_w495() { c_flow_sequence $n $c; }
_w496() { c_flow_mapping $n $c; }
_w497() { c_single_quoted $n $c; }
_w498() { c_double_quoted $n $c; }
_w499() { ns_flow_yaml_content $n $c; }
_w500() { c_flow_json_content $n $c; }
_w501() { c_ns_alias_node; }
_w502() { ns_flow_yaml_content $n $c; }
_w503() { c_ns_properties $n $c; }
_w504() { s_separate $n $c; }
_w505() { ns_flow_yaml_content $n $c; }
_w506() { peg_seq _w504 _w505; }
_w507() { e_scalar; }
_w508() { peg_alt _w506 _w507; }
_w509() { peg_seq _w503 _w508; }
_w510() { c_ns_properties $n $c; }
_w511() { s_separate $n $c; }
_w512() { peg_seq _w510 _w511; }
_w513() { opt_p _w512; }
_w514() { c_flow_json_content $n $c; }
_w515() { c_ns_alias_node; }
_w516() { ns_flow_content $n $c; }
_w517() { c_ns_properties $n $c; }
_w518() { s_separate $n $c; }
_w519() { ns_flow_content $n $c; }
_w520() { peg_seq _w518 _w519; }
_w521() { e_scalar; }
_w522() { peg_alt _w520 _w521; }
_w523() { peg_seq _w517 _w522; }
_w524() { ns_dec_digit; }
_w525() { parse_int_p _w524; }
_w526() { detect_indent $n; }
_w527() { match_cp 45; }
_w528() { parse_sym_p _w527 "STRIP"; }
_w529() { match_cp 43; }
_w530() { parse_sym_p _w529 "KEEP"; }
_w531() { val_p "CLIP"; }
_w532() { peg_alt _w525 _w526; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_alt _w528 _w530 _w531; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; s_b_comment; }
_w533() { match_cp 45; }
_w534() { parse_sym_p _w533 "STRIP"; }
_w535() { match_cp 43; }
_w536() { parse_sym_p _w535 "KEEP"; }
_w537() { val_p "CLIP"; }
_w538() { ns_dec_digit; }
_w539() { parse_int_p _w538; }
_w540() { detect_indent $n; }
_w541() { peg_alt _w534 _w536 _w537; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; peg_alt _w539 _w540; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; s_b_comment; }
_w542() { ns_dec_digit; }
_w543() { ok_r; }
_w544() { match_cp 45; }
_w545() { match_cp 43; }
_w546() { ok_r; }
_w547() { s_indent_le $n; }
_w548() { b_non_content; }
_w549() { peg_seq _w547 _w548; }
_w550() { star_p _w549; }
_w551() { l_trail_comments $n; }
_w552() { opt_p _w551; }
_w553() { l_empty $n "BLOCK-IN"; }
_w554() { star_p _w553; }
_w555() { l_trail_comments $n; }
_w556() { opt_p _w555; }
_w557() { s_indent_lt $n; }
_w558() { c_nb_comment_text; }
_w559() { b_comment; }
_w560() { l_comment; }
_w561() { star_p _w560; }
_w562() { match_cp 124; }
_w563() { ns_dec_digit; }
_w564() { parse_int_p _w563; }
_w565() { detect_indent $n; }
_w566() { match_cp 45; }
_w567() { parse_sym_p _w566 "STRIP"; }
_w568() { match_cp 43; }
_w569() { parse_sym_p _w568 "KEEP"; }
_w570() { val_p "CLIP"; }
_w571() { s_b_comment; }
_w572() { l_literal_content $(( $n + $m )) $t; }
_w573() { peg_alt _w564 _w565; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_alt _w567 _w569 _w570; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; peg_seq _w571 _w572; }
_w574() { l_empty $n "BLOCK-IN"; }
_w575() { star_p _w574; }
_w576() { s_indent $n; }
_w577() { nb_char; }
_w578() { plus_p _w577; }
_w579() { b_as_line_feed; }
_w580() { l_nb_literal_text $n; }
_w581() { l_nb_literal_text $n; }
_w582() { b_nb_literal_next $n; }
_w583() { star_p _w582; }
_w584() { b_chomped_last $t; }
_w585() { peg_seq _w581 _w583 _w584; }
_w586() { opt_p _w585; }
_w587() { l_chomped_empty $n $t; }
_w588() { peg_seq _w586 _w587; }
_w589() { match_cp 62; }
_w590() { ns_dec_digit; }
_w591() { parse_int_p _w590; }
_w592() { detect_indent $n; }
_w593() { match_cp 45; }
_w594() { parse_sym_p _w593 "STRIP"; }
_w595() { match_cp 43; }
_w596() { parse_sym_p _w595 "KEEP"; }
_w597() { val_p "CLIP"; }
_w598() { s_b_comment; }
_w599() { l_folded_content $(( $n + $m )) $t; }
_w600() { peg_alt _w591 _w592; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_alt _w594 _w596 _w597; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; peg_seq _w598 _w599; }
_w601() { s_indent $n; }
_w602() { ns_char; }
_w603() { nb_char; }
_w604() { star_p _w603; }
_w605() { s_nb_folded_text $n; }
_w606() { b_l_folded $n "BLOCK-IN"; }
_w607() { s_nb_folded_text $n; }
_w608() { peg_seq _w606 _w607; }
_w609() { star_p _w608; }
_w610() { s_indent $n; }
_w611() { s_white; }
_w612() { nb_char; }
_w613() { star_p _w612; }
_w614() { b_as_line_feed; }
_w615() { l_empty $n "BLOCK-IN"; }
_w616() { star_p _w615; }
_w617() { s_nb_spaced_text $n; }
_w618() { b_l_spaced $n; }
_w619() { s_nb_spaced_text $n; }
_w620() { peg_seq _w618 _w619; }
_w621() { star_p _w620; }
_w622() { l_empty $n "BLOCK-IN"; }
_w623() { star_p _w622; }
_w624() { l_nb_folded_lines $n; }
_w625() { l_nb_spaced_lines $n; }
_w626() { peg_alt _w624 _w625; }
_w627() { l_nb_same_lines $n; }
_w628() { b_as_line_feed; }
_w629() { l_nb_same_lines $n; }
_w630() { peg_seq _w628 _w629; }
_w631() { star_p _w630; }
_w632() { l_nb_diff_lines $n; }
_w633() { b_chomped_last $t; }
_w634() { peg_seq _w632 _w633; }
_w635() { opt_p _w634; }
_w636() { l_chomped_empty $n $t; }
_w637() { peg_seq _w635 _w636; }
_w638() { s_indent $(( $n + $m )); }
_w639() { c_l_block_seq_entry $(( $n + $m )); }
_w640() { peg_seq _w638 _w639; }
_w641() { plus_p _w640; }
_w642() { detect_indent $n; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; collect_p _w641; }
_w643() { match_cp 45; }
_w644() { ns_char; }
_w645() { neg_p _w644; }
_w646() { s_lblock_indented $n "BLOCK-IN"; }
_w647() { s_indent $m; }
_w648() { ns_l_compact_sequence $(( $n + 1 + $m )); }
_w649() { ns_l_compact_mapping $(( $n + 1 + $m )); }
_w650() { peg_alt _w648 _w649; }
_w651() { detect_indent 0; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_seq _w647 _w650; }
_w652() { s_lblock_node $n $c; }
_w653() { e_node; }
_w654() { s_l_comments; }
_w655() { peg_seq _w653 _w654; }
_w656() { c_l_block_seq_entry $n; }
_w657() { s_indent $n; }
_w658() { c_l_block_seq_entry $n; }
_w659() { peg_seq _w657 _w658; }
_w660() { star_p _w659; }
_w661() { s_indent $(( $n + $m )); }
_w662() { ns_l_block_map_entry $(( $n + $m )); }
_w663() { peg_seq _w661 _w662; }
_w664() { plus_p _w663; }
_w665() { detect_indent $n; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; collect_p _w664; }
_w666() { c_l_block_map_explicit_entry $n; }
_w667() { ns_l_block_map_implicit_entry $n; }
_w668() { c_l_block_map_explicit_key $n; }
_w669() { l_block_map_explicit_value $n; }
_w670() { e_node; }
_w671() { peg_alt _w669 _w670; }
_w672() { match_cp 63; }
_w673() { s_lblock_indented $n "BLOCK-OUT"; }
_w674() { s_indent $n; }
_w675() { match_cp 58; }
_w676() { s_lblock_indented $n "BLOCK-OUT"; }
_w677() { ns_s_block_map_implicit_key; }
_w678() { e_node; }
_w679() { peg_alt _w677 _w678; }
_w680() { scalar_p _w679; }
_w681() { c_l_block_map_implicit_value $n; }
_w682() { peg_seq _w680 _w681; }
_w683() { c_s_implicit_json_key "BLOCK-KEY"; }
_w684() { ns_s_implicit_yaml_key "BLOCK-KEY"; }
_w685() { match_cp 58; }
_w686() { s_lblock_node $n "BLOCK-OUT"; }
_w687() { e_node; }
_w688() { s_l_comments; }
_w689() { peg_seq _w687 _w688; }
_w690() { scalar_p _w689; }
_w691() { peg_alt _w686 _w690; }
_w692() { ns_l_block_map_entry $n; }
_w693() { s_indent $n; }
_w694() { ns_l_block_map_entry $n; }
_w695() { peg_seq _w693 _w694; }
_w696() { star_p _w695; }
_w697() { s_lblock_in_block $n $c; }
_w698() { s_lflow_in_block $n; }
_w699() { s_separate $(( $n + 1 )) "FLOW-OUT"; }
_w700() { ns_flow_node $(( $n + 1 )) "FLOW-OUT"; }
_w701() { s_l_comments; }
_w702() { s_lblock_scalar $n $c; }
_w703() { s_lblock_collection $n $c; }
_w704() { s_separate $(( $n + 1 )) $c; }
_w705() { c_ns_properties $(( $n + 1 )) $c; }
_w706() { s_separate $(( $n + 1 )) $c; }
_w707() { peg_seq _w705 _w706; }
_w708() { opt_p _w707; }
_w709() { c_lliteral $n; }
_w710() { c_lfolded $n; }
_w711() { peg_alt _w709 _w710; }
_w712() { s_separate $(( $n + 1 )) $c; }
_w713() { c_ns_properties $(( $n + 1 )) $c; }
_w714() { peg_seq _w712 _w713; }
_w715() { opt_p _w714; }
_w716() { s_l_comments; }
_w717() { lblock_sequence $(seq_spaces $n $c); }
_w718() { lblock_mapping $n; }
_w719() { peg_alt _w717 _w718; }
_w720() { c_byte_order_mark; }
_w721() { opt_p _w720; }
_w722() { l_comment; }
_w723() { star_p _w722; }
_w724() { c_document_end; }
_w725() { s_l_comments; }
_w726() { sol_p; }
_w727() { c_directives_end; }
_w728() { c_document_end; }
_w729() { peg_alt _w727 _w728; }
_w730() { b_char; }
_w731() { s_white; }
_w732() { eof_p; }
_w733() { peg_alt _w730 _w731 _w732; }
_w734() { s_lblock_node -1 "BLOCK-IN"; }
_w735() { c_directives_end; }
_w736() { l_bare_document; }
_w737() { e_node; }
_w738() { s_l_comments; }
_w739() { peg_seq _w737 _w738; }
_w740() { peg_alt _w736 _w739; }
_w741() { peg_seq _w735 _w740; }
_w742() { l_directive; }
_w743() { plus_p _w742; }
_w744() { l_explicit_document; }
_w745() { l_directive_document; }
_w746() { l_explicit_document; }
_w747() { l_bare_document; }
_w748() { l_document_prefix; }
_w749() { star_p _w748; }
_w750() { l_any_document; }
_w751() { opt_p _w750; }
_w752() { l_document_suffix; }
_w753() { plus_p _w752; }
_w754() { l_document_prefix; }
_w755() { star_p _w754; }
_w756() { l_any_document; }
_w757() { opt_p _w756; }
_w758() { peg_seq _w753 _w755 _w757; }
_w759() { l_document_prefix; }
_w760() { star_p _w759; }
_w761() { l_explicit_document; }
_w762() { opt_p _w761; }
_w763() { peg_seq _w760 _w762; }
_w764() { peg_alt _w758 _w763; }
_w765() { star_p _w764; }
_w766() { peg_seq _w749 _w751 _w765; }

# ════════════════════════════════════════════════════════════════ 
# YAML 1.2 Grammar — 211 rules 
# ════════════════════════════════════════════════════════════════ 

# [1] C-PRINTABLE 
c_printable() {
    peg_alt _w1 _w2 _w3 _w4 _w5 _w6 _w7 _w8
}

# [2] NB-JSON 
nb_json() {
    peg_alt _w9 _w10
}

# [3] C-BYTE-ORDER-MARK 
c_byte_order_mark() {
    match_cp 0xFEFF
}

# [4] C-SEQUENCE-ENTRY 
c_sequence_entry() {
    match_cp 45
}

# [5] C-MAPPING-KEY 
c_mapping_key() {
    match_cp 63
}

# [6] C-MAPPING-VALUE 
c_mapping_value() {
    match_cp 58
}

# [7] C-COLLECT-ENTRY 
c_collect_entry() {
    match_cp 44
}

# [8] C-SEQUENCE-START 
c_sequence_start() {
    match_cp 91
}

# [9] C-SEQUENCE-END 
c_sequence_end() {
    match_cp 93
}

# [10] C-MAPPING-START 
c_mapping_start() {
    match_cp 123
}

# [11] C-MAPPING-END 
c_mapping_end() {
    match_cp 125
}

# [12] C-COMMENT 
c_comment() {
    match_cp 35
}

# [13] C-ANCHOR 
c_anchor() {
    match_cp 38
}

# [14] C-ALIAS 
c_alias() {
    match_cp 42
}

# [15] C-TAG 
c_tag() {
    match_cp 33
}

# [16] C-LITERAL 
c_literal() {
    match_cp 124
}

# [17] C-FOLDED 
c_folded() {
    match_cp 62
}

# [18] C-SINGLE-QUOTE 
c_single_quote() {
    match_cp 39
}

# [19] C-DOUBLE-QUOTE 
c_double_quote() {
    match_cp 34
}

# [20] C-DIRECTIVE 
c_directive() {
    match_cp 37
}

# [21] C-RESERVED 
c_reserved() {
    peg_alt _w11 _w12
}

# [22] C-INDICATOR 
c_indicator() {
    peg_alt _w13 _w14 _w15 _w16 _w17 _w18 _w19 _w20 _w21 _w22 _w23 _w24 _w25 _w26 _w27 _w28 _w29 _w30
}

# [23] C-FLOW-INDICATOR 
c_flow_indicator() {
    peg_alt _w31 _w32 _w33 _w34 _w35
}

# [24] B-LINE-FEED 
b_line_feed() {
    match_cp 0x0A
}

# [25] B-CARRIAGE-RETURN 
b_carriage_return() {
    match_cp 0x0D
}

# [26] B-CHAR 
b_char() {
    peg_alt _w36 _w37
}

# [27] NB-CHAR 
nb_char() {
    minus_p _w38 _w41
}

# [28] B-BREAK 
b_break() {
    peg_alt _w44 _w45 _w46
}

# [29] B-AS-LINE-FEED 
b_as_line_feed() {
    b_break
}

# [30] B-NON-CONTENT 
b_non_content() {
    b_break
}

# [31] S-SPACE 
s_space() {
    match_cp 0x20
}

# [32] S-TAB 
s_tab() {
    match_cp 0x9
}

# [33] S-WHITE 
s_white() {
    peg_alt _w47 _w48
}

# [34] NS-CHAR 
ns_char() {
    minus_p _w49 _w50
}

# [35] NS-DEC-DIGIT 
ns_dec_digit() {
    match_range 0x30 0x39
}

# [36] NS-HEX-DIGIT 
ns_hex_digit() {
    peg_alt _w51 _w52 _w53
}

# [37] NS-ASCII-LETTER 
ns_ascii_letter() {
    peg_alt _w54 _w55
}

# [38] NS-WORD-CHAR 
ns_word_char() {
    peg_alt _w56 _w57 _w58
}

# [39] NS-URI-CHAR 
ns_uri_char() {
    peg_alt 
        _w88
        _w89
        _w90
        _w91
        _w92
        _w93
        _w94
        _w95
        _w96
        _w97
        _w98
        _w99
        _w100
        _w101
        _w102
        _w103
        _w104
        _w105
        _w106
        _w107
        _w108
        _w109
        _w110
}

# [40] NS-TAG-CHAR 
ns_tag_char() {
    minus_p _w111 _w114
}

# [41] C-ESCAPE 
c_escape() {
    match_cp 92
}

# [42] NS-ESC-NULL 
ns_esc_null() {
    match_cp 48
}

# [43] NS-ESC-BELL 
ns_esc_bell() {
    match_cp 97
}

# [44] NS-ESC-BACKSPACE 
ns_esc_backspace() {
    match_cp 98
}

# [45] NS-ESC-HORIZONTAL-TAB 
ns_esc_horizontal_tab() {
    match_cp 116
}

# [46] NS-ESC-LINE-FEED 
ns_esc_line_feed() {
    match_cp 110
}

# [47] NS-ESC-VERTICAL-TAB 
ns_esc_vertical_tab() {
    match_cp 118
}

# [48] NS-ESC-FORM-FEED 
ns_esc_form_feed() {
    match_cp 102
}

# [49] NS-ESC-CARRIAGE-RETURN 
ns_esc_carriage_return() {
    match_cp 114
}

# [50] NS-ESC-ESCAPE 
ns_esc_escape() {
    match_cp 101
}

# [51] NS-ESC-SPACE 
ns_esc_space() {
    match_cp 0x20
}

# [52] NS-ESC-DOUBLE-QUOTE 
ns_esc_double_quote() {
    match_cp 34
}

# [53] NS-ESC-SLASH 
ns_esc_slash() {
    match_cp 47
}

# [54] NS-ESC-BACKSLASH 
ns_esc_backslash() {
    match_cp 92
}

# [55] NS-ESC-NEXT-LINE 
ns_esc_next_line() {
    match_cp 78
}

# [56] NS-ESC-NON-BREAKING-SPACE 
ns_esc_non_breaking_space() {
    match_cp 95
}

# [57] NS-ESC-LINE-SEPARATOR 
ns_esc_line_separator() {
    match_cp 76
}

# [58] NS-ESC-PARAGRAPH-SEPARATOR 
ns_esc_paragraph_separator() {
    match_cp 80
}

# [59] NS-ESC-8-BIT 
ns_esc_8_bit() {
    peg_seq _w115 _w117
}

# [60] NS-ESC-16-BIT 
ns_esc_16_bit() {
    peg_seq _w118 _w120
}

# [61] NS-ESC-32-BIT 
ns_esc_32_bit() {
    peg_seq _w121 _w123
}

# [62] C-NS-ESC-CHAR 
c_ns_esc_char() {
    peg_seq _w124 _w165
}

# [63] S-INDENT 
s_indent() { local n="$1"
    rep_p $n _w166
}

# [64] S-INDENT-LT 
s_indent_lt() { local n="$1"
    star_p _w167
}

# [65] S-INDENT-LE 
s_indent_le() { local n="$1"
    star_p _w168
}

# [66] S-SEPARATE-IN-LINE 
s_separate_in_line() {
    peg_alt _w170 _w171
}

# [67] S-LINE-PREFIX 
s_line_prefix() { local n="$1"; c="$2"
    case "$c" in
        BLOCK-IN) s_block_line_prefix $n ;;
        BLOCK-OUT) s_block_line_prefix $n ;;
        FLOW-IN) s_flow_line_prefix $n ;;
        FLOW-OUT) s_flow_line_prefix $n ;;
        *) fail_r "no case" ;;
    esac
}

# [68] S-BLOCK-LINE-PREFIX 
s_block_line_prefix() { local n="$1"
    s_indent $n
}

# [69] S-FLOW-LINE-PREFIX 
s_flow_line_prefix() { local n="$1"
    peg_seq _w172 _w174
}

# [70] L-EMPTY 
l_empty() { local n="$1"; c="$2"
    peg_seq _w177 _w178
}

# [71] B-L-TRIMMED 
b_l_trimmed() { local n="$1"; c="$2"
    peg_seq _w179 _w181
}

# [72] B-AS-SPACE 
b_as_space() {
    b_break
}

# [73] B-L-FOLDED 
b_l_folded() { local n="$1"; c="$2"
    peg_alt _w182 _w183
}

# [74] S-FLOW-FOLDED 
s_flow_folded() { local n="$1"
    peg_seq _w185 _w186 _w187
}

# [75] C-NB-COMMENT-TEXT 
c_nb_comment_text() {
    peg_seq _w188 _w190
}

# [76] B-COMMENT 
b_comment() {
    peg_alt _w191 _w192
}

# [77] S-B-COMMENT 
s_b_comment() {
    peg_seq _w197 _w198
}

# [78] L-COMMENT 
l_comment() {
    peg_seq _w199 _w201 _w202
}

# [79] S-L-COMMENTS 
s_l_comments() {
    peg_seq _w205 _w207
}

# [80] S-SEPARATE 
s_separate() { local n="$1"; c="$2"
    case "$c" in
        BLOCK-OUT) s_separate_lines $n ;;
        BLOCK-IN) s_separate_lines $n ;;
        FLOW-OUT) s_separate_lines $n ;;
        FLOW-IN) s_separate_lines $n ;;
        BLOCK-KEY) s_separate_in_line ;;
        FLOW-KEY) s_separate_in_line ;;
        *) fail_r "no case" ;;
    esac
}

# [81] S-SEPARATE-LINES 
s_separate_lines() { local n="$1"
    peg_alt _w210 _w211
}

# [82] L-DIRECTIVE 
l_directive() {
    peg_seq _w212 _w216 _w217
}

# [83] NS-RESERVED-DIRECTIVE 
ns_reserved_directive() {
    peg_seq _w218 _w222
}

# [84] NS-DIRECTIVE-NAME 
ns_directive_name() {
    plus_p _w223
}

# [85] NS-DIRECTIVE-PARAMETER 
ns_directive_parameter() {
    plus_p _w224
}

# [86] NS-YAML-DIRECTIVE 
ns_yaml_directive() {
    peg_seq _w225 _w226 _w227
}

# [87] NS-YAML-VERSION 
ns_yaml_version() {
    peg_seq _w229 _w230 _w232
}

# [88] NS-TAG-DIRECTIVE 
ns_tag_directive() {
    peg_seq _w233 _w234 _w235 _w236 _w237
}

# [89] C-TAG-HANDLE 
c_tag_handle() {
    peg_alt _w238 _w239 _w240
}

# [90] C-PRIMARY-TAG-HANDLE 
c_primary_tag_handle() {
    match_cp 33
}

# [91] C-SECONDARY-TAG-HANDLE 
c_secondary_tag_handle() {
    match_str "!!"
}

# [92] C-NAMED-TAG-HANDLE 
c_named_tag_handle() {
    peg_seq _w241 _w243 _w244
}

# [93] NS-TAG-PREFIX 
ns_tag_prefix() {
    peg_alt _w245 _w246
}

# [94] C-NS-LOCAL-TAG-PREFIX 
c_ns_local_tag_prefix() {
    peg_seq _w247 _w249
}

# [95] NS-GLOBAL-TAG-PREFIX 
ns_global_tag_prefix() {
    peg_seq _w250 _w252
}

# [96] C-NS-PROPERTIES 
c_ns_properties() { local n="$1"; c="$2"
    peg_alt _w258 _w264
}

# [97] C-NS-TAG-PROPERTY 
c_ns_tag_property() {
    peg_alt _w265 _w266 _w267
}

# [98] C-VERBATIM-TAG 
c_verbatim_tag() {
    peg_seq _w268 _w270 _w271
}

# [99] C-NS-SHORTHAND-TAG 
c_ns_shorthand_tag() {
    peg_seq _w272 _w274
}

# [100] C-NON-SPECIFIC-TAG 
c_non_specific_tag() {
    match_cp 33
}

# [101] C-NS-ANCHOR-PROPERTY 
c_ns_anchor_property() {
    build_p "ANCHOR" _w278
}

# [102] NS-ANCHOR-CHAR 
ns_anchor_char() {
    minus_p _w279 _w280
}

# [103] NS-ANCHOR-NAME 
ns_anchor_name() {
    plus_p _w281
}

# [104] C-NS-ALIAS-NODE 
c_ns_alias_node() {
    build_p "ALIAS" _w285
}

# [105] E-SCALAR 
e_scalar() {
    ok_r
}

# [106] E-NODE 
e_node() {
    e_scalar
}

# [107] NB-DOUBLE-CHAR 
nb_double_char() {
    peg_alt _w286 _w291
}

# [108] NS-DOUBLE-CHAR 
ns_double_char() {
    minus_p _w292 _w293
}

# [109] C-DOUBLE-QUOTED 
c_double_quoted() { local n="$1"; c="$2"
    scalar_p _w297
}

# [110] NB-DOUBLE-TEXT 
nb_double_text() { local n="$1"; c="$2"
    case "$c" in
        FLOW-OUT) nb_double_multi_line $n ;;
        FLOW-IN) nb_double_multi_line $n ;;
        BLOCK-KEY) nb_double_one_line ;;
        FLOW-KEY) nb_double_one_line ;;
        *) fail_r "no case" ;;
    esac
}

# [111] NB-DOUBLE-ONE-LINE 
nb_double_one_line() {
    star_p _w298
}

# [112] S-DOUBLE-ESCAPED 
s_double_escaped() { local n="$1"
    peg_seq _w300 _w301 _w302 _w304 _w305
}

# [113] S-DOUBLE-BREAK 
s_double_break() { local n="$1"
    peg_alt _w306 _w307
}

# [114] NB-NS-DOUBLE-IN-LINE 
nb_ns_double_in_line() {
    star_p _w311
}

# [115] S-DOUBLE-NEXT-LINE 
s_double_next_line() { local n="$1"
    peg_seq _w312 _w320
}

# [116] NB-DOUBLE-MULTI-LINE 
nb_double_multi_line() { local n="$1"
    peg_seq _w321 _w325
}

# [117] C-QUOTED-QUOTE 
c_quoted_quote() {
    match_str "''"
}

# [118] NB-SINGLE-CHAR 
nb_single_char() {
    peg_alt _w326 _w329
}

# [119] NS-SINGLE-CHAR 
ns_single_char() {
    minus_p _w330 _w331
}

# [120] C-SINGLE-QUOTED 
c_single_quoted() { local n="$1"; c="$2"
    scalar_p _w335
}

# [121] NB-SINGLE-TEXT 
nb_single_text() { local n="$1"; c="$2"
    case "$c" in
        FLOW-OUT) nb_single_multi_line $n ;;
        FLOW-IN) nb_single_multi_line $n ;;
        BLOCK-KEY) nb_single_one_line ;;
        FLOW-KEY) nb_single_one_line ;;
        *) fail_r "no case" ;;
    esac
}

# [122] NB-SINGLE-ONE-LINE 
nb_single_one_line() {
    star_p _w336
}

# [123] NS-SINGLE-IN-LINE 
ns_single_in_line() {
    star_p _w340
}

# [124] S-SINGLE-NEXT-LINE 
s_single_next_line() { local n="$1"
    peg_seq _w341 _w349
}

# [125] NB-SINGLE-MULTI-LINE 
nb_single_multi_line() { local n="$1"
    peg_seq _w350 _w354
}

# [126] NS-PLAIN-FIRST 
ns_plain_first() { local c="$1"
    peg_alt _w357 _w364
}

# [127] NS-PLAIN-SAFE 
ns_plain_safe() { local c="$1"
    case "$c" in
        FLOW-OUT) ns_plain_safe_out ;;
        FLOW-IN) ns_plain_safe_in ;;
        BLOCK-KEY) ns_plain_safe_out ;;
        FLOW-KEY) ns_plain_safe_in ;;
        *) fail_r "no case" ;;
    esac
}

# [128] NS-PLAIN-SAFE-OUT 
ns_plain_safe_out() {
    ns_char
}

# [129] NS-PLAIN-SAFE-IN 
ns_plain_safe_in() {
    minus_p _w365 _w366
}

# [130] NS-PLAIN-CHAR 
ns_plain_char() { local c="$1"
    peg_alt _w371 _w375 _w379
}

# [131] NS-PLAIN 
ns_plain() { local n="$1"; c="$2"
    scalar_p _w380
}

# [132] NB-NS-PLAIN-IN-LINE 
nb_ns_plain_in_line() { local c="$1"
    star_p _w384
}

# [133] NS-PLAIN-ONE-LINE 
ns_plain_one_line() { local c="$1"
    peg_seq _w385 _w386
}

# [134] S-NS-PLAIN-NEXT-LINE 
s_ns_plain_next_line() { local n="$1"; c="$2"
    peg_seq _w387 _w389 _w390 _w391
}

# [135] NS-PLAIN-MULTI-LINE 
ns_plain_multi_line() { local n="$1"; c="$2"
    peg_seq _w392 _w394
}

# [137] C-FLOW-SEQUENCE 
c_flow_sequence() { local n="$1"; c="$2"
    build_p "SEQUENCE" _w402
}

# [138] NS-S-FLOW-SEQ-ENTRIES 
ns_s_flow_seq_entries() { local n="$1"; c="$2"
    peg_seq _w403 _w405 _w412
}

# [139] NS-FLOW-SEQ-ENTRY 
ns_flow_seq_entry() { local n="$1"; c="$2"
    peg_alt _w413 _w414
}

# [140] C-FLOW-MAPPING 
c_flow_mapping() { local n="$1"; c="$2"
    build_p "MAPPING" _w422
}

# [141] NS-S-FLOW-MAP-ENTRIES 
ns_s_flow_map_entries() { local n="$1"; c="$2"
    peg_seq _w423 _w425 _w432
}

# [142] NS-FLOW-MAP-ENTRY 
ns_flow_map_entry() { local n="$1"; c="$2"
    peg_alt _w436 _w437
}

# [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
ns_flow_map_explicit_entry() { local n="$1"; c="$2"
    peg_alt _w438 _w441
}

# [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
ns_flow_map_implicit_entry() { local n="$1"; c="$2"
    build_p "PAIR" _w445
}

# [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
ns_flow_map_yaml_key_entry() { local n="$1"; c="$2"
    peg_seq _w446 _w452
}

# [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
c_ns_flow_map_empty_key_entry() { local n="$1"; c="$2"
    peg_seq _w453 _w454
}

# [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
c_ns_flow_map_separate_value() { local n="$1"; c="$2"
    peg_seq _w455 _w457 _w462
}

# [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
c_ns_flow_map_json_key_entry() { local n="$1"; c="$2"
    peg_seq _w463 _w469
}

# [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
c_ns_flow_map_adjacent_value() { local n="$1"; c="$2"
    peg_seq _w470 _w476
}

# [150] NS-FLOW-PAIR 
ns_flow_pair() { local n="$1"; c="$2"
    peg_alt _w480 _w481
}

# [151] NS-FLOW-PAIR-ENTRY 
ns_flow_pair_entry() { local n="$1"; c="$2"
    peg_alt _w482 _w483 _w484
}

# [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
ns_flow_pair_yaml_key_entry() { local n="$1"; c="$2"
    peg_seq _w485 _w486
}

# [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
c_ns_flow_pair_json_key_entry() { local n="$1"; c="$2"
    peg_seq _w487 _w488
}

# [154] NS-S-IMPLICIT-YAML-KEY 
ns_s_implicit_yaml_key() { local c="$1"
    peg_seq _w489 _w491
}

# [155] C-S-IMPLICIT-JSON-KEY 
c_s_implicit_json_key() { local c="$1"
    peg_seq _w492 _w494
}

# [156] NS-FLOW-YAML-CONTENT 
ns_flow_yaml_content() { local n="$1"; c="$2"
    ns_plain $n $c
}

# [157] C-FLOW-JSON-CONTENT 
c_flow_json_content() { local n="$1"; c="$2"
    peg_alt _w495 _w496 _w497 _w498
}

# [158] NS-FLOW-CONTENT 
ns_flow_content() { local n="$1"; c="$2"
    peg_alt _w499 _w500
}

# [159] NS-FLOW-YAML-NODE 
ns_flow_yaml_node() { local n="$1"; c="$2"
    peg_alt _w501 _w502 _w509
}

# [160] C-FLOW-JSON-NODE 
c_flow_json_node() { local n="$1"; c="$2"
    peg_seq _w513 _w514
}

# [161] NS-FLOW-NODE 
ns_flow_node() { local n="$1"; c="$2"
    peg_alt _w515 _w516 _w523
}

# [162] C-B-BLOCK-HEADER 
c_b_block_header() { local n="$1"
    peg_alt _w532 _w541
}

# [163] C-INDENTATION-INDICATOR 
c_indentation_indicator() { local n="$1"
    peg_alt _w542 _w543
}

# [164] C-CHOMPING-INDICATOR 
c_chomping_indicator() {
    peg_alt _w544 _w545 _w546
}

# [165] B-CHOMPED-LAST 
b_chomped_last() { local t="$1"
    case "$t" in
        STRIP) b_non_content ;;
        CLIP) b_as_line_feed ;;
        KEEP) b_as_line_feed ;;
        *) fail_r "no case" ;;
    esac
}

# [166] L-CHOMPED-EMPTY 
l_chomped_empty() { local n="$1"; t="$2"
    case "$t" in
        STRIP) l_strip_empty $n ;;
        CLIP) l_strip_empty $n ;;
        KEEP) l_keep_empty $n ;;
        *) fail_r "no case" ;;
    esac
}

# [167] L-STRIP-EMPTY 
l_strip_empty() { local n="$1"
    peg_seq _w550 _w552
}

# [168] L-KEEP-EMPTY 
l_keep_empty() { local n="$1"
    peg_seq _w554 _w556
}

# [169] L-TRAIL-COMMENTS 
l_trail_comments() { local n="$1"
    peg_seq _w557 _w558 _w559 _w561
}

# [170] C-L+LITERAL 
c_lliteral() { local n="$1"
    peg_seq _w562 _w573
}

# [171] L-NB-LITERAL-TEXT 
l_nb_literal_text() { local n="$1"
    peg_seq _w575 _w576 _w578
}

# [172] B-NB-LITERAL-NEXT 
b_nb_literal_next() { local n="$1"
    peg_seq _w579 _w580
}

# [173] L-LITERAL-CONTENT 
l_literal_content() { local n="$1"; t="$2"
    scalar_p _w588
}

# [174] C-L+FOLDED 
c_lfolded() { local n="$1"
    peg_seq _w589 _w600
}

# [175] S-NB-FOLDED-TEXT 
s_nb_folded_text() { local n="$1"
    peg_seq _w601 _w602 _w604
}

# [176] L-NB-FOLDED-LINES 
l_nb_folded_lines() { local n="$1"
    peg_seq _w605 _w609
}

# [177] S-NB-SPACED-TEXT 
s_nb_spaced_text() { local n="$1"
    peg_seq _w610 _w611 _w613
}

# [178] B-L-SPACED 
b_l_spaced() { local n="$1"
    peg_seq _w614 _w616
}

# [179] L-NB-SPACED-LINES 
l_nb_spaced_lines() { local n="$1"
    peg_seq _w617 _w621
}

# [180] L-NB-SAME-LINES 
l_nb_same_lines() { local n="$1"
    peg_seq _w623 _w626
}

# [181] L-NB-DIFF-LINES 
l_nb_diff_lines() { local n="$1"
    peg_seq _w627 _w631
}

# [182] L-FOLDED-CONTENT 
l_folded_content() { local n="$1"; t="$2"
    scalar_p _w637
}

# [183] L+BLOCK-SEQUENCE 
lblock_sequence() { local n="$1"
    build_p "SEQUENCE" _w642
}

# [184] C-L-BLOCK-SEQ-ENTRY 
c_l_block_seq_entry() { local n="$1"
    peg_seq _w643 _w645 _w646
}

# [185] S-L+BLOCK-INDENTED 
s_lblock_indented() { local n="$1"; c="$2"
    peg_alt _w651 _w652 _w655
}

# [186] NS-L-COMPACT-SEQUENCE 
ns_l_compact_sequence() { local n="$1"
    peg_seq _w656 _w660
}

# [187] L+BLOCK-MAPPING 
lblock_mapping() { local n="$1"
    build_p "MAPPING" _w665
}

# [188] NS-L-BLOCK-MAP-ENTRY 
ns_l_block_map_entry() { local n="$1"
    peg_alt _w666 _w667
}

# [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
c_l_block_map_explicit_entry() { local n="$1"
    peg_seq _w668 _w671
}

# [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
c_l_block_map_explicit_key() { local n="$1"
    peg_seq _w672 _w673
}

# [191] L-BLOCK-MAP-EXPLICIT-VALUE 
l_block_map_explicit_value() { local n="$1"
    peg_seq _w674 _w675 _w676
}

# [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
ns_l_block_map_implicit_entry() { local n="$1"
    build_p "PAIR" _w682
}

# [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
ns_s_block_map_implicit_key() {
    peg_alt _w683 _w684
}

# [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
c_l_block_map_implicit_value() { local n="$1"
    peg_seq _w685 _w691
}

# [195] NS-L-COMPACT-MAPPING 
ns_l_compact_mapping() { local n="$1"
    peg_seq _w692 _w696
}

# [196] S-L+BLOCK-NODE 
s_lblock_node() { local n="$1"; c="$2"
    peg_alt _w697 _w698
}

# [197] S-L+FLOW-IN-BLOCK 
s_lflow_in_block() { local n="$1"
    peg_seq _w699 _w700 _w701
}

# [198] S-L+BLOCK-IN-BLOCK 
s_lblock_in_block() { local n="$1"; c="$2"
    peg_alt _w702 _w703
}

# [199] S-L+BLOCK-SCALAR 
s_lblock_scalar() { local n="$1"; c="$2"
    peg_seq _w704 _w708 _w711
}

# [200] S-L+BLOCK-COLLECTION 
s_lblock_collection() { local n="$1"; c="$2"
    peg_seq _w715 _w716 _w719
}

# [202] L-DOCUMENT-PREFIX 
l_document_prefix() {
    peg_seq _w721 _w723
}

# [203] C-DIRECTIVES-END 
c_directives_end() {
    match_str "---"
}

# [204] C-DOCUMENT-END 
c_document_end() {
    match_str "..."
}

# [205] L-DOCUMENT-SUFFIX 
l_document_suffix() {
    peg_seq _w724 _w725
}

# [206] C-FORBIDDEN 
c_forbidden() {
    peg_seq _w726 _w729 _w733
}

# [207] L-BARE-DOCUMENT 
l_bare_document() {
    build_p "DOC" _w734
}

# [208] L-EXPLICIT-DOCUMENT 
l_explicit_document() {
    build_p "DOC" _w741
}

# [209] L-DIRECTIVE-DOCUMENT 
l_directive_document() {
    peg_seq _w743 _w744
}

# [210] L-ANY-DOCUMENT 
l_any_document() {
    peg_alt _w745 _w746 _w747
}

# [211] L-YAML-STREAM 
l_yaml_stream() {
    build_p "STREAM" _w766
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

# ── Native Value Type ──

# ── Native Value Type (parallel arrays) ──

# Value types: null bool int float str map seq
YV_TYPE=()    # type tag
YV_STR=()     # string/bool/int/float value as string
YV_KIDS=()    # space-separated child value indices (for map/seq)
YV_KEYS=()    # space-separated key indices (for map only)
YV_N=0

yv_null()  { local i=$YV_N; YV_TYPE[$i]="null";  YV_STR[$i]="";   YV_KIDS[$i]=""; YV_KEYS[$i]=""; YV_N=$((YV_N+1)); RESULT=$i; }
yv_bool()  { local i=$YV_N; YV_TYPE[$i]="bool";  YV_STR[$i]="$1"; YV_KIDS[$i]=""; YV_KEYS[$i]=""; YV_N=$((YV_N+1)); RESULT=$i; }
yv_int()   { local i=$YV_N; YV_TYPE[$i]="int";   YV_STR[$i]="$1"; YV_KIDS[$i]=""; YV_KEYS[$i]=""; YV_N=$((YV_N+1)); RESULT=$i; }
yv_float() { local i=$YV_N; YV_TYPE[$i]="float"; YV_STR[$i]="$1"; YV_KIDS[$i]=""; YV_KEYS[$i]=""; YV_N=$((YV_N+1)); RESULT=$i; }
yv_str()   { local i=$YV_N; YV_TYPE[$i]="str";   YV_STR[$i]="$1"; YV_KIDS[$i]=""; YV_KEYS[$i]=""; YV_N=$((YV_N+1)); RESULT=$i; }
yv_seq()   { local i=$YV_N; YV_TYPE[$i]="seq";   YV_STR[$i]="";   YV_KIDS[$i]="$1"; YV_KEYS[$i]=""; YV_N=$((YV_N+1)); RESULT=$i; }
yv_map()   { local i=$YV_N; YV_TYPE[$i]="map";   YV_STR[$i]="";   YV_KIDS[$i]="$1"; YV_KEYS[$i]="$2"; YV_N=$((YV_N+1)); RESULT=$i; }

yv_get() {
    # yv_get map_idx key_string
    local mi=$1 key="$2"
    local keys=(${YV_KEYS[$mi]}) vals=(${YV_KIDS[$mi]})
    for ki in "${!keys[@]}"; do
        if [[ "${YV_STR[${keys[$ki]}]}" == "$key" ]]; then
            RESULT=${vals[$ki]}; return
        fi
    done
    yv_null
}

yv_at() {
    # yv_at seq_idx position
    local si=$1 pos=$2
    local kids=(${YV_KIDS[$si]})
    if [[ $pos -lt ${#kids[@]} ]]; then
        RESULT=${kids[$pos]}
    else
        yv_null
    fi
}

yv_to_str() { RESULT="${YV_STR[$1]}"; }

# ── Schema Coercion ──

coerce_scalar() {
    local s="$1"
    case "$s" in
        null|Null|NULL|"~"|"") yv_null ;;
        true|True|TRUE) yv_bool "true" ;;
        false|False|FALSE) yv_bool "false" ;;
        .inf|.Inf|.INF|+.inf) yv_float "inf" ;;
        -.inf|-.Inf|-.INF) yv_float "-inf" ;;
        .nan|.NaN|.NAN) yv_float "nan" ;;
        0x*|0X*)
            local hex="${s:2}"
            if [[ "$hex" =~ ^[0-9a-fA-F]+$ ]]; then
                yv_int "$((16#$hex))"
            else yv_str "$s"; fi ;;
        0o*|0O*)
            local oct="${s:2}"
            if [[ "$oct" =~ ^[0-7]+$ ]]; then
                yv_int "$((8#$oct))"
            else yv_str "$s"; fi ;;
        *)
            if [[ "$s" =~ ^-?[0-9]+$ ]]; then
                yv_int "$s"
            elif [[ "$s" =~ ^-?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?$ ]]; then
                yv_float "$s"
            else
                yv_str "$s"
            fi ;;
    esac
}

# ── AST → Native Conversion with Anchor Resolution ──

# ── Converter ──
# Anchors stored as: ANCHOR_NAMES=() ANCHOR_VALS=()
ANCHOR_NAMES=()
ANCHOR_VALS=()

convert() {
    local idx=$1
    if [[ ${AST_LEAF[$idx]} -eq 1 ]]; then
        coerce_scalar "${AST_TEXT[$idx]}"
        return
    fi
    local t="${AST_TAG[$idx]}"
    local kids=(${AST_KIDS[$idx]})

    case "$t" in
        ANCHOR)
            local aname="" aval=-1
            for ci in "${kids[@]}"; do
                if [[ ${AST_LEAF[$ci]} -eq 1 && -z "$aname" ]]; then
                    aname="${AST_TEXT[$ci]}"
                else
                    convert $ci; aval=$RESULT
                fi
            done
            if [[ -n "$aname" && $aval -ge 0 ]]; then
                ANCHOR_NAMES+=("$aname"); ANCHOR_VALS+=($aval)
            fi
            RESULT=$aval ;;
        ALIAS)
            for ci in "${kids[@]}"; do
                if [[ ${AST_LEAF[$ci]} -eq 1 ]]; then
                    local aname="${AST_TEXT[$ci]}"
                    for ai in "${!ANCHOR_NAMES[@]}"; do
                        if [[ "${ANCHOR_NAMES[$ai]}" == "$aname" ]]; then
                            RESULT=${ANCHOR_VALS[$ai]}; return
                        fi
                    done
                fi
            done
            yv_null ;;
        MAPPING)
            local mkeys="" mvals=""
            for ci in "${kids[@]}"; do
                if [[ "${AST_TAG[$ci]}" == "PAIR" ]]; then
                    local pkids=(${AST_KIDS[$ci]})
                    if [[ ${#pkids[@]} -ge 2 ]]; then
                        convert ${pkids[0]}; local ki=$RESULT
                        convert ${pkids[1]}; local vi=$RESULT
                        mkeys+="$ki "; mvals+="$vi "
                    fi
                fi
            done
            yv_map "$mvals" "$mkeys" ;;
        SEQUENCE)
            local sitems=""
            for ci in "${kids[@]}"; do
                convert $ci; sitems+="$RESULT "
            done
            yv_seq "$sitems" ;;
        DOC|STREAM)
            if [[ ${#kids[@]} -eq 1 ]]; then
                convert ${kids[0]}
            else
                local ditems=""
                for ci in "${kids[@]}"; do
                    convert $ci; ditems+="$RESULT "
                done
                local ditems_a=($ditems)
                if [[ ${#ditems_a[@]} -eq 1 ]]; then
                    RESULT=${ditems_a[0]}
                else
                    yv_seq "$ditems"
                fi
            fi ;;
        PAIR)
            if [[ ${#kids[@]} -ge 2 ]]; then
                convert ${kids[1]}
            else yv_null; fi ;;
        *)
            if [[ ${#kids[@]} -eq 1 ]]; then
                convert ${kids[0]}
            else
                local items=""
                for ci in "${kids[@]}"; do
                    convert $ci; items+="$RESULT "
                done
                yv_seq "$items"
            fi ;;
    esac
}

# ── Public API ──

yaml_load() {
    local text="$1"
    init_input "$text"
    ANCHOR_NAMES=(); ANCHOR_VALS=()
    AST_TAG=(); AST_TEXT=(); AST_KIDS=(); AST_LEAF=(); AST_N=0
    YV_TYPE=(); YV_STR=(); YV_KIDS=(); YV_KEYS=(); YV_N=0
    l_yaml_stream
    if [[ $FAILED -eq 0 && $RAST -ge 0 ]]; then
        convert $RAST
    else
        yv_null
    fi
}

# ── Main ──

usage() {
    echo "Usage: $0 [file]" >&2
    echo "  Reads YAML from file or stdin." >&2
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
    l_yaml_stream

    if [[ $FAILED -eq 0 ]]; then
        echo "OK: $POS chars"
        [[ $RAST -ge 0 ]] && print_ast $RAST 0
    else
        echo "FAIL @$POS: $RTAG" >&2
        exit 1
    fi
}

main "$@"
