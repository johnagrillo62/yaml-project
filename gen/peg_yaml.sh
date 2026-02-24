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
_w85() { ns_uri_char; }
_w86() { c_tag; }
_w87() { c_flow_indicator; }
_w88() { peg_alt _w86 _w87; }
_w89() { match_cp 120; }
_w90() { ns_hex_digit; }
_w91() { rep_p 2 _w90; }
_w92() { match_cp 117; }
_w93() { ns_hex_digit; }
_w94() { rep_p 4 _w93; }
_w95() { match_cp 85; }
_w96() { ns_hex_digit; }
_w97() { rep_p 8 _w96; }
_w98() { c_escape; }
_w99() { ns_esc_null; }
_w100() { ns_esc_bell; }
_w101() { ns_esc_backspace; }
_w102() { ns_esc_horizontal_tab; }
_w103() { ns_esc_line_feed; }
_w104() { ns_esc_vertical_tab; }
_w105() { ns_esc_form_feed; }
_w106() { ns_esc_carriage_return; }
_w107() { ns_esc_escape; }
_w108() { ns_esc_space; }
_w109() { ns_esc_double_quote; }
_w110() { ns_esc_slash; }
_w111() { ns_esc_backslash; }
_w112() { ns_esc_next_line; }
_w113() { ns_esc_non_breaking_space; }
_w114() { ns_esc_line_separator; }
_w115() { ns_esc_paragraph_separator; }
_w116() { ns_esc_8_bit; }
_w117() { ns_esc_16_bit; }
_w118() { ns_esc_32_bit; }
_w119() { peg_alt _w99 _w100 _w101 _w102 _w103 _w104 _w105 _w106 _w107 _w108 _w109 _w110 _w111 _w112 _w113 _w114 _w115 _w116 _w117 _w118; }
_w120() { s_space; }
_w121() { s_space; }
_w122() { s_space; }
_w123() { s_white; }
_w124() { plus_p _w123; }
_w125() { ok_r; }
_w126() { s_indent $n; }
_w127() { s_separate_in_line; }
_w128() { opt_p _w127; }
_w129() { s_line_prefix $n $c; }
_w130() { s_indent_lt $n; }
_w131() { peg_alt _w129 _w130; }
_w132() { b_as_line_feed; }
_w133() { b_non_content; }
_w134() { l_empty $n $c; }
_w135() { plus_p _w134; }
_w136() { b_l_trimmed $n $c; }
_w137() { b_as_space; }
_w138() { s_separate_in_line; }
_w139() { opt_p _w138; }
_w140() { b_l_folded $n "FLOW-IN"; }
_w141() { s_flow_line_prefix $n; }
_w142() { c_comment; }
_w143() { nb_char; }
_w144() { star_p _w143; }
_w145() { b_non_content; }
_w146() { ok_r; }
_w147() { s_separate_in_line; }
_w148() { c_nb_comment_text; }
_w149() { opt_p _w148; }
_w150() { peg_seq _w147 _w149; }
_w151() { opt_p _w150; }
_w152() { b_comment; }
_w153() { s_separate_in_line; }
_w154() { c_nb_comment_text; }
_w155() { opt_p _w154; }
_w156() { b_non_content; }
_w157() { s_b_comment; }
_w158() { ok_r; }
_w159() { peg_alt _w157 _w158; }
_w160() { l_comment; }
_w161() { star_p _w160; }
_w162() { s_l_comments; }
_w163() { s_flow_line_prefix $n; }
_w164() { peg_seq _w162 _w163; }
_w165() { s_separate_in_line; }
_w166() { c_directive; }
_w167() { ns_yaml_directive; }
_w168() { ns_tag_directive; }
_w169() { ns_reserved_directive; }
_w170() { peg_alt _w167 _w168 _w169; }
_w171() { s_l_comments; }
_w172() { ns_directive_name; }
_w173() { s_separate_in_line; }
_w174() { ns_directive_parameter; }
_w175() { peg_seq _w173 _w174; }
_w176() { star_p _w175; }
_w177() { ns_char; }
_w178() { ns_char; }
_w179() { match_str "YAML"; }
_w180() { s_separate_in_line; }
_w181() { ns_yaml_version; }
_w182() { ns_dec_digit; }
_w183() { plus_p _w182; }
_w184() { match_cp 46; }
_w185() { ns_dec_digit; }
_w186() { plus_p _w185; }
_w187() { match_str "TAG"; }
_w188() { s_separate_in_line; }
_w189() { c_tag_handle; }
_w190() { s_separate_in_line; }
_w191() { ns_tag_prefix; }
_w192() { c_named_tag_handle; }
_w193() { c_secondary_tag_handle; }
_w194() { c_primary_tag_handle; }
_w195() { match_cp 33; }
_w196() { ns_word_char; }
_w197() { plus_p _w196; }
_w198() { match_cp 33; }
_w199() { c_ns_local_tag_prefix; }
_w200() { ns_global_tag_prefix; }
_w201() { match_cp 33; }
_w202() { ns_uri_char; }
_w203() { star_p _w202; }
_w204() { ns_tag_char; }
_w205() { ns_uri_char; }
_w206() { star_p _w205; }
_w207() { c_ns_tag_property; }
_w208() { s_separate $n $c; }
_w209() { c_ns_anchor_property; }
_w210() { peg_seq _w208 _w209; }
_w211() { opt_p _w210; }
_w212() { peg_seq _w207 _w211; }
_w213() { c_ns_anchor_property; }
_w214() { s_separate $n $c; }
_w215() { c_ns_tag_property; }
_w216() { peg_seq _w214 _w215; }
_w217() { opt_p _w216; }
_w218() { peg_seq _w213 _w217; }
_w219() { c_verbatim_tag; }
_w220() { c_ns_shorthand_tag; }
_w221() { c_non_specific_tag; }
_w222() { match_str "!<"; }
_w223() { ns_uri_char; }
_w224() { plus_p _w223; }
_w225() { match_cp 62; }
_w226() { c_tag_handle; }
_w227() { ns_tag_char; }
_w228() { plus_p _w227; }
_w229() { c_anchor; }
_w230() { ns_anchor_name; }
_w231() { scalar_p _w230; }
_w232() { peg_seq _w229 _w231; }
_w233() { ns_char; }
_w234() { c_flow_indicator; }
_w235() { ns_anchor_char; }
_w236() { c_alias; }
_w237() { ns_anchor_name; }
_w238() { scalar_p _w237; }
_w239() { peg_seq _w236 _w238; }
_w240() { c_ns_esc_char; }
_w241() { nb_json; }
_w242() { match_cp 92; }
_w243() { match_cp 34; }
_w244() { peg_alt _w242 _w243; }
_w245() { minus_p _w241 _w244; }
_w246() { nb_double_char; }
_w247() { s_white; }
_w248() { match_cp 34; }
_w249() { nb_double_text $n $c; }
_w250() { match_cp 34; }
_w251() { peg_seq _w248 _w249 _w250; }
_w252() { nb_double_char; }
_w253() { s_white; }
_w254() { star_p _w253; }
_w255() { match_cp 92; }
_w256() { b_non_content; }
_w257() { l_empty $n "FLOW-IN"; }
_w258() { star_p _w257; }
_w259() { s_flow_line_prefix $n; }
_w260() { s_double_escaped $n; }
_w261() { s_flow_folded $n; }
_w262() { s_white; }
_w263() { star_p _w262; }
_w264() { ns_double_char; }
_w265() { peg_seq _w263 _w264; }
_w266() { s_double_break $n; }
_w267() { ns_double_char; }
_w268() { nb_ns_double_in_line; }
_w269() { s_double_next_line $n; }
_w270() { s_white; }
_w271() { star_p _w270; }
_w272() { peg_alt _w269 _w271; }
_w273() { peg_seq _w267 _w268 _w272; }
_w274() { opt_p _w273; }
_w275() { nb_ns_double_in_line; }
_w276() { s_double_next_line $n; }
_w277() { s_white; }
_w278() { star_p _w277; }
_w279() { peg_alt _w276 _w278; }
_w280() { c_quoted_quote; }
_w281() { nb_json; }
_w282() { match_cp 39; }
_w283() { minus_p _w281 _w282; }
_w284() { nb_single_char; }
_w285() { s_white; }
_w286() { match_cp 39; }
_w287() { nb_single_text $n $c; }
_w288() { match_cp 39; }
_w289() { peg_seq _w286 _w287 _w288; }
_w290() { nb_single_char; }
_w291() { s_white; }
_w292() { star_p _w291; }
_w293() { ns_single_char; }
_w294() { peg_seq _w292 _w293; }
_w295() { s_flow_folded $n; }
_w296() { ns_single_char; }
_w297() { ns_single_in_line; }
_w298() { s_single_next_line $n; }
_w299() { s_white; }
_w300() { star_p _w299; }
_w301() { peg_alt _w298 _w300; }
_w302() { peg_seq _w296 _w297 _w301; }
_w303() { opt_p _w302; }
_w304() { ns_single_in_line; }
_w305() { s_single_next_line $n; }
_w306() { s_white; }
_w307() { star_p _w306; }
_w308() { peg_alt _w305 _w307; }
_w309() { ns_char; }
_w310() { c_indicator; }
_w311() { minus_p _w309 _w310; }
_w312() { match_cp 63; }
_w313() { match_cp 58; }
_w314() { match_cp 45; }
_w315() { peg_alt _w312 _w313 _w314; }
_w316() { ns_plain_safe $c; }
_w317() { ahead_p _w316; }
_w318() { peg_seq _w315 _w317; }
_w319() { ns_char; }
_w320() { c_flow_indicator; }
_w321() { ns_plain_safe $c; }
_w322() { match_cp 58; }
_w323() { match_cp 35; }
_w324() { peg_alt _w322 _w323; }
_w325() { minus_p _w321 _w324; }
_w326() { ns_char; }
_w327() { behind_p _w326; }
_w328() { match_cp 35; }
_w329() { peg_seq _w327 _w328; }
_w330() { match_cp 58; }
_w331() { ns_plain_safe $c; }
_w332() { ahead_p _w331; }
_w333() { peg_seq _w330 _w332; }
_w334() { case "$c" in
        FLOW-OUT) ns_plain_multi_line $n $c ;;
        FLOW-IN) ns_plain_multi_line $n $c ;;
        BLOCK-KEY) ns_plain_one_line $c ;;
        FLOW-KEY) ns_plain_one_line $c ;;
        *) fail_r "no case" ;;
    esac; }
_w335() { s_white; }
_w336() { star_p _w335; }
_w337() { ns_plain_char $c; }
_w338() { peg_seq _w336 _w337; }
_w339() { ns_plain_first $c; }
_w340() { nb_ns_plain_in_line $c; }
_w341() { s_flow_folded $n; }
_w342() { c_forbidden; }
_w343() { neg_p _w342; }
_w344() { ns_plain_char $c; }
_w345() { nb_ns_plain_in_line $c; }
_w346() { ns_plain_one_line $c; }
_w347() { s_ns_plain_next_line $n $c; }
_w348() { star_p _w347; }
_w349() { match_cp 91; }
_w350() { s_separate $n $c; }
_w351() { opt_p _w350; }
_w352() { ns_s_flow_seq_entries $n $(in_flow $c); }
_w353() { collect_p _w352; }
_w354() { opt_p _w353; }
_w355() { match_cp 93; }
_w356() { peg_seq _w349 _w351 _w354 _w355; }
_w357() { ns_flow_seq_entry $n $c; }
_w358() { s_separate $n $c; }
_w359() { opt_p _w358; }
_w360() { match_cp 44; }
_w361() { s_separate $n $c; }
_w362() { opt_p _w361; }
_w363() { ns_s_flow_seq_entries $n $c; }
_w364() { opt_p _w363; }
_w365() { peg_seq _w360 _w362 _w364; }
_w366() { opt_p _w365; }
_w367() { ns_flow_pair $n $c; }
_w368() { ns_flow_node $n $c; }
_w369() { match_cp 123; }
_w370() { s_separate $n $c; }
_w371() { opt_p _w370; }
_w372() { ns_s_flow_map_entries $n $(in_flow $c); }
_w373() { collect_p _w372; }
_w374() { opt_p _w373; }
_w375() { match_cp 125; }
_w376() { peg_seq _w369 _w371 _w374 _w375; }
_w377() { ns_flow_map_entry $n $c; }
_w378() { s_separate $n $c; }
_w379() { opt_p _w378; }
_w380() { match_cp 44; }
_w381() { s_separate $n $c; }
_w382() { opt_p _w381; }
_w383() { ns_s_flow_map_entries $n $c; }
_w384() { opt_p _w383; }
_w385() { peg_seq _w380 _w382 _w384; }
_w386() { opt_p _w385; }
_w387() { match_cp 63; }
_w388() { s_separate $n $c; }
_w389() { ns_flow_map_explicit_entry $n $c; }
_w390() { peg_seq _w387 _w388 _w389; }
_w391() { ns_flow_map_implicit_entry $n $c; }
_w392() { ns_flow_map_implicit_entry $n $c; }
_w393() { e_node; }
_w394() { e_node; }
_w395() { peg_seq _w393 _w394; }
_w396() { ns_flow_map_yaml_key_entry $n $c; }
_w397() { c_ns_flow_map_empty_key_entry $n $c; }
_w398() { c_ns_flow_map_json_key_entry $n $c; }
_w399() { peg_alt _w396 _w397 _w398; }
_w400() { ns_flow_yaml_node $n $c; }
_w401() { s_separate $n $c; }
_w402() { opt_p _w401; }
_w403() { c_ns_flow_map_separate_value $n $c; }
_w404() { peg_seq _w402 _w403; }
_w405() { e_node; }
_w406() { peg_alt _w404 _w405; }
_w407() { e_node; }
_w408() { c_ns_flow_map_separate_value $n $c; }
_w409() { match_cp 58; }
_w410() { ns_plain_safe $c; }
_w411() { neg_p _w410; }
_w412() { s_separate $n $c; }
_w413() { ns_flow_node $n $c; }
_w414() { peg_seq _w412 _w413; }
_w415() { e_node; }
_w416() { peg_alt _w414 _w415; }
_w417() { c_flow_json_node $n $c; }
_w418() { s_separate $n $c; }
_w419() { opt_p _w418; }
_w420() { c_ns_flow_map_adjacent_value $n $c; }
_w421() { peg_seq _w419 _w420; }
_w422() { e_node; }
_w423() { peg_alt _w421 _w422; }
_w424() { match_cp 58; }
_w425() { s_separate $n $c; }
_w426() { opt_p _w425; }
_w427() { ns_flow_node $n $c; }
_w428() { peg_seq _w426 _w427; }
_w429() { e_node; }
_w430() { peg_alt _w428 _w429; }
_w431() { match_cp 63; }
_w432() { s_separate $n $c; }
_w433() { ns_flow_map_explicit_entry $n $c; }
_w434() { peg_seq _w431 _w432 _w433; }
_w435() { ns_flow_pair_entry $n $c; }
_w436() { ns_flow_pair_yaml_key_entry $n $c; }
_w437() { c_ns_flow_map_empty_key_entry $n $c; }
_w438() { c_ns_flow_pair_json_key_entry $n $c; }
_w439() { ns_s_implicit_yaml_key "FLOW-KEY"; }
_w440() { c_ns_flow_map_separate_value $n $c; }
_w441() { c_s_implicit_json_key "FLOW-KEY"; }
_w442() { c_ns_flow_map_adjacent_value $n $c; }
_w443() { ns_flow_yaml_node 0 $c; }
_w444() { s_separate_in_line; }
_w445() { opt_p _w444; }
_w446() { c_flow_json_node 0 $c; }
_w447() { s_separate_in_line; }
_w448() { opt_p _w447; }
_w449() { c_flow_sequence $n $c; }
_w450() { c_flow_mapping $n $c; }
_w451() { c_single_quoted $n $c; }
_w452() { c_double_quoted $n $c; }
_w453() { ns_flow_yaml_content $n $c; }
_w454() { c_flow_json_content $n $c; }
_w455() { c_ns_alias_node; }
_w456() { ns_flow_yaml_content $n $c; }
_w457() { c_ns_properties $n $c; }
_w458() { s_separate $n $c; }
_w459() { ns_flow_yaml_content $n $c; }
_w460() { peg_seq _w458 _w459; }
_w461() { e_scalar; }
_w462() { peg_alt _w460 _w461; }
_w463() { peg_seq _w457 _w462; }
_w464() { c_ns_properties $n $c; }
_w465() { s_separate $n $c; }
_w466() { peg_seq _w464 _w465; }
_w467() { opt_p _w466; }
_w468() { c_flow_json_content $n $c; }
_w469() { c_ns_alias_node; }
_w470() { ns_flow_content $n $c; }
_w471() { c_ns_properties $n $c; }
_w472() { s_separate $n $c; }
_w473() { ns_flow_content $n $c; }
_w474() { peg_seq _w472 _w473; }
_w475() { e_scalar; }
_w476() { peg_alt _w474 _w475; }
_w477() { peg_seq _w471 _w476; }
_w478() { ns_dec_digit; }
_w479() { parse_int_p _w478; }
_w480() { detect_indent $n; }
_w481() { match_cp 45; }
_w482() { parse_sym_p _w481 "STRIP"; }
_w483() { match_cp 43; }
_w484() { parse_sym_p _w483 "KEEP"; }
_w485() { val_p "CLIP"; }
_w486() { peg_alt _w479 _w480; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_alt _w482 _w484 _w485; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; s_b_comment; }
_w487() { match_cp 45; }
_w488() { parse_sym_p _w487 "STRIP"; }
_w489() { match_cp 43; }
_w490() { parse_sym_p _w489 "KEEP"; }
_w491() { val_p "CLIP"; }
_w492() { ns_dec_digit; }
_w493() { parse_int_p _w492; }
_w494() { detect_indent $n; }
_w495() { peg_alt _w488 _w490 _w491; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; peg_alt _w493 _w494; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; s_b_comment; }
_w496() { ns_dec_digit; }
_w497() { ok_r; }
_w498() { match_cp 45; }
_w499() { match_cp 43; }
_w500() { ok_r; }
_w501() { s_indent_le $n; }
_w502() { b_non_content; }
_w503() { peg_seq _w501 _w502; }
_w504() { star_p _w503; }
_w505() { l_trail_comments $n; }
_w506() { opt_p _w505; }
_w507() { l_empty $n "BLOCK-IN"; }
_w508() { star_p _w507; }
_w509() { l_trail_comments $n; }
_w510() { opt_p _w509; }
_w511() { s_indent_lt $n; }
_w512() { c_nb_comment_text; }
_w513() { b_comment; }
_w514() { l_comment; }
_w515() { star_p _w514; }
_w516() { match_cp 124; }
_w517() { ns_dec_digit; }
_w518() { parse_int_p _w517; }
_w519() { detect_indent $n; }
_w520() { match_cp 45; }
_w521() { parse_sym_p _w520 "STRIP"; }
_w522() { match_cp 43; }
_w523() { parse_sym_p _w522 "KEEP"; }
_w524() { val_p "CLIP"; }
_w525() { s_b_comment; }
_w526() { l_literal_content $(( $n + $m )) $t; }
_w527() { peg_alt _w518 _w519; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_alt _w521 _w523 _w524; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; peg_seq _w525 _w526; }
_w528() { l_empty $n "BLOCK-IN"; }
_w529() { star_p _w528; }
_w530() { s_indent $n; }
_w531() { nb_char; }
_w532() { plus_p _w531; }
_w533() { b_as_line_feed; }
_w534() { l_nb_literal_text $n; }
_w535() { l_nb_literal_text $n; }
_w536() { b_nb_literal_next $n; }
_w537() { star_p _w536; }
_w538() { b_chomped_last $t; }
_w539() { peg_seq _w535 _w537 _w538; }
_w540() { opt_p _w539; }
_w541() { l_chomped_empty $n $t; }
_w542() { peg_seq _w540 _w541; }
_w543() { match_cp 62; }
_w544() { ns_dec_digit; }
_w545() { parse_int_p _w544; }
_w546() { detect_indent $n; }
_w547() { match_cp 45; }
_w548() { parse_sym_p _w547 "STRIP"; }
_w549() { match_cp 43; }
_w550() { parse_sym_p _w549 "KEEP"; }
_w551() { val_p "CLIP"; }
_w552() { s_b_comment; }
_w553() { l_folded_content $(( $n + $m )) $t; }
_w554() { peg_alt _w545 _w546; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_alt _w548 _w550 _w551; if [[ $FAILED -eq 1 ]]; then return; fi; local t="$RTAG"; save_inp; peg_seq _w552 _w553; }
_w555() { s_indent $n; }
_w556() { ns_char; }
_w557() { nb_char; }
_w558() { star_p _w557; }
_w559() { s_nb_folded_text $n; }
_w560() { b_l_folded $n "BLOCK-IN"; }
_w561() { s_nb_folded_text $n; }
_w562() { peg_seq _w560 _w561; }
_w563() { star_p _w562; }
_w564() { s_indent $n; }
_w565() { s_white; }
_w566() { nb_char; }
_w567() { star_p _w566; }
_w568() { b_as_line_feed; }
_w569() { l_empty $n "BLOCK-IN"; }
_w570() { star_p _w569; }
_w571() { s_nb_spaced_text $n; }
_w572() { b_l_spaced $n; }
_w573() { s_nb_spaced_text $n; }
_w574() { peg_seq _w572 _w573; }
_w575() { star_p _w574; }
_w576() { l_empty $n "BLOCK-IN"; }
_w577() { star_p _w576; }
_w578() { l_nb_folded_lines $n; }
_w579() { l_nb_spaced_lines $n; }
_w580() { peg_alt _w578 _w579; }
_w581() { l_nb_same_lines $n; }
_w582() { b_as_line_feed; }
_w583() { l_nb_same_lines $n; }
_w584() { peg_seq _w582 _w583; }
_w585() { star_p _w584; }
_w586() { l_nb_diff_lines $n; }
_w587() { b_chomped_last $t; }
_w588() { peg_seq _w586 _w587; }
_w589() { opt_p _w588; }
_w590() { l_chomped_empty $n $t; }
_w591() { peg_seq _w589 _w590; }
_w592() { s_indent $(( $n + $m )); }
_w593() { c_l_block_seq_entry $(( $n + $m )); }
_w594() { peg_seq _w592 _w593; }
_w595() { plus_p _w594; }
_w596() { detect_indent $n; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; collect_p _w595; }
_w597() { match_cp 45; }
_w598() { ns_char; }
_w599() { neg_p _w598; }
_w600() { s_lblock_indented $n "BLOCK-IN"; }
_w601() { s_indent $m; }
_w602() { ns_l_compact_sequence $(( $n + 1 + $m )); }
_w603() { ns_l_compact_mapping $(( $n + 1 + $m )); }
_w604() { peg_alt _w602 _w603; }
_w605() { detect_indent 0; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; peg_seq _w601 _w604; }
_w606() { s_lblock_node $n $c; }
_w607() { e_node; }
_w608() { s_l_comments; }
_w609() { peg_seq _w607 _w608; }
_w610() { c_l_block_seq_entry $n; }
_w611() { s_indent $n; }
_w612() { c_l_block_seq_entry $n; }
_w613() { peg_seq _w611 _w612; }
_w614() { star_p _w613; }
_w615() { s_indent $(( $n + $m )); }
_w616() { ns_l_block_map_entry $(( $n + $m )); }
_w617() { peg_seq _w615 _w616; }
_w618() { plus_p _w617; }
_w619() { detect_indent $n; if [[ $FAILED -eq 1 ]]; then return; fi; local m=$RTAGINT; save_inp; collect_p _w618; }
_w620() { c_l_block_map_explicit_entry $n; }
_w621() { ns_l_block_map_implicit_entry $n; }
_w622() { c_l_block_map_explicit_key $n; }
_w623() { l_block_map_explicit_value $n; }
_w624() { e_node; }
_w625() { peg_alt _w623 _w624; }
_w626() { match_cp 63; }
_w627() { s_lblock_indented $n "BLOCK-OUT"; }
_w628() { s_indent $n; }
_w629() { match_cp 58; }
_w630() { s_lblock_indented $n "BLOCK-OUT"; }
_w631() { ns_s_block_map_implicit_key; }
_w632() { e_node; }
_w633() { peg_alt _w631 _w632; }
_w634() { scalar_p _w633; }
_w635() { c_l_block_map_implicit_value $n; }
_w636() { peg_seq _w634 _w635; }
_w637() { c_s_implicit_json_key "BLOCK-KEY"; }
_w638() { ns_s_implicit_yaml_key "BLOCK-KEY"; }
_w639() { match_cp 58; }
_w640() { s_lblock_node $n "BLOCK-OUT"; }
_w641() { e_node; }
_w642() { s_l_comments; }
_w643() { peg_seq _w641 _w642; }
_w644() { scalar_p _w643; }
_w645() { peg_alt _w640 _w644; }
_w646() { ns_l_block_map_entry $n; }
_w647() { s_indent $n; }
_w648() { ns_l_block_map_entry $n; }
_w649() { peg_seq _w647 _w648; }
_w650() { star_p _w649; }
_w651() { s_lblock_in_block $n $c; }
_w652() { s_lflow_in_block $n; }
_w653() { s_separate $(( $n + 1 )) "FLOW-OUT"; }
_w654() { ns_flow_node $(( $n + 1 )) "FLOW-OUT"; }
_w655() { s_l_comments; }
_w656() { s_lblock_scalar $n $c; }
_w657() { s_lblock_collection $n $c; }
_w658() { s_separate $(( $n + 1 )) $c; }
_w659() { c_ns_properties $(( $n + 1 )) $c; }
_w660() { s_separate $(( $n + 1 )) $c; }
_w661() { peg_seq _w659 _w660; }
_w662() { opt_p _w661; }
_w663() { c_lliteral $n; }
_w664() { c_lfolded $n; }
_w665() { peg_alt _w663 _w664; }
_w666() { s_separate $(( $n + 1 )) $c; }
_w667() { c_ns_properties $(( $n + 1 )) $c; }
_w668() { peg_seq _w666 _w667; }
_w669() { opt_p _w668; }
_w670() { s_l_comments; }
_w671() { lblock_sequence $(seq_spaces $n $c); }
_w672() { lblock_mapping $n; }
_w673() { peg_alt _w671 _w672; }
_w674() { c_byte_order_mark; }
_w675() { opt_p _w674; }
_w676() { l_comment; }
_w677() { star_p _w676; }
_w678() { c_document_end; }
_w679() { s_l_comments; }
_w680() { sol_p; }
_w681() { c_directives_end; }
_w682() { c_document_end; }
_w683() { peg_alt _w681 _w682; }
_w684() { b_char; }
_w685() { s_white; }
_w686() { eof_p; }
_w687() { peg_alt _w684 _w685 _w686; }
_w688() { s_lblock_node -1 "BLOCK-IN"; }
_w689() { c_directives_end; }
_w690() { l_bare_document; }
_w691() { e_node; }
_w692() { s_l_comments; }
_w693() { peg_seq _w691 _w692; }
_w694() { peg_alt _w690 _w693; }
_w695() { peg_seq _w689 _w694; }
_w696() { l_directive; }
_w697() { plus_p _w696; }
_w698() { l_explicit_document; }
_w699() { l_directive_document; }
_w700() { l_explicit_document; }
_w701() { l_bare_document; }
_w702() { l_document_prefix; }
_w703() { star_p _w702; }
_w704() { l_any_document; }
_w705() { opt_p _w704; }
_w706() { l_document_suffix; }
_w707() { plus_p _w706; }
_w708() { l_document_prefix; }
_w709() { star_p _w708; }
_w710() { l_any_document; }
_w711() { opt_p _w710; }
_w712() { peg_seq _w707 _w709 _w711; }
_w713() { l_document_prefix; }
_w714() { star_p _w713; }
_w715() { l_explicit_document; }
_w716() { opt_p _w715; }
_w717() { peg_seq _w714 _w716; }
_w718() { peg_alt _w712 _w717; }
_w719() { star_p _w718; }
_w720() { peg_seq _w703 _w705 _w719; }

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
    peg_alt _w62 _w63 _w64 _w65 _w66 _w67 _w68 _w69 _w70 _w71 _w72 _w73 _w74 _w75 _w76 _w77 _w78 _w79 _w80 _w81 _w82 _w83 _w84
}

# [40] NS-TAG-CHAR 
ns_tag_char() {
    minus_p _w85 _w88
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
    peg_seq _w89 _w91
}

# [60] NS-ESC-16-BIT 
ns_esc_16_bit() {
    peg_seq _w92 _w94
}

# [61] NS-ESC-32-BIT 
ns_esc_32_bit() {
    peg_seq _w95 _w97
}

# [62] C-NS-ESC-CHAR 
c_ns_esc_char() {
    peg_seq _w98 _w119
}

# [63] S-INDENT 
s_indent() { local n="$1"
    rep_p $n _w120
}

# [64] S-INDENT-LT 
s_indent_lt() { local n="$1"
    star_p _w121
}

# [65] S-INDENT-LE 
s_indent_le() { local n="$1"
    star_p _w122
}

# [66] S-SEPARATE-IN-LINE 
s_separate_in_line() {
    peg_alt _w124 _w125
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
    peg_seq _w126 _w128
}

# [70] L-EMPTY 
l_empty() { local n="$1"; c="$2"
    peg_seq _w131 _w132
}

# [71] B-L-TRIMMED 
b_l_trimmed() { local n="$1"; c="$2"
    peg_seq _w133 _w135
}

# [72] B-AS-SPACE 
b_as_space() {
    b_break
}

# [73] B-L-FOLDED 
b_l_folded() { local n="$1"; c="$2"
    peg_alt _w136 _w137
}

# [74] S-FLOW-FOLDED 
s_flow_folded() { local n="$1"
    peg_seq _w139 _w140 _w141
}

# [75] C-NB-COMMENT-TEXT 
c_nb_comment_text() {
    peg_seq _w142 _w144
}

# [76] B-COMMENT 
b_comment() {
    peg_alt _w145 _w146
}

# [77] S-B-COMMENT 
s_b_comment() {
    peg_seq _w151 _w152
}

# [78] L-COMMENT 
l_comment() {
    peg_seq _w153 _w155 _w156
}

# [79] S-L-COMMENTS 
s_l_comments() {
    peg_seq _w159 _w161
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
    peg_alt _w164 _w165
}

# [82] L-DIRECTIVE 
l_directive() {
    peg_seq _w166 _w170 _w171
}

# [83] NS-RESERVED-DIRECTIVE 
ns_reserved_directive() {
    peg_seq _w172 _w176
}

# [84] NS-DIRECTIVE-NAME 
ns_directive_name() {
    plus_p _w177
}

# [85] NS-DIRECTIVE-PARAMETER 
ns_directive_parameter() {
    plus_p _w178
}

# [86] NS-YAML-DIRECTIVE 
ns_yaml_directive() {
    peg_seq _w179 _w180 _w181
}

# [87] NS-YAML-VERSION 
ns_yaml_version() {
    peg_seq _w183 _w184 _w186
}

# [88] NS-TAG-DIRECTIVE 
ns_tag_directive() {
    peg_seq _w187 _w188 _w189 _w190 _w191
}

# [89] C-TAG-HANDLE 
c_tag_handle() {
    peg_alt _w192 _w193 _w194
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
    peg_seq _w195 _w197 _w198
}

# [93] NS-TAG-PREFIX 
ns_tag_prefix() {
    peg_alt _w199 _w200
}

# [94] C-NS-LOCAL-TAG-PREFIX 
c_ns_local_tag_prefix() {
    peg_seq _w201 _w203
}

# [95] NS-GLOBAL-TAG-PREFIX 
ns_global_tag_prefix() {
    peg_seq _w204 _w206
}

# [96] C-NS-PROPERTIES 
c_ns_properties() { local n="$1"; c="$2"
    peg_alt _w212 _w218
}

# [97] C-NS-TAG-PROPERTY 
c_ns_tag_property() {
    peg_alt _w219 _w220 _w221
}

# [98] C-VERBATIM-TAG 
c_verbatim_tag() {
    peg_seq _w222 _w224 _w225
}

# [99] C-NS-SHORTHAND-TAG 
c_ns_shorthand_tag() {
    peg_seq _w226 _w228
}

# [100] C-NON-SPECIFIC-TAG 
c_non_specific_tag() {
    match_cp 33
}

# [101] C-NS-ANCHOR-PROPERTY 
c_ns_anchor_property() {
    build_p "ANCHOR" _w232
}

# [102] NS-ANCHOR-CHAR 
ns_anchor_char() {
    minus_p _w233 _w234
}

# [103] NS-ANCHOR-NAME 
ns_anchor_name() {
    plus_p _w235
}

# [104] C-NS-ALIAS-NODE 
c_ns_alias_node() {
    build_p "ALIAS" _w239
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
    peg_alt _w240 _w245
}

# [108] NS-DOUBLE-CHAR 
ns_double_char() {
    minus_p _w246 _w247
}

# [109] C-DOUBLE-QUOTED 
c_double_quoted() { local n="$1"; c="$2"
    scalar_p _w251
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
    star_p _w252
}

# [112] S-DOUBLE-ESCAPED 
s_double_escaped() { local n="$1"
    peg_seq _w254 _w255 _w256 _w258 _w259
}

# [113] S-DOUBLE-BREAK 
s_double_break() { local n="$1"
    peg_alt _w260 _w261
}

# [114] NB-NS-DOUBLE-IN-LINE 
nb_ns_double_in_line() {
    star_p _w265
}

# [115] S-DOUBLE-NEXT-LINE 
s_double_next_line() { local n="$1"
    peg_seq _w266 _w274
}

# [116] NB-DOUBLE-MULTI-LINE 
nb_double_multi_line() { local n="$1"
    peg_seq _w275 _w279
}

# [117] C-QUOTED-QUOTE 
c_quoted_quote() {
    match_str "''"
}

# [118] NB-SINGLE-CHAR 
nb_single_char() {
    peg_alt _w280 _w283
}

# [119] NS-SINGLE-CHAR 
ns_single_char() {
    minus_p _w284 _w285
}

# [120] C-SINGLE-QUOTED 
c_single_quoted() { local n="$1"; c="$2"
    scalar_p _w289
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
    star_p _w290
}

# [123] NS-SINGLE-IN-LINE 
ns_single_in_line() {
    star_p _w294
}

# [124] S-SINGLE-NEXT-LINE 
s_single_next_line() { local n="$1"
    peg_seq _w295 _w303
}

# [125] NB-SINGLE-MULTI-LINE 
nb_single_multi_line() { local n="$1"
    peg_seq _w304 _w308
}

# [126] NS-PLAIN-FIRST 
ns_plain_first() { local c="$1"
    peg_alt _w311 _w318
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
    minus_p _w319 _w320
}

# [130] NS-PLAIN-CHAR 
ns_plain_char() { local c="$1"
    peg_alt _w325 _w329 _w333
}

# [131] NS-PLAIN 
ns_plain() { local n="$1"; c="$2"
    scalar_p _w334
}

# [132] NB-NS-PLAIN-IN-LINE 
nb_ns_plain_in_line() { local c="$1"
    star_p _w338
}

# [133] NS-PLAIN-ONE-LINE 
ns_plain_one_line() { local c="$1"
    peg_seq _w339 _w340
}

# [134] S-NS-PLAIN-NEXT-LINE 
s_ns_plain_next_line() { local n="$1"; c="$2"
    peg_seq _w341 _w343 _w344 _w345
}

# [135] NS-PLAIN-MULTI-LINE 
ns_plain_multi_line() { local n="$1"; c="$2"
    peg_seq _w346 _w348
}

# [137] C-FLOW-SEQUENCE 
c_flow_sequence() { local n="$1"; c="$2"
    build_p "SEQUENCE" _w356
}

# [138] NS-S-FLOW-SEQ-ENTRIES 
ns_s_flow_seq_entries() { local n="$1"; c="$2"
    peg_seq _w357 _w359 _w366
}

# [139] NS-FLOW-SEQ-ENTRY 
ns_flow_seq_entry() { local n="$1"; c="$2"
    peg_alt _w367 _w368
}

# [140] C-FLOW-MAPPING 
c_flow_mapping() { local n="$1"; c="$2"
    build_p "MAPPING" _w376
}

# [141] NS-S-FLOW-MAP-ENTRIES 
ns_s_flow_map_entries() { local n="$1"; c="$2"
    peg_seq _w377 _w379 _w386
}

# [142] NS-FLOW-MAP-ENTRY 
ns_flow_map_entry() { local n="$1"; c="$2"
    peg_alt _w390 _w391
}

# [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
ns_flow_map_explicit_entry() { local n="$1"; c="$2"
    peg_alt _w392 _w395
}

# [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
ns_flow_map_implicit_entry() { local n="$1"; c="$2"
    build_p "PAIR" _w399
}

# [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
ns_flow_map_yaml_key_entry() { local n="$1"; c="$2"
    peg_seq _w400 _w406
}

# [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
c_ns_flow_map_empty_key_entry() { local n="$1"; c="$2"
    peg_seq _w407 _w408
}

# [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
c_ns_flow_map_separate_value() { local n="$1"; c="$2"
    peg_seq _w409 _w411 _w416
}

# [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
c_ns_flow_map_json_key_entry() { local n="$1"; c="$2"
    peg_seq _w417 _w423
}

# [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
c_ns_flow_map_adjacent_value() { local n="$1"; c="$2"
    peg_seq _w424 _w430
}

# [150] NS-FLOW-PAIR 
ns_flow_pair() { local n="$1"; c="$2"
    peg_alt _w434 _w435
}

# [151] NS-FLOW-PAIR-ENTRY 
ns_flow_pair_entry() { local n="$1"; c="$2"
    peg_alt _w436 _w437 _w438
}

# [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
ns_flow_pair_yaml_key_entry() { local n="$1"; c="$2"
    peg_seq _w439 _w440
}

# [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
c_ns_flow_pair_json_key_entry() { local n="$1"; c="$2"
    peg_seq _w441 _w442
}

# [154] NS-S-IMPLICIT-YAML-KEY 
ns_s_implicit_yaml_key() { local c="$1"
    peg_seq _w443 _w445
}

# [155] C-S-IMPLICIT-JSON-KEY 
c_s_implicit_json_key() { local c="$1"
    peg_seq _w446 _w448
}

# [156] NS-FLOW-YAML-CONTENT 
ns_flow_yaml_content() { local n="$1"; c="$2"
    ns_plain $n $c
}

# [157] C-FLOW-JSON-CONTENT 
c_flow_json_content() { local n="$1"; c="$2"
    peg_alt _w449 _w450 _w451 _w452
}

# [158] NS-FLOW-CONTENT 
ns_flow_content() { local n="$1"; c="$2"
    peg_alt _w453 _w454
}

# [159] NS-FLOW-YAML-NODE 
ns_flow_yaml_node() { local n="$1"; c="$2"
    peg_alt _w455 _w456 _w463
}

# [160] C-FLOW-JSON-NODE 
c_flow_json_node() { local n="$1"; c="$2"
    peg_seq _w467 _w468
}

# [161] NS-FLOW-NODE 
ns_flow_node() { local n="$1"; c="$2"
    peg_alt _w469 _w470 _w477
}

# [162] C-B-BLOCK-HEADER 
c_b_block_header() { local n="$1"
    peg_alt _w486 _w495
}

# [163] C-INDENTATION-INDICATOR 
c_indentation_indicator() { local n="$1"
    peg_alt _w496 _w497
}

# [164] C-CHOMPING-INDICATOR 
c_chomping_indicator() {
    peg_alt _w498 _w499 _w500
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
    peg_seq _w504 _w506
}

# [168] L-KEEP-EMPTY 
l_keep_empty() { local n="$1"
    peg_seq _w508 _w510
}

# [169] L-TRAIL-COMMENTS 
l_trail_comments() { local n="$1"
    peg_seq _w511 _w512 _w513 _w515
}

# [170] C-L+LITERAL 
c_lliteral() { local n="$1"
    peg_seq _w516 _w527
}

# [171] L-NB-LITERAL-TEXT 
l_nb_literal_text() { local n="$1"
    peg_seq _w529 _w530 _w532
}

# [172] B-NB-LITERAL-NEXT 
b_nb_literal_next() { local n="$1"
    peg_seq _w533 _w534
}

# [173] L-LITERAL-CONTENT 
l_literal_content() { local n="$1"; t="$2"
    scalar_p _w542
}

# [174] C-L+FOLDED 
c_lfolded() { local n="$1"
    peg_seq _w543 _w554
}

# [175] S-NB-FOLDED-TEXT 
s_nb_folded_text() { local n="$1"
    peg_seq _w555 _w556 _w558
}

# [176] L-NB-FOLDED-LINES 
l_nb_folded_lines() { local n="$1"
    peg_seq _w559 _w563
}

# [177] S-NB-SPACED-TEXT 
s_nb_spaced_text() { local n="$1"
    peg_seq _w564 _w565 _w567
}

# [178] B-L-SPACED 
b_l_spaced() { local n="$1"
    peg_seq _w568 _w570
}

# [179] L-NB-SPACED-LINES 
l_nb_spaced_lines() { local n="$1"
    peg_seq _w571 _w575
}

# [180] L-NB-SAME-LINES 
l_nb_same_lines() { local n="$1"
    peg_seq _w577 _w580
}

# [181] L-NB-DIFF-LINES 
l_nb_diff_lines() { local n="$1"
    peg_seq _w581 _w585
}

# [182] L-FOLDED-CONTENT 
l_folded_content() { local n="$1"; t="$2"
    scalar_p _w591
}

# [183] L+BLOCK-SEQUENCE 
lblock_sequence() { local n="$1"
    build_p "SEQUENCE" _w596
}

# [184] C-L-BLOCK-SEQ-ENTRY 
c_l_block_seq_entry() { local n="$1"
    peg_seq _w597 _w599 _w600
}

# [185] S-L+BLOCK-INDENTED 
s_lblock_indented() { local n="$1"; c="$2"
    peg_alt _w605 _w606 _w609
}

# [186] NS-L-COMPACT-SEQUENCE 
ns_l_compact_sequence() { local n="$1"
    peg_seq _w610 _w614
}

# [187] L+BLOCK-MAPPING 
lblock_mapping() { local n="$1"
    build_p "MAPPING" _w619
}

# [188] NS-L-BLOCK-MAP-ENTRY 
ns_l_block_map_entry() { local n="$1"
    peg_alt _w620 _w621
}

# [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
c_l_block_map_explicit_entry() { local n="$1"
    peg_seq _w622 _w625
}

# [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
c_l_block_map_explicit_key() { local n="$1"
    peg_seq _w626 _w627
}

# [191] L-BLOCK-MAP-EXPLICIT-VALUE 
l_block_map_explicit_value() { local n="$1"
    peg_seq _w628 _w629 _w630
}

# [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
ns_l_block_map_implicit_entry() { local n="$1"
    build_p "PAIR" _w636
}

# [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
ns_s_block_map_implicit_key() {
    peg_alt _w637 _w638
}

# [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
c_l_block_map_implicit_value() { local n="$1"
    peg_seq _w639 _w645
}

# [195] NS-L-COMPACT-MAPPING 
ns_l_compact_mapping() { local n="$1"
    peg_seq _w646 _w650
}

# [196] S-L+BLOCK-NODE 
s_lblock_node() { local n="$1"; c="$2"
    peg_alt _w651 _w652
}

# [197] S-L+FLOW-IN-BLOCK 
s_lflow_in_block() { local n="$1"
    peg_seq _w653 _w654 _w655
}

# [198] S-L+BLOCK-IN-BLOCK 
s_lblock_in_block() { local n="$1"; c="$2"
    peg_alt _w656 _w657
}

# [199] S-L+BLOCK-SCALAR 
s_lblock_scalar() { local n="$1"; c="$2"
    peg_seq _w658 _w662 _w665
}

# [200] S-L+BLOCK-COLLECTION 
s_lblock_collection() { local n="$1"; c="$2"
    peg_seq _w669 _w670 _w673
}

# [202] L-DOCUMENT-PREFIX 
l_document_prefix() {
    peg_seq _w675 _w677
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
    peg_seq _w678 _w679
}

# [206] C-FORBIDDEN 
c_forbidden() {
    peg_seq _w680 _w683 _w687
}

# [207] L-BARE-DOCUMENT 
l_bare_document() {
    build_p "DOC" _w688
}

# [208] L-EXPLICIT-DOCUMENT 
l_explicit_document() {
    build_p "DOC" _w695
}

# [209] L-DIRECTIVE-DOCUMENT 
l_directive_document() {
    peg_seq _w697 _w698
}

# [210] L-ANY-DOCUMENT 
l_any_document() {
    peg_alt _w699 _w700 _w701
}

# [211] L-YAML-STREAM 
l_yaml_stream() {
    build_p "STREAM" _w720
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
