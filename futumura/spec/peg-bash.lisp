;;;; peg-bash.lisp — Bash target for emit-yaml-peg.lisp
;;;;
;;;; Bash has no closures, no data structures, no return values.
;;;; Strategy: every combinator is a function that reads/writes globals.
;;;; Grammar rules become functions. Seq/Alt become loops over
;;;; function-name arrays. AST is a flat parallel-array tree.

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Bash")
(def-tgt "default-output" "yaml_reader.sh")
(def-tgt "comment-prefix" "#")
(def-tgt "call-style" "bash")

(def-tgt "keywords"
  '("case" "do" "done" "elif" "else" "esac" "fi" "for" "function"
    "if" "in" "select" "then" "until" "while" "time"
    "declare" "local" "readonly" "export" "unset"
    "true" "false" "test"))
(def-tgt "keyword-prefix" "r_")

;;; ── Identifier rules ──

(def-tgt "ident-prefix" "l_")
(def-tgt "ident-transform" :snake)

;;; ── Closure wrapping ──
;;; Bash can't do closures. We emit function names as strings
;;; and call them indirectly. ref-wrap and box-wrap return
;;; the function name as a string to be called later.

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    ;; In bash, "closures" are just function calls.
    ;; body is already a function call string like "l_some_rule"
    body))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    body))

;;; ── Seq/Alt ──
;;; These call peg_seq / peg_alt with function names as arguments

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "peg_seq ~{~A~^ ~}" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "peg_alt ~{~A~^ ~}" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (with-output-to-string (s)
      (format s "case \"$~A\" in~%" param)
      (loop for (val body) in cases
            do (format s "        ~A) ~A ;;~%" val body))
      (format s "        *) fail_r \"no case\" ;;~%    esac"))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "~A; if [[ $FAILED -eq 1 ]]; then return; fi; local ~A=$RTAGINT; save_inp; ~A"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "~A; if [[ $FAILED -eq 1 ]]; then return; fi; local ~A=\"$RTAG\"; save_inp; ~A"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (format nil "$~A" (peg-ident sym))))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) name))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "$(in_flow ~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "$(seq_spaces ~A ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A() { local ~{~A=\"$~D\"~^; ~}"
                name
                (loop for p in params
                      for i from 1
                      collect (peg-ident p) collect i))
        (format nil "~A() {" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "~A~%    ~A~%}" sig body)))

(def-tgt "fwd-decl" nil) ;; Bash doesn't need forward declarations

;;; ── Header ──

(def-tgt "header"
"#!/usr/bin/env bash
set -eo pipefail")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"# ── Input ──

SRC=\"\"
POS=0; LINE=1; COL=0; LEN=0

init_input() {
    SRC=\"$1\"; LEN=${#SRC}
    POS=0; LINE=1; COL=0
}

at_eof() { [[ $POS -ge $LEN ]]; }

peek_cp() {
    if [[ $POS -ge $LEN ]]; then PEEK=-1; return; fi
    LC_CTYPE=C printf -v PEEK '%d' \"'${SRC:$POS:1}\"
}

adv() {
    if [[ $POS -ge $LEN ]]; then return; fi
    local ch=\"${SRC:$POS:1}\"
    POS=$((POS+1))
    if [[ \"$ch\" == $'\\n' ]]; then
        LINE=$((LINE+1)); COL=0
    else
        COL=$((COL+1))
    fi
}

# Save/restore input position
save_inp()    { _SP=$POS; _SL=$LINE; _SC=$COL; }
restore_inp() { POS=$_SP; LINE=$_SL; COL=$_SC; }"

"# ── AST (flat parallel arrays) ──

AST_TAG=()    # tag or \"SCALAR\"
AST_TEXT=()   # leaf text (empty for branches)
AST_KIDS=()   # space-separated child indices
AST_LEAF=()   # 1=leaf, 0=branch
AST_N=0       # next free index

ast_branch() {
    local idx=$AST_N
    AST_TAG[$idx]=\"$1\"; AST_TEXT[$idx]=\"\"
    AST_KIDS[$idx]=\"\"; AST_LEAF[$idx]=0
    AST_N=$((AST_N+1)); RESULT=$idx
}

ast_leaf() {
    local idx=$AST_N
    AST_TAG[$idx]=\"SCALAR\"; AST_TEXT[$idx]=\"$1\"
    AST_KIDS[$idx]=\"\"; AST_LEAF[$idx]=1
    AST_N=$((AST_N+1)); RESULT=$idx
}

ast_add_child() {
    # ast_add_child parent child
    if [[ -z \"${AST_KIDS[$1]}\" ]]; then
        AST_KIDS[$1]=\"$2\"
    else
        AST_KIDS[$1]=\"${AST_KIDS[$1]} $2\"
    fi
}"

"# ── Result ──

FAILED=0
RVAL=\"\"
RPOS=0; RLINE=0; RCOL=0
RTAG=\"\"; RTAGINT=0
RAST=-1        # index into AST arrays, -1 = none
RAST_LIST=\"\"   # space-separated AST indices

ok_r() {
    FAILED=0; RVAL=\"\"; RPOS=$POS; RLINE=$LINE; RCOL=$COL
    RTAG=\"\"; RTAGINT=0; RAST=-1; RAST_LIST=\"\"
}

ok_v() {
    FAILED=0; RVAL=\"$1\"; RPOS=$POS; RLINE=$LINE; RCOL=$COL
    RTAG=\"\"; RTAGINT=0; RAST=-1; RAST_LIST=\"\"
}

fail_r() {
    FAILED=1; RVAL=\"\"; RPOS=$POS; RLINE=$LINE; RCOL=$COL
    RTAG=\"\"; RTAGINT=0; RAST=-1; RAST_LIST=\"\"
}"

"# ── Context ──

in_flow() {
    case \"$1\" in
        FLOW-OUT|FLOW-IN) echo \"FLOW-IN\" ;;
        *) echo \"FLOW-KEY\" ;;
    esac
}

seq_spaces() {
    if [[ \"$2\" == \"BLOCK-OUT\" ]]; then
        echo $(($1 - 1))
    else
        echo \"$1\"
    fi
}"

"# ── Combinators ──

match_cp() {
    peek_cp
    if [[ $PEEK -eq $1 ]]; then
        local ch=\"${SRC:$POS:1}\"
        adv; ok_v \"$ch\"
    else
        fail_r \"cp\"
    fi
}

match_range() {
    peek_cp
    if [[ $PEEK -ge $1 && $PEEK -le $2 ]]; then
        local ch=\"${SRC:$POS:1}\"
        adv; ok_v \"$ch\"
    else
        fail_r \"rng\"
    fi
}

match_str() {
    local t=\"$1\" tl=${#1}
    if [[ \"\${SRC:\$POS:\$tl}\" == \"$t\" ]]; then
        local i=0
        while [[ $i -lt $tl ]]; do adv; i=$((i+1)); done
        ok_v \"$t\"
    else
        fail_r \"str\"
    fi
}

peg_seq() {
    local acc=\"\" sp=$POS sl=$LINE sc=$COL
    local asts=\"\" _fn
    for _fn in \"$@\"; do
        \"$_fn\"
        if [[ $FAILED -eq 1 ]]; then
            POS=$sp; LINE=$sl; COL=$sc; return
        fi
        acc+=\"$RVAL\"
        [[ $RAST -ge 0 ]] && asts+=\"$RAST \"
        [[ -n \"$RAST_LIST\" ]] && asts+=\"$RAST_LIST \"
    done
    ok_v \"$acc\"
    RAST_LIST=\"$asts\"
}

peg_alt() {
    local sp=$POS sl=$LINE sc=$COL _fn
    for _fn in \"$@\"; do
        POS=$sp; LINE=$sl; COL=$sc
        \"$_fn\"
        if [[ $FAILED -eq 0 ]]; then return; fi
    done
    fail_r \"alt\"
}

star_p() {
    local fn=\"$1\" acc=\"\" asts=\"\"
    local sp=$POS
    while true; do
        \"$fn\"
        if [[ $FAILED -eq 1 || $POS -le $sp ]]; then
            FAILED=0; break
        fi
        acc+=\"$RVAL\"
        [[ $RAST -ge 0 ]] && asts+=\"$RAST \"
        [[ -n \"$RAST_LIST\" ]] && asts+=\"$RAST_LIST \"
        sp=$POS
    done
    ok_v \"$acc\"
    RAST_LIST=\"$asts\"
}

plus_p() {
    local fn=\"$1\"
    \"$fn\"
    if [[ $FAILED -eq 1 ]]; then return; fi
    local first_val=\"$RVAL\" first_ast=\"\"
    [[ $RAST -ge 0 ]] && first_ast=\"$RAST \"
    [[ -n \"$RAST_LIST\" ]] && first_ast+=\"$RAST_LIST \"
    star_p \"$fn\"
    RVAL=\"${first_val}${RVAL}\"
    RAST_LIST=\"${first_ast}${RAST_LIST}\"
}

opt_p() {
    local fn=\"$1\" sp=$POS sl=$LINE sc=$COL
    \"$fn\"
    if [[ $FAILED -eq 1 ]]; then
        POS=$sp; LINE=$sl; COL=$sc; ok_r
    fi
}

neg_p() {
    local fn=\"$1\" sp=$POS sl=$LINE sc=$COL
    \"$fn\"
    POS=$sp; LINE=$sl; COL=$sc
    if [[ $FAILED -eq 1 ]]; then ok_r
    else fail_r \"neg\"; fi
}

minus_p() {
    local fa=\"$1\" fb=\"$2\" sp=$POS sl=$LINE sc=$COL
    \"$fa\"
    if [[ $FAILED -eq 1 ]]; then return; fi
    local sv=\"$RVAL\" spos=$POS sline=$LINE scol=$COL
    local sast=$RAST sal=\"$RAST_LIST\"
    POS=$sp; LINE=$sl; COL=$sc
    \"$fb\"
    if [[ $FAILED -eq 0 ]]; then
        POS=$sp; LINE=$sl; COL=$sc
        fail_r \"minus\"
    else
        POS=$spos; LINE=$sline; COL=$scol
        ok_v \"$sv\"; RAST=$sast; RAST_LIST=\"$sal\"
    fi
}

rep_p() {
    local lo=$1 hi=$2; shift 2; local fn=\"$1\"
    local acc=\"\" asts=\"\" count=0
    while [[ $hi -eq -1 || $count -lt $hi ]]; do
        local sp=$POS
        \"$fn\"
        if [[ $FAILED -eq 1 || $POS -le $sp ]]; then FAILED=0; break; fi
        acc+=\"$RVAL\"
        [[ $RAST -ge 0 ]] && asts+=\"$RAST \"
        [[ -n \"$RAST_LIST\" ]] && asts+=\"$RAST_LIST \"
        count=$((count+1))
    done
    if [[ $count -lt $lo ]]; then fail_r \"rep\"; return; fi
    ok_v \"$acc\"; RAST_LIST=\"$asts\"
}

ahead_p() {
    local fn=\"$1\" sp=$POS sl=$LINE sc=$COL
    \"$fn\"
    POS=$sp; LINE=$sl; COL=$sc
    if [[ $FAILED -eq 0 ]]; then ok_r; fi
}

behind_p() {
    local fn=\"$1\"
    if [[ $POS -eq 0 ]]; then fail_r \"bh\"; return; fi
    local sp=$POS sl=$LINE sc=$COL
    POS=$((POS-1)); COL=$((COL > 0 ? COL-1 : 0))
    \"$fn\"
    POS=$sp; LINE=$sl; COL=$sc
    if [[ $FAILED -eq 0 ]]; then ok_r
    else fail_r \"bh\"; fi
}

sol_p() {
    if [[ $COL -eq 0 ]]; then ok_r
    else fail_r \"sol\"; fi
}

eof_p() {
    if at_eof; then ok_r
    else fail_r \"eof\"; fi
}"

"# ── YAML extensions ──

build_p() {
    local typ=\"$1\"; shift; local fn=\"$1\"
    \"$fn\"
    if [[ $FAILED -eq 1 ]]; then return; fi
    ast_branch \"$typ\"; local node=$RESULT
    for ci in $RAST_LIST; do
        [[ $ci -ge 0 ]] && ast_add_child $node $ci
    done
    if [[ $RAST -ge 0 ]]; then ast_add_child $node $RAST; fi
    RAST=$node; RAST_LIST=\"\"
}

scalar_p() {
    local fn=\"$1\"
    \"$fn\"
    if [[ $FAILED -eq 1 ]]; then return; fi
    ast_leaf \"$RVAL\"; RAST=$RESULT
}

collect_p() { \"$1\"; }

detect_indent() {
    local n=$1 i=$POS sp=0
    while [[ $((i+sp)) -lt $LEN && \"${SRC:$((i+sp)):1}\" == \" \" ]]; do
        sp=$((sp+1))
    done
    if [[ $((i+sp)) -lt $LEN && \"${SRC:$((i+sp)):1}\" != $'\\n' ]]; then
        ok_r; RTAGINT=$(( sp - n > 1 ? sp - n : 1 ))
        return
    fi
    local j=$((i+sp))
    while [[ $j -lt $LEN ]]; do
        if [[ \"${SRC:$j:1}\" == $'\\n' ]]; then
            j=$((j+1))
            [[ $j -ge $LEN ]] && { ok_r; RTAGINT=1; return; }
            sp=0
            while [[ $((j+sp)) -lt $LEN && \"${SRC:$((j+sp)):1}\" == \" \" ]]; do sp=$((sp+1)); done
            local nx=$((j+sp))
            if [[ $nx -ge $LEN || \"${SRC:$nx:1}\" == $'\\n' ]]; then
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
    local fn=\"$1\"
    \"$fn\"
    if [[ $FAILED -eq 1 ]]; then return; fi
    local digits=\"${RVAL//[!0-9]/}\"
    RTAGINT=${digits:-0}
}

parse_sym_p() {
    local fn=\"$1\" sym=\"$2\"
    \"$fn\"
    if [[ $FAILED -eq 1 ]]; then return; fi
    RTAG=\"$sym\"
}

val_p() { ok_r; RTAG=\"$1\"; }"))

;;; ── Combinator references ──

(def-tgt "comb-match-cp"    "match_cp")
(def-tgt "comb-match-range" "match_range")
(def-tgt "comb-match-str"   "match_str")
(def-tgt "comb-star"        "star_p")
(def-tgt "comb-plus"        "plus_p")
(def-tgt "comb-opt"         "opt_p")
(def-tgt "comb-neg"         "neg_p")
(def-tgt "comb-rep"         "rep_p")
(def-tgt "comb-ahead"       "ahead_p")
(def-tgt "comb-behind"      "behind_p")
(def-tgt "comb-minus"       "minus_p")
(def-tgt "comb-build"       "build_p")
(def-tgt "comb-scalar"      "scalar_p")
(def-tgt "comb-collect"     "collect_p")
(def-tgt "comb-sol"         "sol_p")
(def-tgt "comb-eof"         "eof_p")
(def-tgt "comb-ok"          "ok_r")
(def-tgt "comb-detect"      "detect_indent")
(def-tgt "comb-parse-int"   "parse_int_p")
(def-tgt "comb-parse-sym"   "parse_sym_p")
(def-tgt "comb-val"         "val_p")

;;; ── API ──

(def-tgt "api"
"# ── API ──

print_ast() {
    local idx=$1 depth=$2
    local indent=\"\"
    local d=0; while [[ $d -lt $depth ]]; do indent+=\"  \"; d=$((d+1)); done
    if [[ ${AST_LEAF[$idx]} -eq 1 ]]; then
        echo \"${indent}SCALAR: \\\"${AST_TEXT[$idx]}\\\"\"
    else
        echo \"${indent}${AST_TAG[$idx]}\"
        for child in ${AST_KIDS[$idx]}; do
            print_ast $child $((depth+1))
        done
    fi
}")

;;; ── Main ──

(def-tgt "main-fn"
"# ── Main ──

usage() {
    echo \"Usage: $0 [file]\" >&2
    echo \"  Reads YAML from file or stdin.\" >&2
    echo \"  If no file given and stdin is a terminal, shows this help.\" >&2
    exit 1
}

main() {
    local text
    if [[ $# -gt 0 ]]; then
        [[ ! -f \"$1\" ]] && { echo \"Error: file '$1' not found\" >&2; exit 1; }
        text=$(<\"$1\")
    elif [[ -t 0 ]]; then
        # stdin is a terminal — no piped input
        usage
    else
        text=$(cat)
    fi

    init_input \"$text\"
    l_yaml_stream

    if [[ $FAILED -eq 0 ]]; then
        echo \"OK: $POS chars\"
        [[ $RAST -ge 0 ]] && print_ast $RAST 0
    else
        echo \"FAIL @$POS: $RTAG\" >&2
        exit 1
    fi
}

main \"$@\"")

;;; ── Concern Vocab ──
;;; Bash concern vocab — native YAML value types using parallel arrays

(let ((cv (make-hash-table :test 'equal)))
  (setf (gethash "value-type-decl" cv)
"# ── Native Value Type (parallel arrays) ──

# Value types: null bool int float str map seq
YV_TYPE=()    # type tag
YV_STR=()     # string/bool/int/float value as string
YV_KIDS=()    # space-separated child value indices (for map/seq)
YV_KEYS=()    # space-separated key indices (for map only)
YV_N=0

yv_null()  { local i=$YV_N; YV_TYPE[$i]=\"null\";  YV_STR[$i]=\"\";   YV_KIDS[$i]=\"\"; YV_KEYS[$i]=\"\"; YV_N=$((YV_N+1)); RESULT=$i; }
yv_bool()  { local i=$YV_N; YV_TYPE[$i]=\"bool\";  YV_STR[$i]=\"$1\"; YV_KIDS[$i]=\"\"; YV_KEYS[$i]=\"\"; YV_N=$((YV_N+1)); RESULT=$i; }
yv_int()   { local i=$YV_N; YV_TYPE[$i]=\"int\";   YV_STR[$i]=\"$1\"; YV_KIDS[$i]=\"\"; YV_KEYS[$i]=\"\"; YV_N=$((YV_N+1)); RESULT=$i; }
yv_float() { local i=$YV_N; YV_TYPE[$i]=\"float\"; YV_STR[$i]=\"$1\"; YV_KIDS[$i]=\"\"; YV_KEYS[$i]=\"\"; YV_N=$((YV_N+1)); RESULT=$i; }
yv_str()   { local i=$YV_N; YV_TYPE[$i]=\"str\";   YV_STR[$i]=\"$1\"; YV_KIDS[$i]=\"\"; YV_KEYS[$i]=\"\"; YV_N=$((YV_N+1)); RESULT=$i; }
yv_seq()   { local i=$YV_N; YV_TYPE[$i]=\"seq\";   YV_STR[$i]=\"\";   YV_KIDS[$i]=\"$1\"; YV_KEYS[$i]=\"\"; YV_N=$((YV_N+1)); RESULT=$i; }
yv_map()   { local i=$YV_N; YV_TYPE[$i]=\"map\";   YV_STR[$i]=\"\";   YV_KIDS[$i]=\"$1\"; YV_KEYS[$i]=\"$2\"; YV_N=$((YV_N+1)); RESULT=$i; }

yv_get() {
    # yv_get map_idx key_string
    local mi=$1 key=\"$2\"
    local keys=(${YV_KEYS[$mi]}) vals=(${YV_KIDS[$mi]})
    for ki in \"${!keys[@]}\"; do
        if [[ \"${YV_STR[${keys[$ki]}]}\" == \"$key\" ]]; then
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

yv_to_str() { RESULT=\"${YV_STR[$1]}\"; }")

  (setf (gethash "coerce-fn" cv)
"coerce_scalar() {
    local s=\"$1\"
    case \"$s\" in
        null|Null|NULL|\"~\"|\"\") yv_null ;;
        true|True|TRUE) yv_bool \"true\" ;;
        false|False|FALSE) yv_bool \"false\" ;;
        .inf|.Inf|.INF|+.inf) yv_float \"inf\" ;;
        -.inf|-.Inf|-.INF) yv_float \"-inf\" ;;
        .nan|.NaN|.NAN) yv_float \"nan\" ;;
        0x*|0X*)
            local hex=\"${s:2}\"
            if [[ \"$hex\" =~ ^[0-9a-fA-F]+$ ]]; then
                yv_int \"$((16#$hex))\"
            else yv_str \"$s\"; fi ;;
        0o*|0O*)
            local oct=\"${s:2}\"
            if [[ \"$oct\" =~ ^[0-7]+$ ]]; then
                yv_int \"$((8#$oct))\"
            else yv_str \"$s\"; fi ;;
        *)
            if [[ \"$s\" =~ ^-?[0-9]+$ ]]; then
                yv_int \"$s\"
            elif [[ \"$s\" =~ ^-?[0-9]*\\.[0-9]+([eE][+-]?[0-9]+)?$ ]]; then
                yv_float \"$s\"
            else
                yv_str \"$s\"
            fi ;;
    esac
}")

  (setf (gethash "converter-decl" cv)
"# ── Converter ──
# Anchors stored as: ANCHOR_NAMES=() ANCHOR_VALS=()
ANCHOR_NAMES=()
ANCHOR_VALS=()")

  (setf (gethash "convert-fn" cv)
"convert() {
    local idx=$1
    if [[ ${AST_LEAF[$idx]} -eq 1 ]]; then
        coerce_scalar \"${AST_TEXT[$idx]}\"
        return
    fi
    local t=\"${AST_TAG[$idx]}\"
    local kids=(${AST_KIDS[$idx]})

    case \"$t\" in
        ANCHOR)
            local aname=\"\" aval=-1
            for ci in \"${kids[@]}\"; do
                if [[ ${AST_LEAF[$ci]} -eq 1 && -z \"$aname\" ]]; then
                    aname=\"${AST_TEXT[$ci]}\"
                else
                    convert $ci; aval=$RESULT
                fi
            done
            if [[ -n \"$aname\" && $aval -ge 0 ]]; then
                ANCHOR_NAMES+=(\"$aname\"); ANCHOR_VALS+=($aval)
            fi
            RESULT=$aval ;;
        ALIAS)
            for ci in \"${kids[@]}\"; do
                if [[ ${AST_LEAF[$ci]} -eq 1 ]]; then
                    local aname=\"${AST_TEXT[$ci]}\"
                    for ai in \"${!ANCHOR_NAMES[@]}\"; do
                        if [[ \"${ANCHOR_NAMES[$ai]}\" == \"$aname\" ]]; then
                            RESULT=${ANCHOR_VALS[$ai]}; return
                        fi
                    done
                fi
            done
            yv_null ;;
        MAPPING)
            local mkeys=\"\" mvals=\"\"
            for ci in \"${kids[@]}\"; do
                if [[ \"${AST_TAG[$ci]}\" == \"PAIR\" ]]; then
                    local pkids=(${AST_KIDS[$ci]})
                    if [[ ${#pkids[@]} -ge 2 ]]; then
                        convert ${pkids[0]}; local ki=$RESULT
                        convert ${pkids[1]}; local vi=$RESULT
                        mkeys+=\"$ki \"; mvals+=\"$vi \"
                    fi
                fi
            done
            yv_map \"$mvals\" \"$mkeys\" ;;
        SEQUENCE)
            local sitems=\"\"
            for ci in \"${kids[@]}\"; do
                convert $ci; sitems+=\"$RESULT \"
            done
            yv_seq \"$sitems\" ;;
        DOC|STREAM)
            if [[ ${#kids[@]} -eq 1 ]]; then
                convert ${kids[0]}
            else
                local ditems=\"\"
                for ci in \"${kids[@]}\"; do
                    convert $ci; ditems+=\"$RESULT \"
                done
                local ditems_a=($ditems)
                if [[ ${#ditems_a[@]} -eq 1 ]]; then
                    RESULT=${ditems_a[0]}
                else
                    yv_seq \"$ditems\"
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
                local items=\"\"
                for ci in \"${kids[@]}\"; do
                    convert $ci; items+=\"$RESULT \"
                done
                yv_seq \"$items\"
            fi ;;
    esac
}")

  (setf (gethash "load-fn" cv)
"yaml_load() {
    local text=\"$1\"
    init_input \"$text\"
    ANCHOR_NAMES=(); ANCHOR_VALS=()
    AST_TAG=(); AST_TEXT=(); AST_KIDS=(); AST_LEAF=(); AST_N=0
    YV_TYPE=(); YV_STR=(); YV_KIDS=(); YV_KEYS=(); YV_N=0
    l_yaml_stream
    if [[ $FAILED -eq 0 && $RAST -ge 0 ]]; then
        convert $RAST
    else
        yv_null
    fi
}")
  (def-tgt "cv" cv))
