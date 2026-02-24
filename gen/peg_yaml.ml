(* ════════════════════════════════════════════════════════════════ *)
(* peg_yaml.ml — YAML 1.2 parser                                   *)
(* ════════════════════════════════════════════════════════════════ *)


(* ── Input ── *)

type input = {
  src : string;
  pos : int;
  line : int;
  col : int;
}

let mk_input s = { src = s; pos = 0; line = 1; col = 0 }

let at_eof inp = inp.pos >= String.length inp.src

let peek_cp inp =
  if at_eof inp then -1
  else Char.code inp.src.[inp.pos]

let adv inp =
  if at_eof inp then inp
  else
    let c = inp.src.[inp.pos] in
    if c = '\n' then { inp with pos = inp.pos + 1; line = inp.line + 1; col = 0 }
    else { inp with pos = inp.pos + 1; col = inp.col + 1 }

(* ── AST ── *)

type ast =
  | Branch of string * ast list
  | Leaf of string

let ast_tag = function Branch (t, _) -> t | Leaf _ -> "SCALAR"
let ast_children = function Branch (_, cs) -> cs | Leaf _ -> []
let ast_is_leaf = function Leaf _ -> true | _ -> false
let ast_text = function Leaf t -> t | _ -> ""

(* ── Result ── *)

type result = {
  failed : bool;
  rval : string;
  rest : input;
  tag : string;
  tag_int : int;
  ast : ast option;
  ast_list : ast list;
  err : string;
}

let ok_r inp = { failed = false; rval = ""; rest = inp; tag = ""; tag_int = 0; ast = None; ast_list = []; err = "" }
let ok_v inp v = { (ok_r inp) with rval = v }
let fail_r inp m = { (ok_r inp) with failed = true; err = m }

(* ── Context ── *)

let in_flow c =
  if c = "FLOW-OUT" || c = "FLOW-IN" then "FLOW-IN"
  else "FLOW-KEY"

let seq_spaces n c =
  if c = "BLOCK-OUT" then n - 1 else n

(* ── Combinators ── *)

type pfn = input -> result

let merge_asts acc r =
  let a = match r.ast with Some x -> [x] | None -> [] in
  acc @ a @ r.ast_list

let rec match_cp cp inp =
  if peek_cp inp = cp then
    ok_v (adv inp) (String.make 1 (Char.chr cp))
  else fail_r inp "cp"

and match_range lo hi inp =
  let c = peek_cp inp in
  if c >= lo && c <= hi then
    ok_v (adv inp) (String.make 1 (Char.chr c))
  else fail_r inp "rng"

and match_str t inp =
  let tlen = String.length t in
  if inp.pos + tlen > String.length inp.src then fail_r inp "str"
  else if String.sub inp.src inp.pos tlen <> t then fail_r inp "str"
  else
    let rec advance i cur = if i >= tlen then cur else advance (i+1) (adv cur) in
    ok_v (advance 0 inp) t

and peg_seq fns inp =
  let rec go fs cur acc asts = match fs with
    | [] ->
      let res = ok_v cur acc in
      (match asts with [a] -> { res with ast = Some a } | [] -> res | _ -> { res with ast_list = asts })
    | f :: rest ->
      let r = f cur in
      if r.failed then r
      else go rest r.rest (acc ^ r.rval) (merge_asts asts r)
  in go fns inp "" []

and peg_alt fns inp = match fns with
  | [] -> fail_r inp "alt"
  | f :: rest ->
    let r = f inp in
    if r.failed then peg_alt rest inp else r

and star f inp =
  let rec go cur acc asts =
    let r = f cur in
    if r.failed || r.rest.pos <= cur.pos then
      let res = ok_v cur acc in
      if asts = [] then res else { res with ast_list = asts }
    else go r.rest (acc ^ r.rval) (merge_asts asts r)
  in go inp "" []

and plus_ f inp =
  let r = f inp in
  if r.failed then r
  else
    let r2 = star f r.rest in
    let asts = merge_asts (merge_asts [] r) r2 in
    let res = ok_v r2.rest (r.rval ^ r2.rval) in
    if asts = [] then res else { res with ast_list = asts }

and opt f inp =
  let r = f inp in
  if r.failed then ok_r inp else r

and neg f inp =
  let r = f inp in
  if r.failed then ok_r inp else fail_r inp "neg"

and minus fa fb inp =
  let ra = fa inp in
  if ra.failed then ra
  else
    let rb = fb inp in
    if not rb.failed && rb.rest.pos = ra.rest.pos then fail_r inp "excl"
    else ra

and rep count f inp =
  if count <= 0 then ok_v inp ""
  else
    let r = f inp in
    if r.failed then r
    else
      let r2 = rep (count - 1) f r.rest in
      if r2.failed then r2
      else ok_v r2.rest (r.rval ^ r2.rval)

and ahead f inp =
  let r = f inp in
  if r.failed then r else { (ok_r inp) with rval = r.rval }

and behind f inp =
  if inp.pos = 0 then fail_r inp "behind"
  else
    let prev = { inp with pos = inp.pos - 1; col = max 0 (inp.col - 1) } in
    let r = f prev in
    if r.failed then fail_r inp "behind" else ok_r inp

and sol inp =
  if inp.col = 0 then ok_r inp else fail_r inp "sol"

and eof_ok inp =
  if at_eof inp then ok_r inp else fail_r inp "eof"

(* ── YAML extensions ── *)

and build_ast typ f inp =
  let r = f inp in
  if r.failed then r
  else
    let children = (match r.ast with Some a -> [a] | None -> []) @ r.ast_list in
    { r with ast = Some (Branch (typ, children)); ast_list = [] }

and scalar_fn f inp =
  let r = f inp in
  if r.failed then r
  else { r with ast = Some (Leaf r.rval) }

and collect_fn f inp = f inp

and detect_indent n inp =
  let s = inp.src in
  let len = String.length s in
  let i = inp.pos in
  let sp = ref 0 in
  while i + !sp < len && s.[i + !sp] = ' ' do incr sp done;
  if i + !sp < len && s.[i + !sp] <> '\n' then
    { (ok_r inp) with tag_int = max 1 (!sp - n) }
  else
    let j = ref (i + !sp) in
    let result = ref None in
    while !j < len && !result = None do
      if s.[!j] = '\n' then begin
        j := !j + 1;
        if !j < len then begin
          sp := 0;
          while !j + !sp < len && s.[!j + !sp] = ' ' do incr sp done;
          let nx = !j + !sp in
          if nx >= len || s.[nx] = '\n' then j := nx
          else result := Some { (ok_r inp) with tag_int = max 1 (!sp - n) }
        end else result := Some { (ok_r inp) with tag_int = 1 }
      end else result := Some { (ok_r inp) with tag_int = 1 }
    done;
    (match !result with Some r -> r | None -> { (ok_r inp) with tag_int = 1 })

and parse_int_fn f inp =
  let r = f inp in
  if r.failed then r
  else
    let v = ref 0 in
    String.iter (fun c -> if c >= '0' && c <= '9' then v := !v * 10 + (Char.code c - Char.code '0')) r.rval;
    { r with tag_int = !v }

and parse_sym_fn f sym inp =
  let r = f inp in
  if r.failed then r
  else { r with tag = sym }

and val_fn v inp =
  { (ok_r inp) with tag = v }

(* ════════════════════════════════════════════════════════════════ *)
(* YAML 1.2 Grammar — 211 rules *)
(* ════════════════════════════════════════════════════════════════ *)

(* [1] C-PRINTABLE *)
and c_printable inp =
    peg_alt [
        (fun inp -> match_cp 0x9 inp);
        (fun inp -> match_cp 0x0A inp);
        (fun inp -> match_cp 0x0D inp);
        (fun inp -> match_range 0x20 0x7E inp);
        (fun inp -> match_cp 0x85 inp);
        (fun inp -> match_range 0xA0 0xD7FF inp);
        (fun inp -> match_range 0xE000 0xFFFD inp);
        (fun inp -> match_range 0x10000 0x10FFFF inp)] inp

(* [2] NB-JSON *)
and nb_json inp =
    peg_alt [(fun inp -> match_cp 0x9 inp); (fun inp -> match_range 0x20 0x10FFFF inp)] inp

(* [3] C-BYTE-ORDER-MARK *)
and c_byte_order_mark inp =
    match_cp 0xFEFF inp

(* [4] C-SEQUENCE-ENTRY *)
and c_sequence_entry inp =
    match_cp 45 inp

(* [5] C-MAPPING-KEY *)
and c_mapping_key inp =
    match_cp 63 inp

(* [6] C-MAPPING-VALUE *)
and c_mapping_value inp =
    match_cp 58 inp

(* [7] C-COLLECT-ENTRY *)
and c_collect_entry inp =
    match_cp 44 inp

(* [8] C-SEQUENCE-START *)
and c_sequence_start inp =
    match_cp 91 inp

(* [9] C-SEQUENCE-END *)
and c_sequence_end inp =
    match_cp 93 inp

(* [10] C-MAPPING-START *)
and c_mapping_start inp =
    match_cp 123 inp

(* [11] C-MAPPING-END *)
and c_mapping_end inp =
    match_cp 125 inp

(* [12] C-COMMENT *)
and c_comment inp =
    match_cp 35 inp

(* [13] C-ANCHOR *)
and c_anchor inp =
    match_cp 38 inp

(* [14] C-ALIAS *)
and c_alias inp =
    match_cp 42 inp

(* [15] C-TAG *)
and c_tag inp =
    match_cp 33 inp

(* [16] C-LITERAL *)
and c_literal inp =
    match_cp 124 inp

(* [17] C-FOLDED *)
and c_folded inp =
    match_cp 62 inp

(* [18] C-SINGLE-QUOTE *)
and c_single_quote inp =
    match_cp 39 inp

(* [19] C-DOUBLE-QUOTE *)
and c_double_quote inp =
    match_cp 34 inp

(* [20] C-DIRECTIVE *)
and c_directive inp =
    match_cp 37 inp

(* [21] C-RESERVED *)
and c_reserved inp =
    peg_alt [(fun inp -> match_cp 64 inp); (fun inp -> match_cp 96 inp)] inp

(* [22] C-INDICATOR *)
and c_indicator inp =
    peg_alt [
        (fun inp -> c_sequence_entry inp);
        (fun inp -> c_mapping_key inp);
        (fun inp -> c_mapping_value inp);
        (fun inp -> c_collect_entry inp);
        (fun inp -> c_sequence_start inp);
        (fun inp -> c_sequence_end inp);
        (fun inp -> c_mapping_start inp);
        (fun inp -> c_mapping_end inp);
        (fun inp -> c_comment inp);
        (fun inp -> c_anchor inp);
        (fun inp -> c_alias inp);
        (fun inp -> c_tag inp);
        (fun inp -> c_literal inp);
        (fun inp -> c_folded inp);
        (fun inp -> c_single_quote inp);
        (fun inp -> c_double_quote inp);
        (fun inp -> c_directive inp);
        (fun inp -> c_reserved inp)] inp

(* [23] C-FLOW-INDICATOR *)
and c_flow_indicator inp =
    peg_alt [
        (fun inp -> c_collect_entry inp);
        (fun inp -> c_sequence_start inp);
        (fun inp -> c_sequence_end inp);
        (fun inp -> c_mapping_start inp);
        (fun inp -> c_mapping_end inp)] inp

(* [24] B-LINE-FEED *)
and b_line_feed inp =
    match_cp 0x0A inp

(* [25] B-CARRIAGE-RETURN *)
and b_carriage_return inp =
    match_cp 0x0D inp

(* [26] B-CHAR *)
and b_char inp =
    peg_alt [(fun inp -> b_line_feed inp); (fun inp -> b_carriage_return inp)] inp

(* [27] NB-CHAR *)
and nb_char inp =
    minus (fun inp -> c_printable inp) (fun inp -> peg_alt [(fun inp -> b_char inp); (fun inp -> c_byte_order_mark inp)] inp) inp

(* [28] B-BREAK *)
and b_break inp =
    peg_alt [
        (fun inp -> peg_seq [(fun inp -> b_carriage_return inp); (fun inp -> b_line_feed inp)] inp);
        (fun inp -> b_carriage_return inp);
        (fun inp -> b_line_feed inp)] inp

(* [29] B-AS-LINE-FEED *)
and b_as_line_feed inp =
    b_break inp

(* [30] B-NON-CONTENT *)
and b_non_content inp =
    b_break inp

(* [31] S-SPACE *)
and s_space inp =
    match_cp 0x20 inp

(* [32] S-TAB *)
and s_tab inp =
    match_cp 0x9 inp

(* [33] S-WHITE *)
and s_white inp =
    peg_alt [(fun inp -> s_space inp); (fun inp -> s_tab inp)] inp

(* [34] NS-CHAR *)
and ns_char inp =
    minus (fun inp -> nb_char inp) (fun inp -> s_white inp) inp

(* [35] NS-DEC-DIGIT *)
and ns_dec_digit inp =
    match_range 0x30 0x39 inp

(* [36] NS-HEX-DIGIT *)
and ns_hex_digit inp =
    peg_alt [
        (fun inp -> ns_dec_digit inp);
        (fun inp -> match_range 0x41 0x46 inp);
        (fun inp -> match_range 0x61 0x66 inp)] inp

(* [37] NS-ASCII-LETTER *)
and ns_ascii_letter inp =
    peg_alt [(fun inp -> match_range 0x41 0x5A inp); (fun inp -> match_range 0x61 0x7A inp)] inp

(* [38] NS-WORD-CHAR *)
and ns_word_char inp =
    peg_alt [
        (fun inp -> ns_dec_digit inp);
        (fun inp -> ns_ascii_letter inp);
        (fun inp -> match_cp 45 inp)] inp

(* [39] NS-URI-CHAR *)
and ns_uri_char inp =
    peg_alt [
        (fun inp -> peg_seq [
            (fun inp -> match_cp 37 inp);
            (fun inp -> ns_hex_digit inp);
            (fun inp -> ns_hex_digit inp)] inp);
        (fun inp -> ns_word_char inp);
        (fun inp -> match_cp 35 inp);
        (fun inp -> match_cp 59 inp);
        (fun inp -> match_cp 47 inp);
        (fun inp -> match_cp 63 inp);
        (fun inp -> match_cp 58 inp);
        (fun inp -> match_cp 64 inp);
        (fun inp -> match_cp 38 inp);
        (fun inp -> match_cp 61 inp);
        (fun inp -> match_cp 43 inp);
        (fun inp -> match_cp 36 inp);
        (fun inp -> match_cp 44 inp);
        (fun inp -> match_cp 95 inp);
        (fun inp -> match_cp 46 inp);
        (fun inp -> match_cp 33 inp);
        (fun inp -> match_cp 126 inp);
        (fun inp -> match_cp 42 inp);
        (fun inp -> match_cp 39 inp);
        (fun inp -> match_cp 40 inp);
        (fun inp -> match_cp 41 inp);
        (fun inp -> match_cp 91 inp);
        (fun inp -> match_cp 93 inp)] inp

(* [40] NS-TAG-CHAR *)
and ns_tag_char inp =
    minus (fun inp -> ns_uri_char inp) (fun inp -> peg_alt [(fun inp -> c_tag inp); (fun inp -> c_flow_indicator inp)] inp) inp

(* [41] C-ESCAPE *)
and c_escape inp =
    match_cp 92 inp

(* [42] NS-ESC-NULL *)
and ns_esc_null inp =
    match_cp 48 inp

(* [43] NS-ESC-BELL *)
and ns_esc_bell inp =
    match_cp 97 inp

(* [44] NS-ESC-BACKSPACE *)
and ns_esc_backspace inp =
    match_cp 98 inp

(* [45] NS-ESC-HORIZONTAL-TAB *)
and ns_esc_horizontal_tab inp =
    match_cp 116 inp

(* [46] NS-ESC-LINE-FEED *)
and ns_esc_line_feed inp =
    match_cp 110 inp

(* [47] NS-ESC-VERTICAL-TAB *)
and ns_esc_vertical_tab inp =
    match_cp 118 inp

(* [48] NS-ESC-FORM-FEED *)
and ns_esc_form_feed inp =
    match_cp 102 inp

(* [49] NS-ESC-CARRIAGE-RETURN *)
and ns_esc_carriage_return inp =
    match_cp 114 inp

(* [50] NS-ESC-ESCAPE *)
and ns_esc_escape inp =
    match_cp 101 inp

(* [51] NS-ESC-SPACE *)
and ns_esc_space inp =
    match_cp 0x20 inp

(* [52] NS-ESC-DOUBLE-QUOTE *)
and ns_esc_double_quote inp =
    match_cp 34 inp

(* [53] NS-ESC-SLASH *)
and ns_esc_slash inp =
    match_cp 47 inp

(* [54] NS-ESC-BACKSLASH *)
and ns_esc_backslash inp =
    match_cp 92 inp

(* [55] NS-ESC-NEXT-LINE *)
and ns_esc_next_line inp =
    match_cp 78 inp

(* [56] NS-ESC-NON-BREAKING-SPACE *)
and ns_esc_non_breaking_space inp =
    match_cp 95 inp

(* [57] NS-ESC-LINE-SEPARATOR *)
and ns_esc_line_separator inp =
    match_cp 76 inp

(* [58] NS-ESC-PARAGRAPH-SEPARATOR *)
and ns_esc_paragraph_separator inp =
    match_cp 80 inp

(* [59] NS-ESC-8-BIT *)
and ns_esc_8_bit inp =
    peg_seq [(fun inp -> match_cp 120 inp); (fun inp -> rep 2 (fun inp -> ns_hex_digit inp) inp)] inp

(* [60] NS-ESC-16-BIT *)
and ns_esc_16_bit inp =
    peg_seq [(fun inp -> match_cp 117 inp); (fun inp -> rep 4 (fun inp -> ns_hex_digit inp) inp)] inp

(* [61] NS-ESC-32-BIT *)
and ns_esc_32_bit inp =
    peg_seq [(fun inp -> match_cp 85 inp); (fun inp -> rep 8 (fun inp -> ns_hex_digit inp) inp)] inp

(* [62] C-NS-ESC-CHAR *)
and c_ns_esc_char inp =
    peg_seq [
        (fun inp -> c_escape inp);
        (fun inp -> peg_alt [
            (fun inp -> ns_esc_null inp);
            (fun inp -> ns_esc_bell inp);
            (fun inp -> ns_esc_backspace inp);
            (fun inp -> ns_esc_horizontal_tab inp);
            (fun inp -> ns_esc_line_feed inp);
            (fun inp -> ns_esc_vertical_tab inp);
            (fun inp -> ns_esc_form_feed inp);
            (fun inp -> ns_esc_carriage_return inp);
            (fun inp -> ns_esc_escape inp);
            (fun inp -> ns_esc_space inp);
            (fun inp -> ns_esc_double_quote inp);
            (fun inp -> ns_esc_slash inp);
            (fun inp -> ns_esc_backslash inp);
            (fun inp -> ns_esc_next_line inp);
            (fun inp -> ns_esc_non_breaking_space inp);
            (fun inp -> ns_esc_line_separator inp);
            (fun inp -> ns_esc_paragraph_separator inp);
            (fun inp -> ns_esc_8_bit inp);
            (fun inp -> ns_esc_16_bit inp);
            (fun inp -> ns_esc_32_bit inp)] inp)] inp

(* [63] S-INDENT *)
and s_indent inp n =
    rep n (fun inp -> s_space inp) inp

(* [64] S-INDENT-LT *)
and s_indent_lt inp n =
    star (fun inp -> s_space inp) inp

(* [65] S-INDENT-LE *)
and s_indent_le inp n =
    star (fun inp -> s_space inp) inp

(* [66] S-SEPARATE-IN-LINE *)
and s_separate_in_line inp =
    peg_alt [(fun inp -> plus_ (fun inp -> s_white inp) inp); (fun inp -> ok_r inp)] inp

(* [67] S-LINE-PREFIX *)
and s_line_prefix inp n c =
    (match c with | "BLOCK-IN" -> s_block_line_prefix inp n | "BLOCK-OUT" -> s_block_line_prefix inp n | "FLOW-IN" -> s_flow_line_prefix inp n | "FLOW-OUT" -> s_flow_line_prefix inp n | _ -> fail_r inp "no case")

(* [68] S-BLOCK-LINE-PREFIX *)
and s_block_line_prefix inp n =
    s_indent inp n

(* [69] S-FLOW-LINE-PREFIX *)
and s_flow_line_prefix inp n =
    peg_seq [(fun inp -> s_indent inp n); (fun inp -> opt (fun inp -> s_separate_in_line inp) inp)] inp

(* [70] L-EMPTY *)
and l_empty inp n c =
    peg_seq [
        (fun inp -> peg_alt [(fun inp -> s_line_prefix inp n c); (fun inp -> s_indent_lt inp n)] inp);
        (fun inp -> b_as_line_feed inp)] inp

(* [71] B-L-TRIMMED *)
and b_l_trimmed inp n c =
    peg_seq [(fun inp -> b_non_content inp); (fun inp -> plus_ (fun inp -> l_empty inp n c) inp)] inp

(* [72] B-AS-SPACE *)
and b_as_space inp =
    b_break inp

(* [73] B-L-FOLDED *)
and b_l_folded inp n c =
    peg_alt [(fun inp -> b_l_trimmed inp n c); (fun inp -> b_as_space inp)] inp

(* [74] S-FLOW-FOLDED *)
and s_flow_folded inp n =
    peg_seq [
        (fun inp -> opt (fun inp -> s_separate_in_line inp) inp);
        (fun inp -> b_l_folded inp n "FLOW-IN");
        (fun inp -> s_flow_line_prefix inp n)] inp

(* [75] C-NB-COMMENT-TEXT *)
and c_nb_comment_text inp =
    peg_seq [(fun inp -> c_comment inp); (fun inp -> star (fun inp -> nb_char inp) inp)] inp

(* [76] B-COMMENT *)
and b_comment inp =
    peg_alt [(fun inp -> b_non_content inp); (fun inp -> ok_r inp)] inp

(* [77] S-B-COMMENT *)
and s_b_comment inp =
    peg_seq [
        (fun inp -> opt (fun inp -> peg_seq [
            (fun inp -> s_separate_in_line inp);
            (fun inp -> opt (fun inp -> c_nb_comment_text inp) inp)] inp) inp);
        (fun inp -> b_comment inp)] inp

(* [78] L-COMMENT *)
and l_comment inp =
    peg_seq [
        (fun inp -> s_separate_in_line inp);
        (fun inp -> opt (fun inp -> c_nb_comment_text inp) inp);
        (fun inp -> b_non_content inp)] inp

(* [79] S-L-COMMENTS *)
and s_l_comments inp =
    peg_seq [
        (fun inp -> peg_alt [(fun inp -> s_b_comment inp); (fun inp -> ok_r inp)] inp);
        (fun inp -> star (fun inp -> l_comment inp) inp)] inp

(* [80] S-SEPARATE *)
and s_separate inp n c =
    (match c with | "BLOCK-OUT" -> s_separate_lines inp n | "BLOCK-IN" -> s_separate_lines inp n | "FLOW-OUT" -> s_separate_lines inp n | "FLOW-IN" -> s_separate_lines inp n | "BLOCK-KEY" -> s_separate_in_line inp | "FLOW-KEY" -> s_separate_in_line inp | _ -> fail_r inp "no case")

(* [81] S-SEPARATE-LINES *)
and s_separate_lines inp n =
    peg_alt [
        (fun inp -> peg_seq [(fun inp -> s_l_comments inp); (fun inp -> s_flow_line_prefix inp n)] inp);
        (fun inp -> s_separate_in_line inp)] inp

(* [82] L-DIRECTIVE *)
and l_directive inp =
    peg_seq [
        (fun inp -> c_directive inp);
        (fun inp -> peg_alt [
            (fun inp -> ns_yaml_directive inp);
            (fun inp -> ns_tag_directive inp);
            (fun inp -> ns_reserved_directive inp)] inp);
        (fun inp -> s_l_comments inp)] inp

(* [83] NS-RESERVED-DIRECTIVE *)
and ns_reserved_directive inp =
    peg_seq [
        (fun inp -> ns_directive_name inp);
        (fun inp -> star (fun inp -> peg_seq [(fun inp -> s_separate_in_line inp); (fun inp -> ns_directive_parameter inp)] inp) inp)] inp

(* [84] NS-DIRECTIVE-NAME *)
and ns_directive_name inp =
    plus_ (fun inp -> ns_char inp) inp

(* [85] NS-DIRECTIVE-PARAMETER *)
and ns_directive_parameter inp =
    plus_ (fun inp -> ns_char inp) inp

(* [86] NS-YAML-DIRECTIVE *)
and ns_yaml_directive inp =
    peg_seq [
        (fun inp -> match_str "YAML" inp);
        (fun inp -> s_separate_in_line inp);
        (fun inp -> ns_yaml_version inp)] inp

(* [87] NS-YAML-VERSION *)
and ns_yaml_version inp =
    peg_seq [
        (fun inp -> plus_ (fun inp -> ns_dec_digit inp) inp);
        (fun inp -> match_cp 46 inp);
        (fun inp -> plus_ (fun inp -> ns_dec_digit inp) inp)] inp

(* [88] NS-TAG-DIRECTIVE *)
and ns_tag_directive inp =
    peg_seq [
        (fun inp -> match_str "TAG" inp);
        (fun inp -> s_separate_in_line inp);
        (fun inp -> c_tag_handle inp);
        (fun inp -> s_separate_in_line inp);
        (fun inp -> ns_tag_prefix inp)] inp

(* [89] C-TAG-HANDLE *)
and c_tag_handle inp =
    peg_alt [
        (fun inp -> c_named_tag_handle inp);
        (fun inp -> c_secondary_tag_handle inp);
        (fun inp -> c_primary_tag_handle inp)] inp

(* [90] C-PRIMARY-TAG-HANDLE *)
and c_primary_tag_handle inp =
    match_cp 33 inp

(* [91] C-SECONDARY-TAG-HANDLE *)
and c_secondary_tag_handle inp =
    match_str "!!" inp

(* [92] C-NAMED-TAG-HANDLE *)
and c_named_tag_handle inp =
    peg_seq [
        (fun inp -> match_cp 33 inp);
        (fun inp -> plus_ (fun inp -> ns_word_char inp) inp);
        (fun inp -> match_cp 33 inp)] inp

(* [93] NS-TAG-PREFIX *)
and ns_tag_prefix inp =
    peg_alt [(fun inp -> c_ns_local_tag_prefix inp); (fun inp -> ns_global_tag_prefix inp)] inp

(* [94] C-NS-LOCAL-TAG-PREFIX *)
and c_ns_local_tag_prefix inp =
    peg_seq [(fun inp -> match_cp 33 inp); (fun inp -> star (fun inp -> ns_uri_char inp) inp)] inp

(* [95] NS-GLOBAL-TAG-PREFIX *)
and ns_global_tag_prefix inp =
    peg_seq [(fun inp -> ns_tag_char inp); (fun inp -> star (fun inp -> ns_uri_char inp) inp)] inp

(* [96] C-NS-PROPERTIES *)
and c_ns_properties inp n c =
    peg_alt [
        (fun inp -> peg_seq [
            (fun inp -> c_ns_tag_property inp);
            (fun inp -> opt (fun inp -> peg_seq [(fun inp -> s_separate inp n c); (fun inp -> c_ns_anchor_property inp)] inp) inp)] inp);
        (fun inp -> peg_seq [
            (fun inp -> c_ns_anchor_property inp);
            (fun inp -> opt (fun inp -> peg_seq [(fun inp -> s_separate inp n c); (fun inp -> c_ns_tag_property inp)] inp) inp)] inp)] inp

(* [97] C-NS-TAG-PROPERTY *)
and c_ns_tag_property inp =
    peg_alt [
        (fun inp -> c_verbatim_tag inp);
        (fun inp -> c_ns_shorthand_tag inp);
        (fun inp -> c_non_specific_tag inp)] inp

(* [98] C-VERBATIM-TAG *)
and c_verbatim_tag inp =
    peg_seq [
        (fun inp -> match_str "!<" inp);
        (fun inp -> plus_ (fun inp -> ns_uri_char inp) inp);
        (fun inp -> match_cp 62 inp)] inp

(* [99] C-NS-SHORTHAND-TAG *)
and c_ns_shorthand_tag inp =
    peg_seq [(fun inp -> c_tag_handle inp); (fun inp -> plus_ (fun inp -> ns_tag_char inp) inp)] inp

(* [100] C-NON-SPECIFIC-TAG *)
and c_non_specific_tag inp =
    match_cp 33 inp

(* [101] C-NS-ANCHOR-PROPERTY *)
and c_ns_anchor_property inp =
    build_ast "ANCHOR" (fun inp -> peg_seq [(fun inp -> c_anchor inp); (fun inp -> scalar_fn (fun inp -> ns_anchor_name inp) inp)] inp) inp

(* [102] NS-ANCHOR-CHAR *)
and ns_anchor_char inp =
    minus (fun inp -> ns_char inp) (fun inp -> c_flow_indicator inp) inp

(* [103] NS-ANCHOR-NAME *)
and ns_anchor_name inp =
    plus_ (fun inp -> ns_anchor_char inp) inp

(* [104] C-NS-ALIAS-NODE *)
and c_ns_alias_node inp =
    build_ast "ALIAS" (fun inp -> peg_seq [(fun inp -> c_alias inp); (fun inp -> scalar_fn (fun inp -> ns_anchor_name inp) inp)] inp) inp

(* [105] E-SCALAR *)
and e_scalar inp =
    ok_r inp

(* [106] E-NODE *)
and e_node inp =
    e_scalar inp

(* [107] NB-DOUBLE-CHAR *)
and nb_double_char inp =
    peg_alt [
        (fun inp -> c_ns_esc_char inp);
        (fun inp -> minus (fun inp -> nb_json inp) (fun inp -> peg_alt [(fun inp -> match_cp 92 inp); (fun inp -> match_cp 34 inp)] inp) inp)] inp

(* [108] NS-DOUBLE-CHAR *)
and ns_double_char inp =
    minus (fun inp -> nb_double_char inp) (fun inp -> s_white inp) inp

(* [109] C-DOUBLE-QUOTED *)
and c_double_quoted inp n c =
    scalar_fn (fun inp -> peg_seq [
        (fun inp -> match_cp 34 inp);
        (fun inp -> nb_double_text inp n c);
        (fun inp -> match_cp 34 inp)] inp) inp

(* [110] NB-DOUBLE-TEXT *)
and nb_double_text inp n c =
    (match c with | "FLOW-OUT" -> nb_double_multi_line inp n | "FLOW-IN" -> nb_double_multi_line inp n | "BLOCK-KEY" -> nb_double_one_line inp | "FLOW-KEY" -> nb_double_one_line inp | _ -> fail_r inp "no case")

(* [111] NB-DOUBLE-ONE-LINE *)
and nb_double_one_line inp =
    star (fun inp -> nb_double_char inp) inp

(* [112] S-DOUBLE-ESCAPED *)
and s_double_escaped inp n =
    peg_seq [
        (fun inp -> star (fun inp -> s_white inp) inp);
        (fun inp -> match_cp 92 inp);
        (fun inp -> b_non_content inp);
        (fun inp -> star (fun inp -> l_empty inp n "FLOW-IN") inp);
        (fun inp -> s_flow_line_prefix inp n)] inp

(* [113] S-DOUBLE-BREAK *)
and s_double_break inp n =
    peg_alt [(fun inp -> s_double_escaped inp n); (fun inp -> s_flow_folded inp n)] inp

(* [114] NB-NS-DOUBLE-IN-LINE *)
and nb_ns_double_in_line inp =
    star (fun inp -> peg_seq [(fun inp -> star (fun inp -> s_white inp) inp); (fun inp -> ns_double_char inp)] inp) inp

(* [115] S-DOUBLE-NEXT-LINE *)
and s_double_next_line inp n =
    peg_seq [
        (fun inp -> s_double_break inp n);
        (fun inp -> opt (fun inp -> peg_seq [
            (fun inp -> ns_double_char inp);
            (fun inp -> nb_ns_double_in_line inp);
            (fun inp -> peg_alt [(fun inp -> s_double_next_line inp n); (fun inp -> star (fun inp -> s_white inp) inp)] inp)] inp) inp)] inp

(* [116] NB-DOUBLE-MULTI-LINE *)
and nb_double_multi_line inp n =
    peg_seq [
        (fun inp -> nb_ns_double_in_line inp);
        (fun inp -> peg_alt [(fun inp -> s_double_next_line inp n); (fun inp -> star (fun inp -> s_white inp) inp)] inp)] inp

(* [117] C-QUOTED-QUOTE *)
and c_quoted_quote inp =
    match_str "''" inp

(* [118] NB-SINGLE-CHAR *)
and nb_single_char inp =
    peg_alt [
        (fun inp -> c_quoted_quote inp);
        (fun inp -> minus (fun inp -> nb_json inp) (fun inp -> match_cp 39 inp) inp)] inp

(* [119] NS-SINGLE-CHAR *)
and ns_single_char inp =
    minus (fun inp -> nb_single_char inp) (fun inp -> s_white inp) inp

(* [120] C-SINGLE-QUOTED *)
and c_single_quoted inp n c =
    scalar_fn (fun inp -> peg_seq [
        (fun inp -> match_cp 39 inp);
        (fun inp -> nb_single_text inp n c);
        (fun inp -> match_cp 39 inp)] inp) inp

(* [121] NB-SINGLE-TEXT *)
and nb_single_text inp n c =
    (match c with | "FLOW-OUT" -> nb_single_multi_line inp n | "FLOW-IN" -> nb_single_multi_line inp n | "BLOCK-KEY" -> nb_single_one_line inp | "FLOW-KEY" -> nb_single_one_line inp | _ -> fail_r inp "no case")

(* [122] NB-SINGLE-ONE-LINE *)
and nb_single_one_line inp =
    star (fun inp -> nb_single_char inp) inp

(* [123] NS-SINGLE-IN-LINE *)
and ns_single_in_line inp =
    star (fun inp -> peg_seq [(fun inp -> star (fun inp -> s_white inp) inp); (fun inp -> ns_single_char inp)] inp) inp

(* [124] S-SINGLE-NEXT-LINE *)
and s_single_next_line inp n =
    peg_seq [
        (fun inp -> s_flow_folded inp n);
        (fun inp -> opt (fun inp -> peg_seq [
            (fun inp -> ns_single_char inp);
            (fun inp -> ns_single_in_line inp);
            (fun inp -> peg_alt [(fun inp -> s_single_next_line inp n); (fun inp -> star (fun inp -> s_white inp) inp)] inp)] inp) inp)] inp

(* [125] NB-SINGLE-MULTI-LINE *)
and nb_single_multi_line inp n =
    peg_seq [
        (fun inp -> ns_single_in_line inp);
        (fun inp -> peg_alt [(fun inp -> s_single_next_line inp n); (fun inp -> star (fun inp -> s_white inp) inp)] inp)] inp

(* [126] NS-PLAIN-FIRST *)
and ns_plain_first inp c =
    peg_alt [
        (fun inp -> minus (fun inp -> ns_char inp) (fun inp -> c_indicator inp) inp);
        (fun inp -> peg_seq [
            (fun inp -> peg_alt [
                (fun inp -> match_cp 63 inp);
                (fun inp -> match_cp 58 inp);
                (fun inp -> match_cp 45 inp)] inp);
            (fun inp -> ahead (fun inp -> ns_plain_safe inp c) inp)] inp)] inp

(* [127] NS-PLAIN-SAFE *)
and ns_plain_safe inp c =
    (match c with | "FLOW-OUT" -> ns_plain_safe_out inp | "FLOW-IN" -> ns_plain_safe_in inp | "BLOCK-KEY" -> ns_plain_safe_out inp | "FLOW-KEY" -> ns_plain_safe_in inp | _ -> fail_r inp "no case")

(* [128] NS-PLAIN-SAFE-OUT *)
and ns_plain_safe_out inp =
    ns_char inp

(* [129] NS-PLAIN-SAFE-IN *)
and ns_plain_safe_in inp =
    minus (fun inp -> ns_char inp) (fun inp -> c_flow_indicator inp) inp

(* [130] NS-PLAIN-CHAR *)
and ns_plain_char inp c =
    peg_alt [
        (fun inp -> minus (fun inp -> ns_plain_safe inp c) (fun inp -> peg_alt [(fun inp -> match_cp 58 inp); (fun inp -> match_cp 35 inp)] inp) inp);
        (fun inp -> peg_seq [(fun inp -> behind (fun inp -> ns_char inp) inp); (fun inp -> match_cp 35 inp)] inp);
        (fun inp -> peg_seq [(fun inp -> match_cp 58 inp); (fun inp -> ahead (fun inp -> ns_plain_safe inp c) inp)] inp)] inp

(* [131] NS-PLAIN *)
and ns_plain inp n c =
    scalar_fn (fun inp -> (match c with | "FLOW-OUT" -> ns_plain_multi_line inp n c | "FLOW-IN" -> ns_plain_multi_line inp n c | "BLOCK-KEY" -> ns_plain_one_line inp c | "FLOW-KEY" -> ns_plain_one_line inp c | _ -> fail_r inp "no case")) inp

(* [132] NB-NS-PLAIN-IN-LINE *)
and nb_ns_plain_in_line inp c =
    star (fun inp -> peg_seq [(fun inp -> star (fun inp -> s_white inp) inp); (fun inp -> ns_plain_char inp c)] inp) inp

(* [133] NS-PLAIN-ONE-LINE *)
and ns_plain_one_line inp c =
    peg_seq [(fun inp -> ns_plain_first inp c); (fun inp -> nb_ns_plain_in_line inp c)] inp

(* [134] S-NS-PLAIN-NEXT-LINE *)
and s_ns_plain_next_line inp n c =
    peg_seq [
        (fun inp -> s_flow_folded inp n);
        (fun inp -> neg (fun inp -> c_forbidden inp) inp);
        (fun inp -> ns_plain_char inp c);
        (fun inp -> nb_ns_plain_in_line inp c)] inp

(* [135] NS-PLAIN-MULTI-LINE *)
and ns_plain_multi_line inp n c =
    peg_seq [
        (fun inp -> ns_plain_one_line inp c);
        (fun inp -> star (fun inp -> s_ns_plain_next_line inp n c) inp)] inp

(* [137] C-FLOW-SEQUENCE *)
and c_flow_sequence inp n c =
    build_ast "SEQUENCE" (fun inp -> peg_seq [
        (fun inp -> match_cp 91 inp);
        (fun inp -> opt (fun inp -> s_separate inp n c) inp);
        (fun inp -> opt (fun inp -> collect_fn (fun inp -> ns_s_flow_seq_entries inp n (in_flow c)) inp) inp);
        (fun inp -> match_cp 93 inp)] inp) inp

(* [138] NS-S-FLOW-SEQ-ENTRIES *)
and ns_s_flow_seq_entries inp n c =
    peg_seq [
        (fun inp -> ns_flow_seq_entry inp n c);
        (fun inp -> opt (fun inp -> s_separate inp n c) inp);
        (fun inp -> opt (fun inp -> peg_seq [
            (fun inp -> match_cp 44 inp);
            (fun inp -> opt (fun inp -> s_separate inp n c) inp);
            (fun inp -> opt (fun inp -> ns_s_flow_seq_entries inp n c) inp)] inp) inp)] inp

(* [139] NS-FLOW-SEQ-ENTRY *)
and ns_flow_seq_entry inp n c =
    peg_alt [(fun inp -> ns_flow_pair inp n c); (fun inp -> ns_flow_node inp n c)] inp

(* [140] C-FLOW-MAPPING *)
and c_flow_mapping inp n c =
    build_ast "MAPPING" (fun inp -> peg_seq [
        (fun inp -> match_cp 123 inp);
        (fun inp -> opt (fun inp -> s_separate inp n c) inp);
        (fun inp -> opt (fun inp -> collect_fn (fun inp -> ns_s_flow_map_entries inp n (in_flow c)) inp) inp);
        (fun inp -> match_cp 125 inp)] inp) inp

(* [141] NS-S-FLOW-MAP-ENTRIES *)
and ns_s_flow_map_entries inp n c =
    peg_seq [
        (fun inp -> ns_flow_map_entry inp n c);
        (fun inp -> opt (fun inp -> s_separate inp n c) inp);
        (fun inp -> opt (fun inp -> peg_seq [
            (fun inp -> match_cp 44 inp);
            (fun inp -> opt (fun inp -> s_separate inp n c) inp);
            (fun inp -> opt (fun inp -> ns_s_flow_map_entries inp n c) inp)] inp) inp)] inp

(* [142] NS-FLOW-MAP-ENTRY *)
and ns_flow_map_entry inp n c =
    peg_alt [
        (fun inp -> peg_seq [
            (fun inp -> match_cp 63 inp);
            (fun inp -> s_separate inp n c);
            (fun inp -> ns_flow_map_explicit_entry inp n c)] inp);
        (fun inp -> ns_flow_map_implicit_entry inp n c)] inp

(* [143] NS-FLOW-MAP-EXPLICIT-ENTRY *)
and ns_flow_map_explicit_entry inp n c =
    peg_alt [
        (fun inp -> ns_flow_map_implicit_entry inp n c);
        (fun inp -> peg_seq [(fun inp -> e_node inp); (fun inp -> e_node inp)] inp)] inp

(* [144] NS-FLOW-MAP-IMPLICIT-ENTRY *)
and ns_flow_map_implicit_entry inp n c =
    build_ast "PAIR" (fun inp -> peg_alt [
        (fun inp -> ns_flow_map_yaml_key_entry inp n c);
        (fun inp -> c_ns_flow_map_empty_key_entry inp n c);
        (fun inp -> c_ns_flow_map_json_key_entry inp n c)] inp) inp

(* [145] NS-FLOW-MAP-YAML-KEY-ENTRY *)
and ns_flow_map_yaml_key_entry inp n c =
    peg_seq [
        (fun inp -> ns_flow_yaml_node inp n c);
        (fun inp -> peg_alt [
            (fun inp -> peg_seq [
                (fun inp -> opt (fun inp -> s_separate inp n c) inp);
                (fun inp -> c_ns_flow_map_separate_value inp n c)] inp);
            (fun inp -> e_node inp)] inp)] inp

(* [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY *)
and c_ns_flow_map_empty_key_entry inp n c =
    peg_seq [(fun inp -> e_node inp); (fun inp -> c_ns_flow_map_separate_value inp n c)] inp

(* [147] C-NS-FLOW-MAP-SEPARATE-VALUE *)
and c_ns_flow_map_separate_value inp n c =
    peg_seq [
        (fun inp -> match_cp 58 inp);
        (fun inp -> neg (fun inp -> ns_plain_safe inp c) inp);
        (fun inp -> peg_alt [
            (fun inp -> peg_seq [(fun inp -> s_separate inp n c); (fun inp -> ns_flow_node inp n c)] inp);
            (fun inp -> e_node inp)] inp)] inp

(* [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY *)
and c_ns_flow_map_json_key_entry inp n c =
    peg_seq [
        (fun inp -> c_flow_json_node inp n c);
        (fun inp -> peg_alt [
            (fun inp -> peg_seq [
                (fun inp -> opt (fun inp -> s_separate inp n c) inp);
                (fun inp -> c_ns_flow_map_adjacent_value inp n c)] inp);
            (fun inp -> e_node inp)] inp)] inp

(* [149] C-NS-FLOW-MAP-ADJACENT-VALUE *)
and c_ns_flow_map_adjacent_value inp n c =
    peg_seq [
        (fun inp -> match_cp 58 inp);
        (fun inp -> peg_alt [
            (fun inp -> peg_seq [
                (fun inp -> opt (fun inp -> s_separate inp n c) inp);
                (fun inp -> ns_flow_node inp n c)] inp);
            (fun inp -> e_node inp)] inp)] inp

(* [150] NS-FLOW-PAIR *)
and ns_flow_pair inp n c =
    peg_alt [
        (fun inp -> peg_seq [
            (fun inp -> match_cp 63 inp);
            (fun inp -> s_separate inp n c);
            (fun inp -> ns_flow_map_explicit_entry inp n c)] inp);
        (fun inp -> ns_flow_pair_entry inp n c)] inp

(* [151] NS-FLOW-PAIR-ENTRY *)
and ns_flow_pair_entry inp n c =
    peg_alt [
        (fun inp -> ns_flow_pair_yaml_key_entry inp n c);
        (fun inp -> c_ns_flow_map_empty_key_entry inp n c);
        (fun inp -> c_ns_flow_pair_json_key_entry inp n c)] inp

(* [152] NS-FLOW-PAIR-YAML-KEY-ENTRY *)
and ns_flow_pair_yaml_key_entry inp n c =
    peg_seq [
        (fun inp -> ns_s_implicit_yaml_key inp "FLOW-KEY");
        (fun inp -> c_ns_flow_map_separate_value inp n c)] inp

(* [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY *)
and c_ns_flow_pair_json_key_entry inp n c =
    peg_seq [
        (fun inp -> c_s_implicit_json_key inp "FLOW-KEY");
        (fun inp -> c_ns_flow_map_adjacent_value inp n c)] inp

(* [154] NS-S-IMPLICIT-YAML-KEY *)
and ns_s_implicit_yaml_key inp c =
    peg_seq [
        (fun inp -> ns_flow_yaml_node inp 0 c);
        (fun inp -> opt (fun inp -> s_separate_in_line inp) inp)] inp

(* [155] C-S-IMPLICIT-JSON-KEY *)
and c_s_implicit_json_key inp c =
    peg_seq [
        (fun inp -> c_flow_json_node inp 0 c);
        (fun inp -> opt (fun inp -> s_separate_in_line inp) inp)] inp

(* [156] NS-FLOW-YAML-CONTENT *)
and ns_flow_yaml_content inp n c =
    ns_plain inp n c

(* [157] C-FLOW-JSON-CONTENT *)
and c_flow_json_content inp n c =
    peg_alt [
        (fun inp -> c_flow_sequence inp n c);
        (fun inp -> c_flow_mapping inp n c);
        (fun inp -> c_single_quoted inp n c);
        (fun inp -> c_double_quoted inp n c)] inp

(* [158] NS-FLOW-CONTENT *)
and ns_flow_content inp n c =
    peg_alt [(fun inp -> ns_flow_yaml_content inp n c); (fun inp -> c_flow_json_content inp n c)] inp

(* [159] NS-FLOW-YAML-NODE *)
and ns_flow_yaml_node inp n c =
    peg_alt [
        (fun inp -> c_ns_alias_node inp);
        (fun inp -> ns_flow_yaml_content inp n c);
        (fun inp -> peg_seq [
            (fun inp -> c_ns_properties inp n c);
            (fun inp -> peg_alt [
                (fun inp -> peg_seq [(fun inp -> s_separate inp n c); (fun inp -> ns_flow_yaml_content inp n c)] inp);
                (fun inp -> e_scalar inp)] inp)] inp)] inp

(* [160] C-FLOW-JSON-NODE *)
and c_flow_json_node inp n c =
    peg_seq [
        (fun inp -> opt (fun inp -> peg_seq [(fun inp -> c_ns_properties inp n c); (fun inp -> s_separate inp n c)] inp) inp);
        (fun inp -> c_flow_json_content inp n c)] inp

(* [161] NS-FLOW-NODE *)
and ns_flow_node inp n c =
    peg_alt [
        (fun inp -> c_ns_alias_node inp);
        (fun inp -> ns_flow_content inp n c);
        (fun inp -> peg_seq [
            (fun inp -> c_ns_properties inp n c);
            (fun inp -> peg_alt [
                (fun inp -> peg_seq [(fun inp -> s_separate inp n c); (fun inp -> ns_flow_content inp n c)] inp);
                (fun inp -> e_scalar inp)] inp)] inp)] inp

(* [162] C-B-BLOCK-HEADER *)
and c_b_block_header inp n =
    peg_alt [
        (fun inp -> let r_ = peg_alt [
            (fun inp -> parse_int_fn (fun inp -> ns_dec_digit inp) inp);
            (fun inp -> detect_indent n inp)] inp in if r_.failed then r_ else let m = r_.tag_int in let inp = r_.rest in let r_ = peg_alt [
            (fun inp -> parse_sym_fn (fun inp -> match_cp 45 inp) "STRIP" inp);
            (fun inp -> parse_sym_fn (fun inp -> match_cp 43 inp) "KEEP" inp);
            (fun inp -> val_fn "CLIP" inp)] inp in if r_.failed then r_ else let t = r_.tag in let inp = r_.rest in s_b_comment inp);
        (fun inp -> let r_ = peg_alt [
            (fun inp -> parse_sym_fn (fun inp -> match_cp 45 inp) "STRIP" inp);
            (fun inp -> parse_sym_fn (fun inp -> match_cp 43 inp) "KEEP" inp);
            (fun inp -> val_fn "CLIP" inp)] inp in if r_.failed then r_ else let t = r_.tag in let inp = r_.rest in let r_ = peg_alt [
            (fun inp -> parse_int_fn (fun inp -> ns_dec_digit inp) inp);
            (fun inp -> detect_indent n inp)] inp in if r_.failed then r_ else let m = r_.tag_int in let inp = r_.rest in s_b_comment inp)] inp

(* [163] C-INDENTATION-INDICATOR *)
and c_indentation_indicator inp n =
    peg_alt [(fun inp -> ns_dec_digit inp); (fun inp -> ok_r inp)] inp

(* [164] C-CHOMPING-INDICATOR *)
and c_chomping_indicator inp =
    peg_alt [(fun inp -> match_cp 45 inp); (fun inp -> match_cp 43 inp); (fun inp -> ok_r inp)] inp

(* [165] B-CHOMPED-LAST *)
and b_chomped_last inp t =
    (match t with | "STRIP" -> b_non_content inp | "CLIP" -> b_as_line_feed inp | "KEEP" -> b_as_line_feed inp | _ -> fail_r inp "no case")

(* [166] L-CHOMPED-EMPTY *)
and l_chomped_empty inp n t =
    (match t with | "STRIP" -> l_strip_empty inp n | "CLIP" -> l_strip_empty inp n | "KEEP" -> l_keep_empty inp n | _ -> fail_r inp "no case")

(* [167] L-STRIP-EMPTY *)
and l_strip_empty inp n =
    peg_seq [
        (fun inp -> star (fun inp -> peg_seq [(fun inp -> s_indent_le inp n); (fun inp -> b_non_content inp)] inp) inp);
        (fun inp -> opt (fun inp -> l_trail_comments inp n) inp)] inp

(* [168] L-KEEP-EMPTY *)
and l_keep_empty inp n =
    peg_seq [
        (fun inp -> star (fun inp -> l_empty inp n "BLOCK-IN") inp);
        (fun inp -> opt (fun inp -> l_trail_comments inp n) inp)] inp

(* [169] L-TRAIL-COMMENTS *)
and l_trail_comments inp n =
    peg_seq [
        (fun inp -> s_indent_lt inp n);
        (fun inp -> c_nb_comment_text inp);
        (fun inp -> b_comment inp);
        (fun inp -> star (fun inp -> l_comment inp) inp)] inp

(* [170] C-L+LITERAL *)
and c_lliteral inp n =
    peg_seq [
        (fun inp -> match_cp 124 inp);
        (fun inp -> let r_ = peg_alt [
            (fun inp -> parse_int_fn (fun inp -> ns_dec_digit inp) inp);
            (fun inp -> detect_indent n inp)] inp in if r_.failed then r_ else let m = r_.tag_int in let inp = r_.rest in let r_ = peg_alt [
            (fun inp -> parse_sym_fn (fun inp -> match_cp 45 inp) "STRIP" inp);
            (fun inp -> parse_sym_fn (fun inp -> match_cp 43 inp) "KEEP" inp);
            (fun inp -> val_fn "CLIP" inp)] inp in if r_.failed then r_ else let t = r_.tag in let inp = r_.rest in peg_seq [(fun inp -> s_b_comment inp); (fun inp -> l_literal_content inp (n + m) t)] inp)] inp

(* [171] L-NB-LITERAL-TEXT *)
and l_nb_literal_text inp n =
    peg_seq [
        (fun inp -> star (fun inp -> l_empty inp n "BLOCK-IN") inp);
        (fun inp -> s_indent inp n);
        (fun inp -> plus_ (fun inp -> nb_char inp) inp)] inp

(* [172] B-NB-LITERAL-NEXT *)
and b_nb_literal_next inp n =
    peg_seq [(fun inp -> b_as_line_feed inp); (fun inp -> l_nb_literal_text inp n)] inp

(* [173] L-LITERAL-CONTENT *)
and l_literal_content inp n t =
    scalar_fn (fun inp -> peg_seq [
        (fun inp -> opt (fun inp -> peg_seq [
            (fun inp -> l_nb_literal_text inp n);
            (fun inp -> star (fun inp -> b_nb_literal_next inp n) inp);
            (fun inp -> b_chomped_last inp t)] inp) inp);
        (fun inp -> l_chomped_empty inp n t)] inp) inp

(* [174] C-L+FOLDED *)
and c_lfolded inp n =
    peg_seq [
        (fun inp -> match_cp 62 inp);
        (fun inp -> let r_ = peg_alt [
            (fun inp -> parse_int_fn (fun inp -> ns_dec_digit inp) inp);
            (fun inp -> detect_indent n inp)] inp in if r_.failed then r_ else let m = r_.tag_int in let inp = r_.rest in let r_ = peg_alt [
            (fun inp -> parse_sym_fn (fun inp -> match_cp 45 inp) "STRIP" inp);
            (fun inp -> parse_sym_fn (fun inp -> match_cp 43 inp) "KEEP" inp);
            (fun inp -> val_fn "CLIP" inp)] inp in if r_.failed then r_ else let t = r_.tag in let inp = r_.rest in peg_seq [(fun inp -> s_b_comment inp); (fun inp -> l_folded_content inp (n + m) t)] inp)] inp

(* [175] S-NB-FOLDED-TEXT *)
and s_nb_folded_text inp n =
    peg_seq [
        (fun inp -> s_indent inp n);
        (fun inp -> ns_char inp);
        (fun inp -> star (fun inp -> nb_char inp) inp)] inp

(* [176] L-NB-FOLDED-LINES *)
and l_nb_folded_lines inp n =
    peg_seq [
        (fun inp -> s_nb_folded_text inp n);
        (fun inp -> star (fun inp -> peg_seq [(fun inp -> b_l_folded inp n "BLOCK-IN"); (fun inp -> s_nb_folded_text inp n)] inp) inp)] inp

(* [177] S-NB-SPACED-TEXT *)
and s_nb_spaced_text inp n =
    peg_seq [
        (fun inp -> s_indent inp n);
        (fun inp -> s_white inp);
        (fun inp -> star (fun inp -> nb_char inp) inp)] inp

(* [178] B-L-SPACED *)
and b_l_spaced inp n =
    peg_seq [
        (fun inp -> b_as_line_feed inp);
        (fun inp -> star (fun inp -> l_empty inp n "BLOCK-IN") inp)] inp

(* [179] L-NB-SPACED-LINES *)
and l_nb_spaced_lines inp n =
    peg_seq [
        (fun inp -> s_nb_spaced_text inp n);
        (fun inp -> star (fun inp -> peg_seq [(fun inp -> b_l_spaced inp n); (fun inp -> s_nb_spaced_text inp n)] inp) inp)] inp

(* [180] L-NB-SAME-LINES *)
and l_nb_same_lines inp n =
    peg_seq [
        (fun inp -> star (fun inp -> l_empty inp n "BLOCK-IN") inp);
        (fun inp -> peg_alt [(fun inp -> l_nb_folded_lines inp n); (fun inp -> l_nb_spaced_lines inp n)] inp)] inp

(* [181] L-NB-DIFF-LINES *)
and l_nb_diff_lines inp n =
    peg_seq [
        (fun inp -> l_nb_same_lines inp n);
        (fun inp -> star (fun inp -> peg_seq [(fun inp -> b_as_line_feed inp); (fun inp -> l_nb_same_lines inp n)] inp) inp)] inp

(* [182] L-FOLDED-CONTENT *)
and l_folded_content inp n t =
    scalar_fn (fun inp -> peg_seq [
        (fun inp -> opt (fun inp -> peg_seq [(fun inp -> l_nb_diff_lines inp n); (fun inp -> b_chomped_last inp t)] inp) inp);
        (fun inp -> l_chomped_empty inp n t)] inp) inp

(* [183] L+BLOCK-SEQUENCE *)
and lblock_sequence inp n =
    build_ast "SEQUENCE" (fun inp -> let r_ = detect_indent n inp in if r_.failed then r_ else let m = r_.tag_int in let inp = r_.rest in collect_fn (fun inp -> plus_ (fun inp -> peg_seq [(fun inp -> s_indent inp (n + m)); (fun inp -> c_l_block_seq_entry inp (n + m))] inp) inp) inp) inp

(* [184] C-L-BLOCK-SEQ-ENTRY *)
and c_l_block_seq_entry inp n =
    peg_seq [
        (fun inp -> match_cp 45 inp);
        (fun inp -> neg (fun inp -> ns_char inp) inp);
        (fun inp -> s_lblock_indented inp n "BLOCK-IN")] inp

(* [185] S-L+BLOCK-INDENTED *)
and s_lblock_indented inp n c =
    peg_alt [
        (fun inp -> let r_ = detect_indent 0 inp in if r_.failed then r_ else let m = r_.tag_int in let inp = r_.rest in peg_seq [
            (fun inp -> s_indent inp m);
            (fun inp -> peg_alt [
                (fun inp -> ns_l_compact_sequence inp (n + 1 + m));
                (fun inp -> ns_l_compact_mapping inp (n + 1 + m))] inp)] inp);
        (fun inp -> s_lblock_node inp n c);
        (fun inp -> peg_seq [(fun inp -> e_node inp); (fun inp -> s_l_comments inp)] inp)] inp

(* [186] NS-L-COMPACT-SEQUENCE *)
and ns_l_compact_sequence inp n =
    peg_seq [
        (fun inp -> c_l_block_seq_entry inp n);
        (fun inp -> star (fun inp -> peg_seq [(fun inp -> s_indent inp n); (fun inp -> c_l_block_seq_entry inp n)] inp) inp)] inp

(* [187] L+BLOCK-MAPPING *)
and lblock_mapping inp n =
    build_ast "MAPPING" (fun inp -> let r_ = detect_indent n inp in if r_.failed then r_ else let m = r_.tag_int in let inp = r_.rest in collect_fn (fun inp -> plus_ (fun inp -> peg_seq [(fun inp -> s_indent inp (n + m)); (fun inp -> ns_l_block_map_entry inp (n + m))] inp) inp) inp) inp

(* [188] NS-L-BLOCK-MAP-ENTRY *)
and ns_l_block_map_entry inp n =
    peg_alt [
        (fun inp -> c_l_block_map_explicit_entry inp n);
        (fun inp -> ns_l_block_map_implicit_entry inp n)] inp

(* [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY *)
and c_l_block_map_explicit_entry inp n =
    peg_seq [
        (fun inp -> c_l_block_map_explicit_key inp n);
        (fun inp -> peg_alt [(fun inp -> l_block_map_explicit_value inp n); (fun inp -> e_node inp)] inp)] inp

(* [190] C-L-BLOCK-MAP-EXPLICIT-KEY *)
and c_l_block_map_explicit_key inp n =
    peg_seq [(fun inp -> match_cp 63 inp); (fun inp -> s_lblock_indented inp n "BLOCK-OUT")] inp

(* [191] L-BLOCK-MAP-EXPLICIT-VALUE *)
and l_block_map_explicit_value inp n =
    peg_seq [
        (fun inp -> s_indent inp n);
        (fun inp -> match_cp 58 inp);
        (fun inp -> s_lblock_indented inp n "BLOCK-OUT")] inp

(* [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY *)
and ns_l_block_map_implicit_entry inp n =
    build_ast "PAIR" (fun inp -> peg_seq [
        (fun inp -> scalar_fn (fun inp -> peg_alt [(fun inp -> ns_s_block_map_implicit_key inp); (fun inp -> e_node inp)] inp) inp);
        (fun inp -> c_l_block_map_implicit_value inp n)] inp) inp

(* [193] NS-S-BLOCK-MAP-IMPLICIT-KEY *)
and ns_s_block_map_implicit_key inp =
    peg_alt [
        (fun inp -> c_s_implicit_json_key inp "BLOCK-KEY");
        (fun inp -> ns_s_implicit_yaml_key inp "BLOCK-KEY")] inp

(* [194] C-L-BLOCK-MAP-IMPLICIT-VALUE *)
and c_l_block_map_implicit_value inp n =
    peg_seq [
        (fun inp -> match_cp 58 inp);
        (fun inp -> peg_alt [
            (fun inp -> s_lblock_node inp n "BLOCK-OUT");
            (fun inp -> scalar_fn (fun inp -> peg_seq [(fun inp -> e_node inp); (fun inp -> s_l_comments inp)] inp) inp)] inp)] inp

(* [195] NS-L-COMPACT-MAPPING *)
and ns_l_compact_mapping inp n =
    peg_seq [
        (fun inp -> ns_l_block_map_entry inp n);
        (fun inp -> star (fun inp -> peg_seq [(fun inp -> s_indent inp n); (fun inp -> ns_l_block_map_entry inp n)] inp) inp)] inp

(* [196] S-L+BLOCK-NODE *)
and s_lblock_node inp n c =
    peg_alt [(fun inp -> s_lblock_in_block inp n c); (fun inp -> s_lflow_in_block inp n)] inp

(* [197] S-L+FLOW-IN-BLOCK *)
and s_lflow_in_block inp n =
    peg_seq [
        (fun inp -> s_separate inp (n + 1) "FLOW-OUT");
        (fun inp -> ns_flow_node inp (n + 1) "FLOW-OUT");
        (fun inp -> s_l_comments inp)] inp

(* [198] S-L+BLOCK-IN-BLOCK *)
and s_lblock_in_block inp n c =
    peg_alt [(fun inp -> s_lblock_scalar inp n c); (fun inp -> s_lblock_collection inp n c)] inp

(* [199] S-L+BLOCK-SCALAR *)
and s_lblock_scalar inp n c =
    peg_seq [
        (fun inp -> s_separate inp (n + 1) c);
        (fun inp -> opt (fun inp -> peg_seq [(fun inp -> c_ns_properties inp (n + 1) c); (fun inp -> s_separate inp (n + 1) c)] inp) inp);
        (fun inp -> peg_alt [(fun inp -> c_lliteral inp n); (fun inp -> c_lfolded inp n)] inp)] inp

(* [200] S-L+BLOCK-COLLECTION *)
and s_lblock_collection inp n c =
    peg_seq [
        (fun inp -> opt (fun inp -> peg_seq [(fun inp -> s_separate inp (n + 1) c); (fun inp -> c_ns_properties inp (n + 1) c)] inp) inp);
        (fun inp -> s_l_comments inp);
        (fun inp -> peg_alt [(fun inp -> lblock_sequence inp (seq_spaces n c)); (fun inp -> lblock_mapping inp n)] inp)] inp

(* [202] L-DOCUMENT-PREFIX *)
and l_document_prefix inp =
    peg_seq [
        (fun inp -> opt (fun inp -> c_byte_order_mark inp) inp);
        (fun inp -> star (fun inp -> l_comment inp) inp)] inp

(* [203] C-DIRECTIVES-END *)
and c_directives_end inp =
    match_str "---" inp

(* [204] C-DOCUMENT-END *)
and c_document_end inp =
    match_str "..." inp

(* [205] L-DOCUMENT-SUFFIX *)
and l_document_suffix inp =
    peg_seq [(fun inp -> c_document_end inp); (fun inp -> s_l_comments inp)] inp

(* [206] C-FORBIDDEN *)
and c_forbidden inp =
    peg_seq [
        (fun inp -> sol inp);
        (fun inp -> peg_alt [(fun inp -> c_directives_end inp); (fun inp -> c_document_end inp)] inp);
        (fun inp -> peg_alt [(fun inp -> b_char inp); (fun inp -> s_white inp); (fun inp -> eof_ok inp)] inp)] inp

(* [207] L-BARE-DOCUMENT *)
and l_bare_document inp =
    build_ast "DOC" (fun inp -> s_lblock_node inp (-1) "BLOCK-IN") inp

(* [208] L-EXPLICIT-DOCUMENT *)
and l_explicit_document inp =
    build_ast "DOC" (fun inp -> peg_seq [
        (fun inp -> c_directives_end inp);
        (fun inp -> peg_alt [
            (fun inp -> l_bare_document inp);
            (fun inp -> peg_seq [(fun inp -> e_node inp); (fun inp -> s_l_comments inp)] inp)] inp)] inp) inp

(* [209] L-DIRECTIVE-DOCUMENT *)
and l_directive_document inp =
    peg_seq [
        (fun inp -> plus_ (fun inp -> l_directive inp) inp);
        (fun inp -> l_explicit_document inp)] inp

(* [210] L-ANY-DOCUMENT *)
and l_any_document inp =
    peg_alt [
        (fun inp -> l_directive_document inp);
        (fun inp -> l_explicit_document inp);
        (fun inp -> l_bare_document inp)] inp

(* [211] L-YAML-STREAM *)
and l_yaml_stream inp =
    build_ast "STREAM" (fun inp -> peg_seq [
        (fun inp -> star (fun inp -> l_document_prefix inp) inp);
        (fun inp -> opt (fun inp -> l_any_document inp) inp);
        (fun inp -> star (fun inp -> peg_alt [
            (fun inp -> peg_seq [
                (fun inp -> plus_ (fun inp -> l_document_suffix inp) inp);
                (fun inp -> star (fun inp -> l_document_prefix inp) inp);
                (fun inp -> opt (fun inp -> l_any_document inp) inp)] inp);
            (fun inp -> peg_seq [
                (fun inp -> star (fun inp -> l_document_prefix inp) inp);
                (fun inp -> opt (fun inp -> l_explicit_document inp) inp)] inp)] inp) inp)] inp) inp

(* ── API ── *)

and print_ast node depth =
  let indent = String.make (depth * 2) ' ' in
  match node with
  | Leaf t -> Printf.printf "%sSCALAR: \"%s\"\n" indent t
  | Branch (t, cs) ->
    Printf.printf "%s%s\n" indent t;
    List.iter (fun c -> print_ast c (depth + 1)) cs

(* ── Main ── *)

let () =
  let text =
    if Array.length Sys.argv > 1 then
      let ic = open_in Sys.argv.(1) in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      Bytes.to_string s
    else if Unix.isatty Unix.stdin then begin
      Printf.eprintf "Usage: %s [file]\n" Sys.argv.(0);
      Printf.eprintf "  Reads YAML from file or stdin.\n";
      Printf.eprintf "  If no file given and stdin is a terminal, shows this help.\n";
      exit 1
    end else begin
      let buf = Buffer.create 4096 in
      (try while true do Buffer.add_char buf (input_char stdin) done with End_of_file -> ());
      Buffer.contents buf
    end
  in
  let inp = mk_input text in
  let r = l_yaml_stream inp in
  if not r.failed then begin
    Printf.printf "OK: %d chars\n" r.rest.pos;
    (match r.ast with Some a -> print_ast a 0 | None -> ())
  end else
    Printf.eprintf "FAIL @%d: %s\n" r.rest.pos r.err
