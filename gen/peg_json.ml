(* ════════════════════════════════════════════════════════════════ *)
(* peg_json.ml — JSON (RFC 8259) parser                                   *)
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

(* ════════════════════════════════════════════════════════════════ *)
(* YAML 1.2 Grammar — 211 rules *)
(* ════════════════════════════════════════════════════════════════ *)

(* [1] JSON-TEXT *)
and json_text inp =
    peg_seq [
        (fun inp -> ws inp);
        (fun inp -> value inp);
        (fun inp -> ws inp);
        (fun inp -> eof_ok inp)] inp

(* [2] VALUE *)
and value inp =
    peg_alt [
        (fun inp -> r_object inp);
        (fun inp -> array inp);
        (fun inp -> string inp);
        (fun inp -> number inp);
        (fun inp -> match_str "true" inp);
        (fun inp -> match_str "false" inp);
        (fun inp -> match_str "null" inp)] inp

(* [3] OBJECT *)
and r_object inp =
    peg_alt [
        (fun inp -> peg_seq [
            (fun inp -> match_cp 123 inp);
            (fun inp -> ws inp);
            (fun inp -> members inp);
            (fun inp -> ws inp);
            (fun inp -> match_cp 125 inp)] inp);
        (fun inp -> peg_seq [(fun inp -> match_cp 123 inp); (fun inp -> ws inp); (fun inp -> match_cp 125 inp)] inp)] inp

(* [4] MEMBERS *)
and members inp =
    peg_seq [
        (fun inp -> member inp);
        (fun inp -> star (fun inp -> peg_seq [
            (fun inp -> ws inp);
            (fun inp -> match_cp 44 inp);
            (fun inp -> ws inp);
            (fun inp -> member inp)] inp) inp)] inp

(* [5] MEMBER *)
and member inp =
    peg_seq [
        (fun inp -> ws inp);
        (fun inp -> string inp);
        (fun inp -> ws inp);
        (fun inp -> match_cp 58 inp);
        (fun inp -> ws inp);
        (fun inp -> value inp);
        (fun inp -> ws inp)] inp

(* [6] ARRAY *)
and array inp =
    peg_alt [
        (fun inp -> peg_seq [
            (fun inp -> match_cp 91 inp);
            (fun inp -> ws inp);
            (fun inp -> elements inp);
            (fun inp -> ws inp);
            (fun inp -> match_cp 93 inp)] inp);
        (fun inp -> peg_seq [(fun inp -> match_cp 91 inp); (fun inp -> ws inp); (fun inp -> match_cp 93 inp)] inp)] inp

(* [7] ELEMENTS *)
and elements inp =
    peg_seq [
        (fun inp -> value inp);
        (fun inp -> star (fun inp -> peg_seq [
            (fun inp -> ws inp);
            (fun inp -> match_cp 44 inp);
            (fun inp -> ws inp);
            (fun inp -> value inp)] inp) inp)] inp

(* [8] STRING *)
and string inp =
    peg_seq [
        (fun inp -> match_cp 34 inp);
        (fun inp -> star (fun inp -> char inp) inp);
        (fun inp -> match_cp 34 inp)] inp

(* [9] CHAR *)
and char inp =
    peg_alt [
        (fun inp -> escaped inp);
        (fun inp -> peg_seq [
            (fun inp -> neg (fun inp -> match_cp 34 inp) inp);
            (fun inp -> neg (fun inp -> match_cp 92 inp) inp);
            (fun inp -> neg (fun inp -> match_cp 0x0 inp) inp);
            (fun inp -> neg (fun inp -> match_range 0x0 0x1F inp) inp);
            (fun inp -> match_range 0x20 0x10FFFF inp)] inp)] inp

(* [10] ESCAPED *)
and escaped inp =
    peg_seq [
        (fun inp -> match_cp 92 inp);
        (fun inp -> peg_alt [
            (fun inp -> match_cp 34 inp);
            (fun inp -> match_cp 92 inp);
            (fun inp -> match_cp 47 inp);
            (fun inp -> match_cp 98 inp);
            (fun inp -> match_cp 102 inp);
            (fun inp -> match_cp 110 inp);
            (fun inp -> match_cp 114 inp);
            (fun inp -> match_cp 116 inp);
            (fun inp -> peg_seq [(fun inp -> match_cp 117 inp); (fun inp -> hex4 inp)] inp)] inp)] inp

(* [11] HEX4 *)
and hex4 inp =
    peg_seq [
        (fun inp -> hexdig inp);
        (fun inp -> hexdig inp);
        (fun inp -> hexdig inp);
        (fun inp -> hexdig inp)] inp

(* [12] HEXDIG *)
and hexdig inp =
    peg_alt [
        (fun inp -> match_range 48 57 inp);
        (fun inp -> match_range 97 102 inp);
        (fun inp -> match_range 65 70 inp)] inp

(* [13] NUMBER *)
and number inp =
    peg_seq [
        (fun inp -> opt (fun inp -> match_cp 45 inp) inp);
        (fun inp -> integer inp);
        (fun inp -> opt (fun inp -> fraction inp) inp);
        (fun inp -> opt (fun inp -> exponent inp) inp)] inp

(* [14] INTEGER *)
and integer inp =
    peg_alt [
        (fun inp -> match_cp 48 inp);
        (fun inp -> peg_seq [
            (fun inp -> match_range 49 57 inp);
            (fun inp -> star (fun inp -> match_range 48 57 inp) inp)] inp)] inp

(* [15] FRACTION *)
and fraction inp =
    peg_seq [
        (fun inp -> match_cp 46 inp);
        (fun inp -> plus_ (fun inp -> match_range 48 57 inp) inp)] inp

(* [16] EXPONENT *)
and exponent inp =
    peg_seq [
        (fun inp -> peg_alt [(fun inp -> match_cp 101 inp); (fun inp -> match_cp 69 inp)] inp);
        (fun inp -> opt (fun inp -> peg_alt [(fun inp -> match_cp 43 inp); (fun inp -> match_cp 45 inp)] inp) inp);
        (fun inp -> plus_ (fun inp -> match_range 48 57 inp) inp)] inp

(* [17] WS *)
and ws inp =
    star (fun inp -> peg_alt [
        (fun inp -> match_cp 0x20 inp);
        (fun inp -> match_cp 0x9 inp);
        (fun inp -> match_cp 0x0A inp);
        (fun inp -> match_cp 0x0D inp)] inp) inp

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
      Printf.eprintf "  Reads JSON from file or stdin.\n";
      Printf.eprintf "  If no file given and stdin is a terminal, shows this help.\n";
      exit 1
    end else begin
      let buf = Buffer.create 4096 in
      (try while true do Buffer.add_char buf (input_char stdin) done with End_of_file -> ());
      Buffer.contents buf
    end
  in
  let inp = mk_input text in
  let r = json_text inp in
  if not r.failed then begin
    Printf.printf "OK: %d chars\n" r.rest.pos;
    (match r.ast with Some a -> print_ast a 0 | None -> ())
   end else begin
    Printf.eprintf "FAIL @%d: %s\n" r.rest.pos r.err;
    exit 1
  end
