;;;; peg-ocaml.lisp — OCaml target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "OCaml")
(def-tgt "default-output" "peg_yaml.ml")
(def-tgt "comment-prefix" "(*")
(def-tgt "comment-suffix" "*)")
(def-tgt "call-style" "haskell")
(def-tgt "list-sep" ";")

(def-tgt "keywords"
  '("and" "as" "assert" "asr" "begin" "class" "constraint" "do" "done"
    "downto" "else" "end" "exception" "external" "false" "for" "fun"
    "function" "functor" "if" "in" "include" "inherit" "initializer"
    "land" "lazy" "let" "lor" "lsl" "lsr" "lxor" "match" "method"
    "mod" "module" "mutable" "new" "nonrec" "object" "of" "open" "or"
    "private" "rec" "sig" "struct" "then" "to" "true" "try" "type"
    "val" "virtual" "when" "while" "with"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "(fun inp -> ~A)" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "(fun inp -> ~A)" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "peg_seq [~{~A~^; ~}] inp" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "peg_alt [~{~A~^; ~}] inp" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "(match ~A with~{ | ~S -> ~A~} | _ -> fail_r inp \"no case\")"
            param
            (loop for (val body) in cases
                  collect val collect body))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "let r_ = ~A in if r_.failed then r_ else let ~A = r_.tag_int in let inp = r_.rest in ~A"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "let r_ = ~A in if r_.failed then r_ else let ~A = r_.tag in let inp = r_.rest in ~A"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "(Char.code ~A)" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "(in_flow ~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "(seq_spaces ~A ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    (if params
        (format nil "~A inp~{ ~A~}" name (mapcar #'peg-ident params))
        (format nil "~A inp" name))))

(def-tgt "fn-body"
  (lambda (sig body)
    (format nil "and ~A =~%    ~A" sig body)))

(def-tgt "fwd-decl" nil)

;;; ── Header ──

(def-tgt "header"
"(* ════════════════════════════════════════════════════════════════ *)
(* peg_yaml.ml — YAML 1.2 parser                                   *)
(* ════════════════════════════════════════════════════════════════ *)
")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"(* ── Input ── *)

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
    if c = '\\n' then { inp with pos = inp.pos + 1; line = inp.line + 1; col = 0 }
    else { inp with pos = inp.pos + 1; col = inp.col + 1 }"

"(* ── AST ── *)

type ast =
  | Branch of string * ast list
  | Leaf of string

let ast_tag = function Branch (t, _) -> t | Leaf _ -> \"SCALAR\"
let ast_children = function Branch (_, cs) -> cs | Leaf _ -> []
let ast_is_leaf = function Leaf _ -> true | _ -> false
let ast_text = function Leaf t -> t | _ -> \"\""

"(* ── Result ── *)

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

let ok_r inp = { failed = false; rval = \"\"; rest = inp; tag = \"\"; tag_int = 0; ast = None; ast_list = []; err = \"\" }
let ok_v inp v = { (ok_r inp) with rval = v }
let fail_r inp m = { (ok_r inp) with failed = true; err = m }"

"(* ── Context ── *)

let in_flow c =
  if c = \"FLOW-OUT\" || c = \"FLOW-IN\" then \"FLOW-IN\"
  else \"FLOW-KEY\"

let seq_spaces n c =
  if c = \"BLOCK-OUT\" then n - 1 else n"

"(* ── Combinators ── *)

type pfn = input -> result

let merge_asts acc r =
  let a = match r.ast with Some x -> [x] | None -> [] in
  acc @ a @ r.ast_list

let rec match_cp cp inp =
  if peek_cp inp = cp then
    ok_v (adv inp) (String.make 1 (Char.chr cp))
  else fail_r inp \"cp\"

and match_range lo hi inp =
  let c = peek_cp inp in
  if c >= lo && c <= hi then
    ok_v (adv inp) (String.make 1 (Char.chr c))
  else fail_r inp \"rng\"

and match_str t inp =
  let tlen = String.length t in
  if inp.pos + tlen > String.length inp.src then fail_r inp \"str\"
  else if String.sub inp.src inp.pos tlen <> t then fail_r inp \"str\"
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
  in go fns inp \"\" []

and peg_alt fns inp = match fns with
  | [] -> fail_r inp \"alt\"
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
  in go inp \"\" []

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
  if r.failed then ok_r inp else fail_r inp \"neg\"

and minus fa fb inp =
  let ra = fa inp in
  if ra.failed then ra
  else
    let rb = fb inp in
    if not rb.failed && rb.rest.pos = ra.rest.pos then fail_r inp \"excl\"
    else ra

and rep count f inp =
  if count <= 0 then ok_v inp \"\"
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
  if inp.pos = 0 then fail_r inp \"behind\"
  else
    let prev = { inp with pos = inp.pos - 1; col = max 0 (inp.col - 1) } in
    let r = f prev in
    if r.failed then fail_r inp \"behind\" else ok_r inp

and sol inp =
  if inp.col = 0 then ok_r inp else fail_r inp \"sol\"

and eof_ok inp =
  if at_eof inp then ok_r inp else fail_r inp \"eof\""

"(* ── YAML extensions ── *)

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
  if i + !sp < len && s.[i + !sp] <> '\\n' then
    { (ok_r inp) with tag_int = max 1 (!sp - n) }
  else
    let j = ref (i + !sp) in
    let result = ref None in
    while !j < len && !result = None do
      if s.[!j] = '\\n' then begin
        j := !j + 1;
        if !j < len then begin
          sp := 0;
          while !j + !sp < len && s.[!j + !sp] = ' ' do incr sp done;
          let nx = !j + !sp in
          if nx >= len || s.[nx] = '\\n' then j := nx
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
  { (ok_r inp) with tag = v }"))

;;; ── Combinator name overrides ──

(def-tgt "comb-star"        "star")
(def-tgt "comb-plus"        "plus_")
(def-tgt "comb-opt"         "opt")
(def-tgt "comb-neg"         "neg")
(def-tgt "comb-rep"         "rep")
(def-tgt "comb-ahead"       "ahead")
(def-tgt "comb-behind"      "behind")
(def-tgt "comb-minus"       "minus")
(def-tgt "comb-build"       "build_ast")
(def-tgt "comb-scalar"      "scalar_fn")
(def-tgt "comb-collect"     "collect_fn")
(def-tgt "comb-sol"         "sol")
(def-tgt "comb-eof"         "eof_ok")
(def-tgt "comb-ok"          "ok_r")
(def-tgt "comb-detect"      "detect_indent")
(def-tgt "comb-parse-int"   "parse_int_fn")
(def-tgt "comb-parse-sym"   "parse_sym_fn")
(def-tgt "comb-val"         "val_fn")

;;; ── API / Concerns ──

(def-tgt "api"
"(* ── API ── *)

and print_ast node depth =
  let indent = String.make (depth * 2) ' ' in
  match node with
  | Leaf t -> Printf.printf \"%sSCALAR: \\\"%s\\\"\\n\" indent t
  | Branch (t, cs) ->
    Printf.printf \"%s%s\\n\" indent t;
    List.iter (fun c -> print_ast c (depth + 1)) cs")

(def-tgt "main-fn"
"(* ── Main ── *)

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
      Printf.eprintf \"Usage: %s [file]\\n\" Sys.argv.(0);
      Printf.eprintf \"  Reads YAML from file or stdin.\\n\";
      Printf.eprintf \"  If no file given and stdin is a terminal, shows this help.\\n\";
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
    Printf.printf \"OK: %d chars\\n\" r.rest.pos;
    (match r.ast with Some a -> print_ast a 0 | None -> ())
  end else
    Printf.eprintf \"FAIL @%d: %s\\n\" r.rest.pos r.err")

(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)
