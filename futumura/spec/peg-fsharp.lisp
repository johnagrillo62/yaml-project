;;;; peg-fsharp.lisp — F# target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "FSharp")
(def-tgt "default-output" "PegYaml.fs")
(def-tgt "comment-prefix" "//")

(def-tgt "keywords"
  '("abstract" "and" "as" "assert" "base" "begin" "class" "default"
    "delegate" "do" "done" "downcast" "downto" "elif" "else" "end"
    "exception" "extern" "false" "finally" "fixed" "for" "fun"
    "function" "global" "if" "in" "inherit" "inline" "interface"
    "internal" "lazy" "let" "match" "member" "module" "mutable"
    "namespace" "new" "not" "null" "of" "open" "or" "override"
    "private" "public" "rec" "return" "select" "static" "struct"
    "then" "to" "true" "try" "type" "upcast" "use" "val" "void"
    "when" "while" "with" "yield"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "(fun inp -> (~A))" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "(fun inp -> (~A))" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "pegSeq [~{~A~^; ~}] inp" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "pegAlt [~{~A~^; ~}] inp" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "(match ~A with~{ | ~S -> ~A~} | _ -> failR inp \"no case\")"
            param
            (loop for (val body) in cases
                  collect val collect body))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "bindInt (fun inp -> ~A) (fun ~A -> fun inp -> ~A) inp"
            expr vname rest)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "bindCtx (fun inp -> ~A) (fun ~A -> fun inp -> ~A) inp"
            expr vname rest)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) (format nil "(int ~A)" name)))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "(inFlow ~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "(seqSpaces ~A ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "call-style" "haskell")
(def-tgt "list-sep" ";")

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
"// ════════════════════════════════════════════════════════════════
// PegYaml.fs — YAML 1.2 parser
// ════════════════════════════════════════════════════════════════
module PegYaml

open System
open System.IO
")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"// ── Input ──

type Input = { src: string; pos: int; line: int; col: int }

let mkInput s = { src = s; pos = 0; line = 1; col = 0 }

let atEof inp = inp.pos >= inp.src.Length

let peekCp inp =
    if atEof inp then -1
    else int inp.src.[inp.pos]

let adv inp =
    if atEof inp then inp
    else
        let c = inp.src.[inp.pos]
        if c = '\\n' then { inp with pos = inp.pos + 1; line = inp.line + 1; col = 0 }
        else { inp with pos = inp.pos + 1; col = inp.col + 1 }"

"// ── AST ──

type Ast =
    | Branch of string * Ast list
    | Leaf of string

let astTag = function Branch (t, _) -> t | Leaf _ -> \"SCALAR\"
let astChildren = function Branch (_, cs) -> cs | Leaf _ -> []
let astIsLeaf = function Leaf _ -> true | _ -> false
let astText = function Leaf t -> t | _ -> \"\""

"// ── Result ──

type Result = {
    failed: bool; rval: string; rest: Input; tag: string;
    tagInt: int; ast: Ast option; astList: Ast list; err: string
}

let okR inp = { failed = false; rval = \"\"; rest = inp; tag = \"\"; tagInt = 0; ast = None; astList = []; err = \"\" }
let okV inp v = { okR inp with rval = v }
let failR inp m = { okR inp with failed = true; err = m }"

"// ── Context ──

let inFlow c =
    if c = \"FLOW-OUT\" || c = \"FLOW-IN\" then \"FLOW-IN\"
    else \"FLOW-KEY\"

let seqSpaces n c =
    if c = \"BLOCK-OUT\" then n - 1 else n"

"// ── Combinators ──

type PFn = Input -> Result

let mergeAsts acc (r: Result) =
    let a = match r.ast with Some x -> [x] | None -> []
    acc @ a @ r.astList

let rec matchCp cp inp =
    if peekCp inp = cp then okV (adv inp) (string (char cp))
    else failR inp \"cp\"

and matchRange lo hi inp =
    let c = peekCp inp
    if c >= lo && c <= hi then okV (adv inp) (string (char c))
    else failR inp \"rng\"

and matchStr (t: string) inp =
    let tlen = t.Length
    if inp.pos + tlen > inp.src.Length then failR inp \"str\"
    elif inp.src.Substring(inp.pos, tlen) <> t then failR inp \"str\"
    else
        let mutable cur = inp
        for _ in 0..tlen-1 do cur <- adv cur
        okV cur t

and pegSeq fns inp =
    let rec go fs cur acc asts =
        match fs with
        | [] ->
            let res = okV cur acc
            match asts with [a] -> { res with ast = Some a } | [] -> res | _ -> { res with astList = asts }
        | f :: rest ->
            let r: Result = f cur
            if r.failed then r
            else go rest r.rest (acc + r.rval) (mergeAsts asts r)
    go fns inp \"\" []

and pegAlt fns inp =
    match fns with
    | [] -> failR inp \"alt\"
    | f :: rest ->
        let r = f inp
        if r.failed then pegAlt rest inp else r

and star f inp =
    let rec go cur acc asts =
        let r = f cur
        if r.failed || r.rest.pos <= cur.pos then
            let res = okV cur acc
            if List.isEmpty asts then res else { res with astList = asts }
        else go r.rest (acc + r.rval) (mergeAsts asts r)
    go inp \"\" []

and plus_ f inp =
    let r = f inp
    if r.failed then r
    else
        let r2 = star f r.rest
        let asts = mergeAsts (mergeAsts [] r) r2
        let res = okV r2.rest (r.rval + r2.rval)
        if List.isEmpty asts then res else { res with astList = asts }

and opt f inp =
    let r = f inp
    if r.failed then okR inp else r

and neg f inp =
    let r = f inp
    if r.failed then okR inp else failR inp \"neg\"

and minus fa fb inp =
    let ra = fa inp
    if ra.failed then ra
    else
        let rb = fb inp
        if not rb.failed && rb.rest.pos = ra.rest.pos then failR inp \"excl\"
        else ra

and rep count f inp =
    if count <= 0 then okV inp \"\"
    else
        let r = f inp
        if r.failed then r
        else
            let r2 = rep (count - 1) f r.rest
            if r2.failed then r2
            else okV r2.rest (r.rval + r2.rval)

and ahead f inp =
    let r = f inp
    if r.failed then r else { okR inp with rval = r.rval }

and behind f inp =
    if inp.pos = 0 then failR inp \"behind\"
    else
        let prev = { inp with pos = inp.pos - 1; col = max 0 (inp.col - 1) }
        let r = f prev
        if r.failed then failR inp \"behind\" else okR inp

and sol inp =
    if inp.col = 0 then okR inp else failR inp \"sol\"

and eofOk inp =
    if atEof inp then okR inp else failR inp \"eof\"

and bindInt (f: Input -> Result) (g: int -> Input -> Result) inp =
    let r = f inp
    if r.failed then r else g r.tagInt r.rest

and bindCtx (f: Input -> Result) (g: string -> Input -> Result) inp =
    let r = f inp
    if r.failed then r else g r.tag r.rest"

"// ── YAML extensions ──

and buildAst typ f inp =
    let r = f inp
    if r.failed then r
    else
        let children = (match r.ast with Some a -> [a] | None -> []) @ r.astList
        { r with ast = Some (Branch (typ, children)); astList = [] }

and scalarFn f inp =
    let r = f inp
    if r.failed then r
    else { r with ast = Some (Leaf r.rval) }

and collectFn f inp = f inp

and detectIndent n inp =
    let s = inp.src
    let len = s.Length
    let i = inp.pos
    let mutable sp = 0
    while i + sp < len && s.[i + sp] = ' ' do sp <- sp + 1
    if i + sp < len && s.[i + sp] <> '\\n' then
        { okR inp with tagInt = max 1 (sp - n) }
    else
        let mutable j = i + sp
        let mutable result = None
        while j < len && result.IsNone do
            if s.[j] = '\\n' then
                j <- j + 1
                if j < len then
                    sp <- 0
                    while j + sp < len && s.[j + sp] = ' ' do sp <- sp + 1
                    let nx = j + sp
                    if nx >= len || s.[nx] = '\\n' then j <- nx
                    else result <- Some { okR inp with tagInt = max 1 (sp - n) }
                else result <- Some { okR inp with tagInt = 1 }
            else result <- Some { okR inp with tagInt = 1 }
        match result with Some r -> r | None -> { okR inp with tagInt = 1 }

and parseIntFn f inp =
    let r = f inp
    if r.failed then r
    else
        let mutable v = 0
        for c in r.rval do
            if c >= '0' && c <= '9' then v <- v * 10 + (int c - int '0')
        { r with tagInt = v }

and parseSymFn f sym inp =
    let r = f inp
    if r.failed then r
    else { r with tag = sym }

and valFn v inp =
    { okR inp with tag = v }"))

;;; ── Combinator name overrides ──

(def-tgt "comb-match-cp"    "matchCp")
(def-tgt "comb-match-range" "matchRange")
(def-tgt "comb-match-str"   "matchStr")
(def-tgt "comb-star"        "star")
(def-tgt "comb-plus"        "plus_")
(def-tgt "comb-opt"         "opt")
(def-tgt "comb-neg"         "neg")
(def-tgt "comb-rep"         "rep")
(def-tgt "comb-ahead"       "ahead")
(def-tgt "comb-behind"      "behind")
(def-tgt "comb-minus"       "minus")
(def-tgt "comb-build"       "buildAst")
(def-tgt "comb-scalar"      "scalarFn")
(def-tgt "comb-collect"     "collectFn")
(def-tgt "comb-sol"         "sol")
(def-tgt "comb-eof"         "eofOk")
(def-tgt "comb-ok"          "okR")
(def-tgt "comb-detect"      "detectIndent")
(def-tgt "comb-parse-int"   "parseIntFn")
(def-tgt "comb-parse-sym"   "parseSymFn")
(def-tgt "comb-val"         "valFn")

;;; ── API ──

(def-tgt "api"
"// ── API ──

and printAst node depth =
    let indent = String.replicate (depth * 2) \" \"
    match node with
    | Leaf t -> printfn \"%sSCALAR: \\\"%s\\\"\" indent t
    | Branch (t, cs) ->
        printfn \"%s%s\" indent t
        List.iter (fun c -> printAst c (depth + 1)) cs")

(def-tgt "main-fn"
"// ── Main ──

[<EntryPoint>]
let main argv =
    let text =
        if argv.Length > 0 then File.ReadAllText(argv.[0])
        elif not Console.IsInputRedirected then
            eprintfn \"Usage: PegYaml [file]\"
            eprintfn \"  Reads YAML from file or stdin.\"
            eprintfn \"  If no file given and stdin is a terminal, shows this help.\"
            exit 1
            \"\"
        else
            use reader = new StreamReader(Console.OpenStandardInput())
            reader.ReadToEnd()
    let inp = mkInput text
    let r = l_yaml_stream inp
    if not r.failed then
        printfn \"OK: %d chars\" r.rest.pos
        match r.ast with Some a -> printAst a 0 | None -> ()
        0
    else
        eprintfn \"FAIL @%d: %s\" r.rest.pos r.err
        1")

(def-tgt "namespace-close" nil)
(def-tgt "yaml-concerns" nil)
(def-tgt "cv" nil)
