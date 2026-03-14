// ════════════════════════════════════════════════════════════════
// PegJson.fs — JSON (RFC 8259) parser
// ════════════════════════════════════════════════════════════════
module PegJson

open System
open System.IO


// ── Input ──

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
        if c = '\n' then { inp with pos = inp.pos + 1; line = inp.line + 1; col = 0 }
        else { inp with pos = inp.pos + 1; col = inp.col + 1 }

// ── AST ──

type Ast =
    | Branch of string * Ast list
    | Leaf of string

let astTag = function Branch (t, _) -> t | Leaf _ -> "SCALAR"
let astChildren = function Branch (_, cs) -> cs | Leaf _ -> []
let astIsLeaf = function Leaf _ -> true | _ -> false
let astText = function Leaf t -> t | _ -> ""

// ── Result ──

type Result = {
    failed: bool; rval: string; rest: Input; tag: string;
    tagInt: int; ast: Ast option; astList: Ast list; err: string
}

let okR inp = { failed = false; rval = ""; rest = inp; tag = ""; tagInt = 0; ast = None; astList = []; err = "" }
let okV inp v = { okR inp with rval = v }
let failR inp m = { okR inp with failed = true; err = m }

// ── Context ──

let inFlow c =
    if c = "FLOW-OUT" || c = "FLOW-IN" then "FLOW-IN"
    else "FLOW-KEY"

let seqSpaces n c =
    if c = "BLOCK-OUT" then n - 1 else n

// ── Combinators ──

type PFn = Input -> Result

let mergeAsts acc (r: Result) =
    let a = match r.ast with Some x -> [x] | None -> []
    acc @ a @ r.astList

let rec matchCp cp inp =
    if peekCp inp = cp then okV (adv inp) (string (char cp))
    else failR inp "cp"

and matchRange lo hi inp =
    let c = peekCp inp
    if c >= lo && c <= hi then okV (adv inp) (string (char c))
    else failR inp "rng"

and matchStr (t: string) inp =
    let tlen = t.Length
    if inp.pos + tlen > inp.src.Length then failR inp "str"
    elif inp.src.Substring(inp.pos, tlen) <> t then failR inp "str"
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
    go fns inp "" []

and pegAlt fns inp =
    match fns with
    | [] -> failR inp "alt"
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
    go inp "" []

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
    if r.failed then okR inp else failR inp "neg"

and minus fa fb inp =
    let ra = fa inp
    if ra.failed then ra
    else
        let rb = fb inp
        if not rb.failed && rb.rest.pos = ra.rest.pos then failR inp "excl"
        else ra

and rep count f inp =
    if count <= 0 then okV inp ""
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
    if inp.pos = 0 then failR inp "behind"
    else
        let prev = { inp with pos = inp.pos - 1; col = max 0 (inp.col - 1) }
        let r = f prev
        if r.failed then failR inp "behind" else okR inp

and sol inp =
    if inp.col = 0 then okR inp else failR inp "sol"

and eofOk inp =
    if atEof inp then okR inp else failR inp "eof"

// ════════════════════════════════════════════════════════════════ 
// YAML 1.2 Grammar — 211 rules 
// ════════════════════════════════════════════════════════════════ 

// [1] JSON-TEXT 
and json_text inp =
    pegSeq ([
        (fun inp -> ws inp);
        (fun inp -> r_value inp);
        (fun inp -> ws inp);
        (fun inp -> eofOk inp)]) inp

// [2] VALUE 
and r_value inp =
    pegAlt ([
        (fun inp -> r_object inp);
        (fun inp -> r_array inp);
        (fun inp -> r_string inp);
        (fun inp -> r_number inp);
        (fun inp -> matchStr "true" inp);
        (fun inp -> matchStr "false" inp);
        (fun inp -> matchStr "null" inp)]) inp

// [3] OBJECT 
and r_object inp =
    pegAlt ([
        (fun inp ->
    pegSeq ([
                (fun inp -> matchCp 123 inp);
                (fun inp -> ws inp);
                (fun inp -> members inp);
                (fun inp -> ws inp);
                (fun inp -> matchCp 125 inp)]) inp);
        (fun inp -> pegSeq ([(fun inp -> matchCp 123 inp); (fun inp -> ws inp); (fun inp -> matchCp 125 inp)]) inp)]) inp

// [4] MEMBERS 
and members inp =
    pegSeq ([
        (fun inp -> r_member inp);
        (fun inp ->
    star (fun inp ->
        pegSeq ([
                    (fun inp -> ws inp);
                    (fun inp -> matchCp 44 inp);
                    (fun inp -> ws inp);
                    (fun inp -> r_member inp)]) inp) inp)]) inp

// [5] MEMBER 
and r_member inp =
    pegSeq ([
        (fun inp -> ws inp);
        (fun inp -> r_string inp);
        (fun inp -> ws inp);
        (fun inp -> matchCp 58 inp);
        (fun inp -> ws inp);
        (fun inp -> r_value inp);
        (fun inp -> ws inp)]) inp

// [6] ARRAY 
and r_array inp =
    pegAlt ([
        (fun inp ->
    pegSeq ([
                (fun inp -> matchCp 91 inp);
                (fun inp -> ws inp);
                (fun inp -> elements inp);
                (fun inp -> ws inp);
                (fun inp -> matchCp 93 inp)]) inp);
        (fun inp -> pegSeq ([(fun inp -> matchCp 91 inp); (fun inp -> ws inp); (fun inp -> matchCp 93 inp)]) inp)]) inp

// [7] ELEMENTS 
and elements inp =
    pegSeq ([
        (fun inp -> r_value inp);
        (fun inp ->
    star (fun inp ->
        pegSeq ([
                    (fun inp -> ws inp);
                    (fun inp -> matchCp 44 inp);
                    (fun inp -> ws inp);
                    (fun inp -> r_value inp)]) inp) inp)]) inp

// [8] STRING 
and r_string inp =
    pegSeq ([
        (fun inp -> matchCp 34 inp);
        (fun inp -> star (fun inp -> r_char inp) inp);
        (fun inp -> matchCp 34 inp)]) inp

// [9] CHAR 
and r_char inp =
    pegAlt ([
        (fun inp -> escaped inp);
        (fun inp ->
    pegSeq ([
                (fun inp -> neg (fun inp -> matchCp 34 inp) inp);
                (fun inp -> neg (fun inp -> matchCp 92 inp) inp);
                (fun inp -> neg (fun inp -> matchCp 0x0 inp) inp);
                (fun inp -> neg (fun inp -> matchRange 0x0 0x1F inp) inp);
                (fun inp -> matchRange 0x20 0x10FFFF inp)]) inp)]) inp

// [10] ESCAPED 
and escaped inp =
    pegSeq ([
        (fun inp -> matchCp 92 inp);
        (fun inp ->
    pegAlt ([
                (fun inp -> matchCp 34 inp);
                (fun inp -> matchCp 92 inp);
                (fun inp -> matchCp 47 inp);
                (fun inp -> matchCp 98 inp);
                (fun inp -> matchCp 102 inp);
                (fun inp -> matchCp 110 inp);
                (fun inp -> matchCp 114 inp);
                (fun inp -> matchCp 116 inp);
                (fun inp -> pegSeq ([(fun inp -> matchCp 117 inp); (fun inp -> hex4 inp)]) inp)]) inp)]) inp

// [11] HEX4 
and hex4 inp =
    pegSeq ([
        (fun inp -> hexdig inp);
        (fun inp -> hexdig inp);
        (fun inp -> hexdig inp);
        (fun inp -> hexdig inp)]) inp

// [12] HEXDIG 
and hexdig inp =
    pegAlt ([
        (fun inp -> matchRange 48 57 inp);
        (fun inp -> matchRange 97 102 inp);
        (fun inp -> matchRange 65 70 inp)]) inp

// [13] NUMBER 
and r_number inp =
    pegSeq ([
        (fun inp -> opt (fun inp -> matchCp 45 inp) inp);
        (fun inp -> r_integer inp);
        (fun inp -> opt (fun inp -> fraction inp) inp);
        (fun inp -> opt (fun inp -> exponent inp) inp)]) inp

// [14] INTEGER 
and r_integer inp =
    pegAlt ([
        (fun inp -> matchCp 48 inp);
        (fun inp ->
    pegSeq ([
                (fun inp -> matchRange 49 57 inp);
                (fun inp -> star (fun inp -> matchRange 48 57 inp) inp)]) inp)]) inp

// [15] FRACTION 
and fraction inp =
    pegSeq ([(fun inp -> matchCp 46 inp); (fun inp -> plus_ (fun inp -> matchRange 48 57 inp) inp)]) inp

// [16] EXPONENT 
and exponent inp =
    pegSeq ([
        (fun inp -> pegAlt ([(fun inp -> matchCp 101 inp); (fun inp -> matchCp 69 inp)]) inp);
        (fun inp -> opt (fun inp -> pegAlt ([(fun inp -> matchCp 43 inp); (fun inp -> matchCp 45 inp)]) inp) inp);
        (fun inp -> plus_ (fun inp -> matchRange 48 57 inp) inp)]) inp

// [17] WS 
and ws inp =
    star (fun inp ->
    pegAlt ([
            (fun inp -> matchCp 0x20 inp);
            (fun inp -> matchCp 0x9 inp);
            (fun inp -> matchCp 0x0A inp);
            (fun inp -> matchCp 0x0D inp)]) inp) inp

// ── API ──

and printAst node depth =
    let indent = String.replicate (depth * 2) " "
    match node with
    | Leaf t -> printfn "%sSCALAR: \"%s\"" indent t
    | Branch (t, cs) ->
        printfn "%s%s" indent t
        List.iter (fun c -> printAst c (depth + 1)) cs

// ── Main ──

[<EntryPoint>]
let main argv =
    let text =
        if argv.Length > 0 then File.ReadAllText(argv.[0])
        elif not Console.IsInputRedirected then
            eprintfn "Usage: PegJson [file]"
            eprintfn "  Reads JSON from file or stdin."
            eprintfn "  If no file given and stdin is a terminal, shows this help."
            exit 1
            ""
        else
            use reader = new StreamReader(Console.OpenStandardInput())
            reader.ReadToEnd()
    let inp = mkInput text
    let r = json_text inp
    if not r.failed then
        printfn "OK: %d chars" r.rest.pos
        match r.ast with Some a -> printAst a 0 | None -> ()
        0
    else
        eprintfn "FAIL @%d: %s" r.rest.pos r.err
        1
