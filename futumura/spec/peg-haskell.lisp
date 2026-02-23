;;;; peg-haskell.lisp — Haskell target for emit-yaml-peg.lisp

(in-package #:yaml-eval)

;;; ── Identity ──

(def-tgt "target-name" "Haskell")
(def-tgt "default-output" "YamlReader.hs")
(def-tgt "comment-prefix" "--")
(def-tgt "call-style" "haskell")

(def-tgt "keywords"
  '("case" "class" "data" "default" "deriving" "do" "else" "forall"
    "foreign" "if" "import" "in" "infix" "infixl" "infixr" "instance"
    "let" "module" "newtype" "of" "qualified" "then" "type" "where"))
(def-tgt "keyword-prefix" "r_")

;;; ── Closure wrapping ──

;; Haskell closures: \inp -> body
(def-tgt "ref-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "(\\inp -> ~A)" body)))

(def-tgt "box-wrap"
  (lambda (body env)
    (declare (ignore env))
    (format nil "(\\inp -> ~A)" body)))

;;; ── Seq/Alt ──

(def-tgt "seq-emit"
  (lambda (wrapped-fns)
    (format nil "pegSeq [~{~A~^, ~}] inp" wrapped-fns)))

(def-tgt "alt-emit"
  (lambda (wrapped-fns)
    (format nil "pegAlt [~{~A~^, ~}] inp" wrapped-fns)))

;;; ── Switch ──

(def-tgt "switch-emit"
  (lambda (param cases)
    (format nil "(\\_ -> ~{if ~A == ~S then ~A else ~}failR inp \"no case\") ()"
            (loop for (val body) in cases
                  collect param collect val collect body))))

;;; ── Let ──

(def-tgt "let-int"
  (lambda (vname expr rest)
    (format nil "bindInt (\\~A inp -> ~A) (\\inp -> ~A) inp"
            vname rest expr)))

(def-tgt "let-ctx"
  (lambda (vname expr rest)
    (format nil "bindCtx (\\~A inp -> ~A) (\\inp -> ~A) inp"
            vname rest expr)))

;;; ── Arg compilation ──

(def-tgt "param-ref"
  (lambda (sym env)
    (declare (ignore env))
    (peg-ident sym)))

(def-tgt "ctx-literal"
  (lambda (s) (format nil "~S" s)))

(def-tgt "char-cast"
  (lambda (name) name))

(def-tgt "in-flow-call"
  (lambda (arg) (format nil "(inFlow ~A)" arg)))

(def-tgt "seq-spaces-call"
  (lambda (n c) (format nil "(seqSpaces ~A ~A)" n c)))

;;; ── Function signatures ──

(def-tgt "fn-sig"
  (lambda (name params)
    ;; Return (sig . type-sig) pair for Haskell
    (let ((sig (if params
                   (format nil "~A inp~{ ~A~}" name (mapcar #'peg-ident params))
                   (format nil "~A inp" name)))
          (tsig (if params
                    (format nil "~A :: Input -> ~{~A -> ~}Result" name
                            (mapcar (lambda (p)
                                      (if (member (symbol-name p) '("N" "M") :test #'string-equal)
                                          "Int" "String"))
                                    params))
                    (format nil "~A :: Input -> Result" name))))
      (cons sig tsig))))

(def-tgt "fn-body"
  (lambda (sig-pair body)
    (let ((sig (car sig-pair))
          (tsig (cdr sig-pair)))
      (format nil "~A~%~A =~%    ~A" tsig sig body))))

(def-tgt "fwd-decl" nil) ;; Haskell doesn't need forward declarations

;;; ── Header ──

(def-tgt "header"
"-- ════════════════════════════════════════════════════════════════
module Main where

import Data.Char (ord, chr, isDigit, isHexDigit)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.IORef
import System.Environment (getArgs)
import System.IO (hGetContents, stdin)")

;;; ── Runtime ──

(def-tgt "runtime-sections"
  (list
"-- ── Input ──

data Input = Input
  { src  :: !String
  , pos  :: !Int
  , line :: !Int
  , col  :: !Int
  } deriving (Show)

mkInput :: String -> Input
mkInput s = Input s 0 1 0

atEof :: Input -> Bool
atEof inp = pos inp >= length (src inp)

peekCp :: Input -> Int
peekCp inp
  | atEof inp = -1
  | otherwise = ord (src inp !! pos inp)

adv :: Input -> Input
adv inp
  | atEof inp = inp
  | c == '\\n' = inp { pos = pos inp + 1, line = line inp + 1, col = 0 }
  | otherwise = inp { pos = pos inp + 1, col = col inp + 1 }
  where c = src inp !! pos inp"

"-- ── AST ──

data Ast = Branch !String [Ast] | Leaf !String deriving (Show)

astTag :: Ast -> String
astTag (Branch t _) = t
astTag (Leaf _) = \"SCALAR\"

astChildren :: Ast -> [Ast]
astChildren (Branch _ cs) = cs
astChildren (Leaf _) = []

astIsLeaf :: Ast -> Bool
astIsLeaf (Leaf _) = True
astIsLeaf _ = False

astText :: Ast -> String
astText (Leaf t) = t
astText _ = \"\""

"-- ── Result ──

data Result = Result
  { failed  :: !Bool
  , rval    :: !String
  , rest    :: !Input
  , tag     :: !String
  , tagInt  :: !Int
  , ast     :: !(Maybe Ast)
  , astList :: ![Ast]
  , err     :: !String
  } deriving (Show)

okR :: Input -> Result
okR inp = Result False \"\" inp \"\" 0 Nothing [] \"\"

okV :: Input -> String -> Result
okV inp v = Result False v inp \"\" 0 Nothing [] \"\"

failR :: Input -> String -> Result
failR inp m = Result True \"\" inp \"\" 0 Nothing [] m"

"-- ── Context ──

inFlow :: String -> String
inFlow c
  | c == \"FLOW-OUT\" || c == \"FLOW-IN\" = \"FLOW-IN\"
  | otherwise = \"FLOW-KEY\"

seqSpaces :: Int -> String -> Int
seqSpaces n c = if c == \"BLOCK-OUT\" then n - 1 else n"

"-- ── Combinators ──

type PFn = Input -> Result

matchCp :: Int -> PFn
matchCp cp inp
  | peekCp inp == cp = okV (adv inp) [chr cp]
  | otherwise = failR inp \"cp\"

matchRange :: Int -> Int -> PFn
matchRange lo hi inp
  | let c = peekCp inp in c >= lo && c <= hi = okV (adv inp) [chr (peekCp inp)]
  | otherwise = failR inp \"rng\"

matchStr :: String -> PFn
matchStr t inp
  | t `isPrefixOf` drop (pos inp) (src inp) =
      let n = length t
          inp' = iterate adv inp !! n
      in okV inp' t
  | otherwise = failR inp \"str\"

mergeAsts :: [Ast] -> Result -> [Ast]
mergeAsts acc r =
  let a = maybe [] (:[]) (ast r)
  in acc ++ a ++ astList r

pegSeq :: [PFn] -> PFn
pegSeq [] inp = okR inp
pegSeq fns inp = go fns inp \"\" []
  where
    go [] cur acc asts =
      let res = okV cur acc
      in case asts of
           [a] -> res { ast = Just a }
           []  -> res
           _   -> res { astList = asts }
    go (f:fs) cur acc asts =
      let r = f cur
      in if failed r then r
         else go fs (rest r) (acc ++ rval r) (mergeAsts asts r)

pegAlt :: [PFn] -> PFn
pegAlt [] inp = failR inp \"alt\"
pegAlt (f:fs) inp =
  let r = f inp
  in if failed r then pegAlt fs inp else r

starP :: PFn -> PFn
starP f inp = go inp \"\" []
  where
    go cur acc asts =
      let r = f cur
      in if failed r || pos (rest r) <= pos cur
         then let res = okV cur acc
              in if null asts then res else res { astList = asts }
         else go (rest r) (acc ++ rval r) (mergeAsts asts r)

plusP :: PFn -> PFn
plusP f inp =
  let r = f inp
  in if failed r then r
     else let r2 = starP f (rest r)
              asts = mergeAsts (mergeAsts [] r) r2
          in (okV (rest r2) (rval r ++ rval r2))
             { astList = if null asts then [] else asts }

optP :: PFn -> PFn
optP f inp = let r = f inp in if failed r then okR inp else r

negP :: PFn -> PFn
negP f inp = let r = f inp in if failed r then okR inp else failR inp \"neg\"

minusP :: PFn -> PFn -> PFn
minusP fa fb inp =
  let ra = fa inp
  in if failed ra then ra
     else let rb = fb inp
          in if not (failed rb) && pos (rest rb) == pos (rest ra)
             then failR inp \"excl\"
             else ra

repP :: Int -> PFn -> PFn
repP 0 _ inp = okV inp \"\"
repP n f inp =
  let r = f inp
  in if failed r then r
     else let r2 = repP (n - 1) f (rest r)
          in if failed r2 then r2
             else okV (rest r2) (rval r ++ rval r2)

aheadP :: PFn -> PFn
aheadP f inp = let r = f inp in if failed r then r else okR inp

behindP :: PFn -> PFn
behindP f inp
  | pos inp == 0 = failR inp \"bh\"
  | otherwise =
      let t = inp { pos = pos inp - 1, col = max 0 (col inp - 1) }
          r = f t
      in if failed r then failR inp \"bh\" else okR inp

solP :: PFn
solP inp = if col inp == 0 then okR inp else failR inp \"sol\"

eofP :: PFn
eofP inp = if atEof inp then okR inp else failR inp \"eof\""

"-- ── YAML extensions ──

-- Aliases: emitter generates snake_case, runtime uses camelCase
match_cp :: Int -> PFn
match_cp = matchCp

match_range :: Int -> Int -> PFn
match_range = matchRange

match_str :: String -> PFn
match_str = matchStr

star :: PFn -> PFn
star = starP

plus_ :: PFn -> PFn
plus_ = plusP

opt :: PFn -> PFn
opt = optP

neg :: PFn -> PFn
neg = negP

minus :: PFn -> PFn -> PFn
minus = minusP

rep :: Int -> PFn -> PFn
rep = repP

ahead :: PFn -> PFn
ahead = aheadP

behind :: PFn -> PFn
behind = behindP

ok :: PFn
ok = okR

sol :: PFn
sol = solP

eof_ok :: PFn
eof_ok = eofP

build :: String -> PFn -> PFn
build = buildP

scalar :: PFn -> PFn
scalar = scalarP

collect :: PFn -> PFn
collect = collectP

detect_indent :: Int -> PFn
detect_indent n inp = detectIndent inp n

parse_int :: PFn -> PFn
parse_int = parseIntP

parse_sym :: PFn -> String -> PFn
parse_sym = parseSymP

val :: String -> PFn
val = valP

in_flow :: String -> String
in_flow = inFlow

seq_spaces :: Int -> String -> Int
seq_spaces = seqSpaces

buildP :: String -> PFn -> PFn
buildP typ f inp =
  let r = f inp
  in if failed r then r
     else let children = maybe [] (:[]) (ast r) ++ astList r
              node = Branch typ children
          in r { ast = Just node, astList = [] }

scalarP :: PFn -> PFn
scalarP f inp =
  let r = f inp
  in if failed r then r
     else r { ast = Just (Leaf (rval r)) }

collectP :: PFn -> PFn
collectP f inp = f inp

detectIndent :: Input -> Int -> Result
detectIndent inp n =
  let s = src inp
      len = length s
      i = pos inp
      sp = length (takeWhile (== ' ') (drop i s))
  in if i + sp < len && s !! (i + sp) /= '\\n'
     then (okR inp) { tagInt = max 1 (sp - n) }
     else scanLines s len (i + sp) n
  where
    scanLines s len j n
      | j >= len = (okR inp) { tagInt = 1 }
      | s !! j == '\\n' =
          let j' = j + 1
          in if j' >= len then (okR inp) { tagInt = 1 }
             else let sp = length (takeWhile (== ' ') (drop j' s))
                      nx = j' + sp
                  in if nx >= len || s !! nx == '\\n'
                     then scanLines s len nx n
                     else (okR inp) { tagInt = max 1 (sp - n) }
      | otherwise = (okR inp) { tagInt = 1 }

parseIntP :: PFn -> PFn
parseIntP f inp =
  let r = f inp
  in if failed r then r
     else let digits = filter isDigit (rval r)
              v = if null digits then 0 else read digits :: Int
          in r { tagInt = v }

parseSymP :: PFn -> String -> PFn
parseSymP f sym inp =
  let r = f inp
  in if failed r then r
     else r { tag = sym }

valP :: String -> PFn
valP v inp = (okR inp) { tag = v }

bindInt :: (Int -> PFn) -> PFn -> PFn
bindInt k f inp =
  let r = f inp
  in if failed r then r else k (tagInt r) (rest r)

bindCtx :: (String -> PFn) -> PFn -> PFn
bindCtx k f inp =
  let r = f inp
  in if failed r then r else k (tag r) (rest r)"))

;;; ── Combinator references ──
;;; These map the emitter's combinator calls to Haskell function names

(def-tgt "comb-match-cp"    "matchCp")
(def-tgt "comb-match-range" "matchRange")
(def-tgt "comb-match-str"   "matchStr")
(def-tgt "comb-star"        "starP")
(def-tgt "comb-plus"        "plusP")
(def-tgt "comb-opt"         "optP")
(def-tgt "comb-neg"         "negP")
(def-tgt "comb-rep"         "repP")
(def-tgt "comb-ahead"       "aheadP")
(def-tgt "comb-behind"      "behindP")
(def-tgt "comb-minus"       "minusP")
(def-tgt "comb-build"       "buildP")
(def-tgt "comb-scalar"      "scalarP")
(def-tgt "comb-collect"     "collectP")
(def-tgt "comb-sol"         "solP")
(def-tgt "comb-eof"         "eofP")
(def-tgt "comb-ok"          "okR")
(def-tgt "comb-detect"      "detectIndent")
(def-tgt "comb-parse-int"   "parseIntP")
(def-tgt "comb-parse-sym"   "parseSymP")
(def-tgt "comb-val"         "valP")

;;; ── API ──

(def-tgt "api"
"-- ── API ──

printAst :: Ast -> Int -> IO ()
printAst node depth = do
  let indent = replicate (depth * 2) ' '
  case node of
    Leaf t -> putStrLn (indent ++ \"SCALAR: \\\"\" ++ t ++ \"\\\"\")
    Branch t cs -> do
      putStrLn (indent ++ t)
      mapM_ (\\c -> printAst c (depth + 1)) cs")

;;; ── Main ──

(def-tgt "main-fn"
"-- ── Main ──

main :: IO ()
main = do
  args <- getArgs
  text <- case args of
    (f:_) -> readFile f
    []    -> hGetContents stdin
  let inp = mkInput text
      r = l_yaml_stream inp
  if not (failed r) then do
    putStrLn (\"OK: \" ++ show (pos (rest r)) ++ \" chars\")
    case ast r of
      Just a  -> printAst a 0
      Nothing -> return ()
  else do
    putStrLn (\"FAIL @\" ++ show (pos (rest r)) ++ \": \" ++ err r)")

;;; ── Concern Vocab ──

(let ((cv (make-hash-table :test 'equal)))
  (setf (gethash "value-type-decl" cv)
"-- ── Native Value Type ──

data YamlValue
  = YNull
  | YBool !Bool
  | YInt !Integer
  | YFloat !Double
  | YStr !String
  | YMap ![(String, YamlValue)]
  | YSeq ![YamlValue]
  deriving (Show)

yStr :: YamlValue -> String
yStr (YStr s) = s
yStr _ = \"\"

yGet :: String -> YamlValue -> YamlValue
yGet key (YMap pairs) = case lookup key pairs of
  Just v  -> v
  Nothing -> YNull
yGet _ _ = YNull

yAt :: Int -> YamlValue -> YamlValue
yAt i (YSeq xs) = if i < length xs then xs !! i else YNull
yAt _ _ = YNull

isMap :: YamlValue -> Bool
isMap (YMap _) = True
isMap _ = False")

  (setf (gethash "coerce-fn" cv)
"coerceScalar :: String -> YamlValue
coerceScalar s
  | s `elem` [\"null\", \"Null\", \"NULL\", \"~\", \"\"] = YNull
  | s `elem` [\"true\", \"True\", \"TRUE\"] = YBool True
  | s `elem` [\"false\", \"False\", \"FALSE\"] = YBool False
  | s `elem` [\".inf\", \".Inf\", \".INF\", \"+.inf\"] = YFloat (1/0)
  | s `elem` [\"-.inf\", \"-.Inf\", \"-.INF\"] = YFloat (-1/0)
  | s `elem` [\".nan\", \".NaN\", \".NAN\"] = YFloat (0/0)
  | Just i <- tryParseInt s = YInt i
  | Just f <- tryParseFloat s = YFloat f
  | otherwise = YStr s

tryParseInt :: String -> Maybe Integer
tryParseInt s
  | null s = Nothing
  | take 2 s == \"0x\" || take 2 s == \"0X\" =
      let hex = drop 2 s
      in if not (null hex) && all isHexDigit hex
         then Just (read (\"0x\" ++ hex))
         else Nothing
  | take 2 s == \"0o\" || take 2 s == \"0O\" =
      let oct = drop 2 s
      in if not (null oct) && all (\\c -> c >= '0' && c <= '7') oct
         then Just (read (\"0o\" ++ oct))
         else Nothing
  | head s == '-' || head s == '+' =
      let rest_ = tail s
      in if not (null rest_) && all isDigit rest_
         then Just (read s)
         else Nothing
  | all isDigit s = Just (read s)
  | otherwise = Nothing

tryParseFloat :: String -> Maybe Double
tryParseFloat s = case reads s of
  [(f, \"\")] -> Just f
  _          -> Nothing")

  (setf (gethash "converter-decl" cv)
"type AnchorMap = Map.Map String YamlValue")

  (setf (gethash "convert-fn" cv)
"convert :: AnchorMap -> Ast -> (YamlValue, AnchorMap)
convert anchors node = case node of
  Leaf t -> (coerceScalar t, anchors)
  Branch t cs -> case t of
    \"ANCHOR\" ->
      let (name, val, anc) = foldl anchorStep (\"\", YNull, anchors) cs
      in (val, if null name then anc else Map.insert name val anc)
    \"ALIAS\" ->
      let found = foldr (\\c acc -> case c of
            Leaf txt -> case Map.lookup txt anchors of
              Just v  -> v
              Nothing -> acc
            _ -> acc) YNull cs
      in (found, anchors)
    \"MAPPING\" ->
      let (pairs, anc) = foldl mapStep ([], anchors) cs
      in (YMap (reverse pairs), anc)
    \"SEQUENCE\" ->
      let (items, anc) = foldl seqStep ([], anchors) cs
      in (YSeq (reverse items), anc)
    \"DOC\" -> convertChildren anchors cs
    \"STREAM\" -> convertChildren anchors cs
    \"PAIR\" ->
      if length cs >= 2
      then let (_, anc) = convert anchors (cs !! 0)
               (v, anc2) = convert anc (cs !! 1)
           in (v, anc2)
      else (YNull, anchors)
    _ ->
      if length cs == 1
      then convert anchors (head cs)
      else let (items, anc) = foldl seqStep ([], anchors) cs
           in (YSeq (reverse items), anc)

anchorStep :: (String, YamlValue, AnchorMap) -> Ast -> (String, YamlValue, AnchorMap)
anchorStep (name, val, anc) c
  | astIsLeaf c && null name = (astText c, val, anc)
  | otherwise = let (v, anc') = convert anc c in (name, v, anc')

mapStep :: ([(String, YamlValue)], AnchorMap) -> Ast -> ([(String, YamlValue)], AnchorMap)
mapStep (pairs, anc) c
  | astTag c == \"PAIR\" && length (astChildren c) >= 2 =
      let (key, anc1) = convert anc (astChildren c !! 0)
          (val, anc2) = convert anc1 (astChildren c !! 1)
      in if yStr key == \"<<\" && isMap val
         then case val of
           YMap mp -> (foldl (\\ps (mk, mv) ->
             if any (\\(k,_) -> k == mk) ps then ps else (mk,mv):ps) pairs mp, anc2)
           _ -> ((yStr key, val):pairs, anc2)
         else ((yStr key, val):pairs, anc2)
  | otherwise = (pairs, anc)

seqStep :: ([YamlValue], AnchorMap) -> Ast -> ([YamlValue], AnchorMap)
seqStep (items, anc) c = let (v, anc') = convert anc c in (v:items, anc')

convertChildren :: AnchorMap -> [Ast] -> (YamlValue, AnchorMap)
convertChildren anc cs
  | length cs == 1 = convert anc (head cs)
  | otherwise =
      let (items, anc') = foldl seqStep ([], anc) cs
          xs = reverse items
      in if length xs == 1 then (head xs, anc') else (YSeq xs, anc')")

  (setf (gethash "load-fn" cv)
"yamlLoad :: String -> YamlValue
yamlLoad text =
  let inp = mkInput text
      r = l_yaml_stream inp
  in if failed r then YNull
     else case ast r of
       Just a  -> fst (convert Map.empty a)
       Nothing -> YNull")
  (def-tgt "cv" cv))
