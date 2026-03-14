-- ════════════════════════════════════════════════════════════════
module Main where

import Data.Char (ord, chr, isDigit, isHexDigit)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.IORef
import System.Environment (getArgs)
import System.IO (hGetContents, stdin, stderr, hPutStrLn)
import System.Exit (exitWith, ExitCode(..), exitSuccess)

-- ── Input ──

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
  | c == '\n' = inp { pos = pos inp + 1, line = line inp + 1, col = 0 }
  | otherwise = inp { pos = pos inp + 1, col = col inp + 1 }
  where c = src inp !! pos inp

-- ── AST ──

data Ast = Branch !String [Ast] | Leaf !String deriving (Show)

astTag :: Ast -> String
astTag (Branch t _) = t
astTag (Leaf _) = "SCALAR"

astChildren :: Ast -> [Ast]
astChildren (Branch _ cs) = cs
astChildren (Leaf _) = []

astIsLeaf :: Ast -> Bool
astIsLeaf (Leaf _) = True
astIsLeaf _ = False

astText :: Ast -> String
astText (Leaf t) = t
astText _ = ""

-- ── Result ──

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
okR inp = Result False "" inp "" 0 Nothing [] ""

okV :: Input -> String -> Result
okV inp v = Result False v inp "" 0 Nothing [] ""

failR :: Input -> String -> Result
failR inp m = Result True "" inp "" 0 Nothing [] m

-- ── Context ──

inFlow :: String -> String
inFlow c
  | c == "FLOW-OUT" || c == "FLOW-IN" = "FLOW-IN"
  | otherwise = "FLOW-KEY"

seqSpaces :: Int -> String -> Int
seqSpaces n c = if c == "BLOCK-OUT" then n - 1 else n

-- ── Combinators ──

type PFn = Input -> Result

matchCp :: Int -> PFn
matchCp cp inp
  | peekCp inp == cp = okV (adv inp) [chr cp]
  | otherwise = failR inp "cp"

matchRange :: Int -> Int -> PFn
matchRange lo hi inp
  | let c = peekCp inp in c >= lo && c <= hi = okV (adv inp) [chr (peekCp inp)]
  | otherwise = failR inp "rng"

matchStr :: String -> PFn
matchStr t inp
  | t `isPrefixOf` drop (pos inp) (src inp) =
      let n = length t
          inp' = iterate adv inp !! n
      in okV inp' t
  | otherwise = failR inp "str"

mergeAsts :: [Ast] -> Result -> [Ast]
mergeAsts acc r =
  let a = maybe [] (:[]) (ast r)
  in acc ++ a ++ astList r

pegSeq :: [PFn] -> PFn
pegSeq [] inp = okR inp
pegSeq fns inp = go fns inp "" []
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
pegAlt [] inp = failR inp "alt"
pegAlt (f:fs) inp =
  let r = f inp
  in if failed r then pegAlt fs inp else r

starP :: PFn -> PFn
starP f inp = go inp "" []
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
negP f inp = let r = f inp in if failed r then okR inp else failR inp "neg"

minusP :: PFn -> PFn -> PFn
minusP fa fb inp =
  let ra = fa inp
  in if failed ra then ra
     else let rb = fb inp
          in if not (failed rb) && pos (rest rb) == pos (rest ra)
             then failR inp "excl"
             else ra

repP :: Int -> PFn -> PFn
repP 0 _ inp = okV inp ""
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
  | pos inp == 0 = failR inp "bh"
  | otherwise =
      let t = inp { pos = pos inp - 1, col = max 0 (col inp - 1) }
          r = f t
      in if failed r then failR inp "bh" else okR inp

solP :: PFn
solP inp = if col inp == 0 then okR inp else failR inp "sol"

eofP :: PFn
eofP inp = if atEof inp then okR inp else failR inp "eof"

-- ════════════════════════════════════════════════════════════════ 
-- YAML 1.2 Grammar — 211 rules 
-- ════════════════════════════════════════════════════════════════ 

-- [1] JSON-TEXT 
json_text :: Input -> Result
json_text inp =
    pegSeq [(\inp -> ws inp), (\inp -> r_value inp), (\inp -> ws inp), (\inp -> eofP inp)] inp

-- [2] VALUE 
r_value :: Input -> Result
r_value inp =
    pegAlt [
        (\inp -> r_object inp),
        (\inp -> r_array inp),
        (\inp -> r_string inp),
        (\inp -> r_number inp),
        (\inp -> matchStr "true" inp),
        (\inp -> matchStr "false" inp),
        (\inp -> matchStr "null" inp)] inp

-- [3] OBJECT 
r_object :: Input -> Result
r_object inp =
    pegAlt [
        (\inp -> pegSeq [
            (\inp -> matchCp 123 inp),
            (\inp -> ws inp),
            (\inp -> r_members inp),
            (\inp -> ws inp),
            (\inp -> matchCp 125 inp)] inp),
        (\inp -> pegSeq [(\inp -> matchCp 123 inp), (\inp -> ws inp), (\inp -> matchCp 125 inp)] inp)] inp

-- [4] MEMBERS 
r_members :: Input -> Result
r_members inp =
    pegSeq [
        (\inp -> r_member inp),
        (\inp -> starP (\inp -> pegSeq [(\inp -> ws inp), (\inp -> matchCp 44 inp), (\inp -> ws inp), (\inp -> r_member inp)] inp) inp)] inp

-- [5] MEMBER 
r_member :: Input -> Result
r_member inp =
    pegSeq [
        (\inp -> ws inp),
        (\inp -> r_string inp),
        (\inp -> ws inp),
        (\inp -> matchCp 58 inp),
        (\inp -> ws inp),
        (\inp -> r_value inp),
        (\inp -> ws inp)] inp

-- [6] ARRAY 
r_array :: Input -> Result
r_array inp =
    pegAlt [
        (\inp -> pegSeq [
            (\inp -> matchCp 91 inp),
            (\inp -> ws inp),
            (\inp -> r_elements inp),
            (\inp -> ws inp),
            (\inp -> matchCp 93 inp)] inp),
        (\inp -> pegSeq [(\inp -> matchCp 91 inp), (\inp -> ws inp), (\inp -> matchCp 93 inp)] inp)] inp

-- [7] ELEMENTS 
r_elements :: Input -> Result
r_elements inp =
    pegSeq [
        (\inp -> r_value inp),
        (\inp -> starP (\inp -> pegSeq [(\inp -> ws inp), (\inp -> matchCp 44 inp), (\inp -> ws inp), (\inp -> r_value inp)] inp) inp)] inp

-- [8] STRING 
r_string :: Input -> Result
r_string inp =
    pegSeq [
        (\inp -> matchCp 34 inp),
        (\inp -> starP (\inp -> r_char inp) inp),
        (\inp -> matchCp 34 inp)] inp

-- [9] CHAR 
r_char :: Input -> Result
r_char inp =
    pegAlt [
        (\inp -> escaped inp),
        (\inp -> pegSeq [
            (\inp -> negP (\inp -> matchCp 34 inp) inp),
            (\inp -> negP (\inp -> matchCp 92 inp) inp),
            (\inp -> negP (\inp -> matchCp 0x0 inp) inp),
            (\inp -> negP (\inp -> matchRange 0x0 0x1F inp) inp),
            (\inp -> matchRange 0x20 0x10FFFF inp)] inp)] inp

-- [10] ESCAPED 
escaped :: Input -> Result
escaped inp =
    pegSeq [
        (\inp -> matchCp 92 inp),
        (\inp -> pegAlt [
            (\inp -> matchCp 34 inp),
            (\inp -> matchCp 92 inp),
            (\inp -> matchCp 47 inp),
            (\inp -> matchCp 98 inp),
            (\inp -> matchCp 102 inp),
            (\inp -> matchCp 110 inp),
            (\inp -> matchCp 114 inp),
            (\inp -> matchCp 116 inp),
            (\inp -> pegSeq [(\inp -> matchCp 117 inp), (\inp -> hex4 inp)] inp)] inp)] inp

-- [11] HEX4 
hex4 :: Input -> Result
hex4 inp =
    pegSeq [(\inp -> hexdig inp), (\inp -> hexdig inp), (\inp -> hexdig inp), (\inp -> hexdig inp)] inp

-- [12] HEXDIG 
hexdig :: Input -> Result
hexdig inp =
    pegAlt [
        (\inp -> matchRange 48 57 inp),
        (\inp -> matchRange 97 102 inp),
        (\inp -> matchRange 65 70 inp)] inp

-- [13] NUMBER 
r_number :: Input -> Result
r_number inp =
    pegSeq [
        (\inp -> optP (\inp -> matchCp 45 inp) inp),
        (\inp -> r_integer inp),
        (\inp -> optP (\inp -> r_fraction inp) inp),
        (\inp -> optP (\inp -> r_exponent inp) inp)] inp

-- [14] INTEGER 
r_integer :: Input -> Result
r_integer inp =
    pegAlt [
        (\inp -> matchCp 48 inp),
        (\inp -> pegSeq [(\inp -> matchRange 49 57 inp), (\inp -> starP (\inp -> matchRange 48 57 inp) inp)] inp)] inp

-- [15] FRACTION 
r_fraction :: Input -> Result
r_fraction inp =
    pegSeq [(\inp -> matchCp 46 inp), (\inp -> plusP (\inp -> matchRange 48 57 inp) inp)] inp

-- [16] EXPONENT 
r_exponent :: Input -> Result
r_exponent inp =
    pegSeq [
        (\inp -> pegAlt [(\inp -> matchCp 101 inp), (\inp -> matchCp 69 inp)] inp),
        (\inp -> optP (\inp -> pegAlt [(\inp -> matchCp 43 inp), (\inp -> matchCp 45 inp)] inp) inp),
        (\inp -> plusP (\inp -> matchRange 48 57 inp) inp)] inp

-- [17] WS 
ws :: Input -> Result
ws inp =
    starP (\inp -> pegAlt [
        (\inp -> matchCp 0x20 inp),
        (\inp -> matchCp 0x9 inp),
        (\inp -> matchCp 0x0A inp),
        (\inp -> matchCp 0x0D inp)] inp) inp

-- ── API ──

printAst :: Ast -> Int -> IO ()
printAst node depth = do
  let indent = replicate (depth * 2) ' '
  case node of
    Leaf t -> putStrLn (indent ++ "SCALAR: \"" ++ t ++ "\"")
    Branch t cs -> do
      putStrLn (indent ++ t)
      mapM_ (\c -> printAst c (depth + 1)) cs

-- ── Main ──

main :: IO ()
main = do
  args <- getArgs
  text <- case args of
    (f:_) -> readFile f
    []    -> hGetContents stdin
  let inp = mkInput text
      r = json_text inp
  if not (failed r) then do
    putStrLn ("OK: " ++ show (pos (rest r)) ++ " chars")
    case ast r of
      Just a  -> printAst a 0
      Nothing -> return ()
    exitSuccess
  else do
    hPutStrLn stderr ("FAIL @" ++ show (pos (rest r)) ++ ": " ++ err r)
    exitWith (ExitFailure 1)
