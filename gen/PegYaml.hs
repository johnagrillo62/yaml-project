-- ════════════════════════════════════════════════════════════════
module Main where

import Data.Char (ord, chr, isDigit, isHexDigit)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.IORef
import System.Environment (getArgs)
import System.IO (hGetContents, stdin)

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

-- ── YAML extensions ──

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
detect_indent n inp = detectIndent n inp

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

detectIndent :: Int -> Input -> Result
detectIndent n inp =
  let s = src inp
      len = length s
      i = pos inp
      sp = length (takeWhile (== ' ') (drop i s))
  in if i + sp < len && s !! (i + sp) /= '\n'
     then (okR inp) { tagInt = max 1 (sp - n) }
     else scanLines s len (i + sp) n
  where
    scanLines s len j n
      | j >= len = (okR inp) { tagInt = 1 }
      | s !! j == '\n' =
          let j' = j + 1
          in if j' >= len then (okR inp) { tagInt = 1 }
             else let sp = length (takeWhile (== ' ') (drop j' s))
                      nx = j' + sp
                  in if nx >= len || s !! nx == '\n'
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
  in if failed r then r else k (tag r) (rest r)

-- ════════════════════════════════════════════════════════════════ 
-- YAML 1.2 Grammar — 211 rules 
-- ════════════════════════════════════════════════════════════════ 

-- [1] C-PRINTABLE 
c_printable :: Input -> Result
c_printable inp =
    pegAlt [
        (\inp -> matchCp 0x9 inp),
        (\inp -> matchCp 0x0A inp),
        (\inp -> matchCp 0x0D inp),
        (\inp -> matchRange 0x20 0x7E inp),
        (\inp -> matchCp 0x85 inp),
        (\inp -> matchRange 0xA0 0xD7FF inp),
        (\inp -> matchRange 0xE000 0xFFFD inp),
        (\inp -> matchRange 0x10000 0x10FFFF inp)] inp

-- [2] NB-JSON 
nb_json :: Input -> Result
nb_json inp =
    pegAlt [(\inp -> matchCp 0x9 inp), (\inp -> matchRange 0x20 0x10FFFF inp)] inp

-- [3] C-BYTE-ORDER-MARK 
c_byte_order_mark :: Input -> Result
c_byte_order_mark inp =
    matchCp 0xFEFF inp

-- [4] C-SEQUENCE-ENTRY 
c_sequence_entry :: Input -> Result
c_sequence_entry inp =
    matchCp 45 inp

-- [5] C-MAPPING-KEY 
c_mapping_key :: Input -> Result
c_mapping_key inp =
    matchCp 63 inp

-- [6] C-MAPPING-VALUE 
c_mapping_value :: Input -> Result
c_mapping_value inp =
    matchCp 58 inp

-- [7] C-COLLECT-ENTRY 
c_collect_entry :: Input -> Result
c_collect_entry inp =
    matchCp 44 inp

-- [8] C-SEQUENCE-START 
c_sequence_start :: Input -> Result
c_sequence_start inp =
    matchCp 91 inp

-- [9] C-SEQUENCE-END 
c_sequence_end :: Input -> Result
c_sequence_end inp =
    matchCp 93 inp

-- [10] C-MAPPING-START 
c_mapping_start :: Input -> Result
c_mapping_start inp =
    matchCp 123 inp

-- [11] C-MAPPING-END 
c_mapping_end :: Input -> Result
c_mapping_end inp =
    matchCp 125 inp

-- [12] C-COMMENT 
c_comment :: Input -> Result
c_comment inp =
    matchCp 35 inp

-- [13] C-ANCHOR 
c_anchor :: Input -> Result
c_anchor inp =
    matchCp 38 inp

-- [14] C-ALIAS 
c_alias :: Input -> Result
c_alias inp =
    matchCp 42 inp

-- [15] C-TAG 
c_tag :: Input -> Result
c_tag inp =
    matchCp 33 inp

-- [16] C-LITERAL 
c_literal :: Input -> Result
c_literal inp =
    matchCp 124 inp

-- [17] C-FOLDED 
c_folded :: Input -> Result
c_folded inp =
    matchCp 62 inp

-- [18] C-SINGLE-QUOTE 
c_single_quote :: Input -> Result
c_single_quote inp =
    matchCp 39 inp

-- [19] C-DOUBLE-QUOTE 
c_double_quote :: Input -> Result
c_double_quote inp =
    matchCp 34 inp

-- [20] C-DIRECTIVE 
c_directive :: Input -> Result
c_directive inp =
    matchCp 37 inp

-- [21] C-RESERVED 
c_reserved :: Input -> Result
c_reserved inp =
    pegAlt [(\inp -> matchCp 64 inp), (\inp -> matchCp 96 inp)] inp

-- [22] C-INDICATOR 
c_indicator :: Input -> Result
c_indicator inp =
    pegAlt [
        (\inp -> c_sequence_entry inp),
        (\inp -> c_mapping_key inp),
        (\inp -> c_mapping_value inp),
        (\inp -> c_collect_entry inp),
        (\inp -> c_sequence_start inp),
        (\inp -> c_sequence_end inp),
        (\inp -> c_mapping_start inp),
        (\inp -> c_mapping_end inp),
        (\inp -> c_comment inp),
        (\inp -> c_anchor inp),
        (\inp -> c_alias inp),
        (\inp -> c_tag inp),
        (\inp -> c_literal inp),
        (\inp -> c_folded inp),
        (\inp -> c_single_quote inp),
        (\inp -> c_double_quote inp),
        (\inp -> c_directive inp),
        (\inp -> c_reserved inp)] inp

-- [23] C-FLOW-INDICATOR 
c_flow_indicator :: Input -> Result
c_flow_indicator inp =
    pegAlt [
        (\inp -> c_collect_entry inp),
        (\inp -> c_sequence_start inp),
        (\inp -> c_sequence_end inp),
        (\inp -> c_mapping_start inp),
        (\inp -> c_mapping_end inp)] inp

-- [24] B-LINE-FEED 
b_line_feed :: Input -> Result
b_line_feed inp =
    matchCp 0x0A inp

-- [25] B-CARRIAGE-RETURN 
b_carriage_return :: Input -> Result
b_carriage_return inp =
    matchCp 0x0D inp

-- [26] B-CHAR 
b_char :: Input -> Result
b_char inp =
    pegAlt [(\inp -> b_line_feed inp), (\inp -> b_carriage_return inp)] inp

-- [27] NB-CHAR 
nb_char :: Input -> Result
nb_char inp =
    minusP (\inp -> c_printable inp) (\inp -> pegAlt [(\inp -> b_char inp), (\inp -> c_byte_order_mark inp)] inp) inp

-- [28] B-BREAK 
b_break :: Input -> Result
b_break inp =
    pegAlt [
        (\inp -> pegSeq [(\inp -> b_carriage_return inp), (\inp -> b_line_feed inp)] inp),
        (\inp -> b_carriage_return inp),
        (\inp -> b_line_feed inp)] inp

-- [29] B-AS-LINE-FEED 
b_as_line_feed :: Input -> Result
b_as_line_feed inp =
    b_break inp

-- [30] B-NON-CONTENT 
b_non_content :: Input -> Result
b_non_content inp =
    b_break inp

-- [31] S-SPACE 
s_space :: Input -> Result
s_space inp =
    matchCp 0x20 inp

-- [32] S-TAB 
s_tab :: Input -> Result
s_tab inp =
    matchCp 0x9 inp

-- [33] S-WHITE 
s_white :: Input -> Result
s_white inp =
    pegAlt [(\inp -> s_space inp), (\inp -> s_tab inp)] inp

-- [34] NS-CHAR 
ns_char :: Input -> Result
ns_char inp =
    minusP (\inp -> nb_char inp) (\inp -> s_white inp) inp

-- [35] NS-DEC-DIGIT 
ns_dec_digit :: Input -> Result
ns_dec_digit inp =
    matchRange 0x30 0x39 inp

-- [36] NS-HEX-DIGIT 
ns_hex_digit :: Input -> Result
ns_hex_digit inp =
    pegAlt [
        (\inp -> ns_dec_digit inp),
        (\inp -> matchRange 0x41 0x46 inp),
        (\inp -> matchRange 0x61 0x66 inp)] inp

-- [37] NS-ASCII-LETTER 
ns_ascii_letter :: Input -> Result
ns_ascii_letter inp =
    pegAlt [(\inp -> matchRange 0x41 0x5A inp), (\inp -> matchRange 0x61 0x7A inp)] inp

-- [38] NS-WORD-CHAR 
ns_word_char :: Input -> Result
ns_word_char inp =
    pegAlt [(\inp -> ns_dec_digit inp), (\inp -> ns_ascii_letter inp), (\inp -> matchCp 45 inp)] inp

-- [39] NS-URI-CHAR 
ns_uri_char :: Input -> Result
ns_uri_char inp =
    pegAlt [
        (\inp -> pegSeq [(\inp -> matchCp 37 inp), (\inp -> ns_hex_digit inp), (\inp -> ns_hex_digit inp)] inp),
        (\inp -> ns_word_char inp),
        (\inp -> matchCp 35 inp),
        (\inp -> matchCp 59 inp),
        (\inp -> matchCp 47 inp),
        (\inp -> matchCp 63 inp),
        (\inp -> matchCp 58 inp),
        (\inp -> matchCp 64 inp),
        (\inp -> matchCp 38 inp),
        (\inp -> matchCp 61 inp),
        (\inp -> matchCp 43 inp),
        (\inp -> matchCp 36 inp),
        (\inp -> matchCp 44 inp),
        (\inp -> matchCp 95 inp),
        (\inp -> matchCp 46 inp),
        (\inp -> matchCp 33 inp),
        (\inp -> matchCp 126 inp),
        (\inp -> matchCp 42 inp),
        (\inp -> matchCp 39 inp),
        (\inp -> matchCp 40 inp),
        (\inp -> matchCp 41 inp),
        (\inp -> matchCp 91 inp),
        (\inp -> matchCp 93 inp)] inp

-- [40] NS-TAG-CHAR 
ns_tag_char :: Input -> Result
ns_tag_char inp =
    minusP (\inp -> ns_uri_char inp) (\inp -> pegAlt [(\inp -> c_tag inp), (\inp -> c_flow_indicator inp)] inp) inp

-- [41] C-ESCAPE 
c_escape :: Input -> Result
c_escape inp =
    matchCp 92 inp

-- [42] NS-ESC-NULL 
ns_esc_null :: Input -> Result
ns_esc_null inp =
    matchCp 48 inp

-- [43] NS-ESC-BELL 
ns_esc_bell :: Input -> Result
ns_esc_bell inp =
    matchCp 97 inp

-- [44] NS-ESC-BACKSPACE 
ns_esc_backspace :: Input -> Result
ns_esc_backspace inp =
    matchCp 98 inp

-- [45] NS-ESC-HORIZONTAL-TAB 
ns_esc_horizontal_tab :: Input -> Result
ns_esc_horizontal_tab inp =
    matchCp 116 inp

-- [46] NS-ESC-LINE-FEED 
ns_esc_line_feed :: Input -> Result
ns_esc_line_feed inp =
    matchCp 110 inp

-- [47] NS-ESC-VERTICAL-TAB 
ns_esc_vertical_tab :: Input -> Result
ns_esc_vertical_tab inp =
    matchCp 118 inp

-- [48] NS-ESC-FORM-FEED 
ns_esc_form_feed :: Input -> Result
ns_esc_form_feed inp =
    matchCp 102 inp

-- [49] NS-ESC-CARRIAGE-RETURN 
ns_esc_carriage_return :: Input -> Result
ns_esc_carriage_return inp =
    matchCp 114 inp

-- [50] NS-ESC-ESCAPE 
ns_esc_escape :: Input -> Result
ns_esc_escape inp =
    matchCp 101 inp

-- [51] NS-ESC-SPACE 
ns_esc_space :: Input -> Result
ns_esc_space inp =
    matchCp 0x20 inp

-- [52] NS-ESC-DOUBLE-QUOTE 
ns_esc_double_quote :: Input -> Result
ns_esc_double_quote inp =
    matchCp 34 inp

-- [53] NS-ESC-SLASH 
ns_esc_slash :: Input -> Result
ns_esc_slash inp =
    matchCp 47 inp

-- [54] NS-ESC-BACKSLASH 
ns_esc_backslash :: Input -> Result
ns_esc_backslash inp =
    matchCp 92 inp

-- [55] NS-ESC-NEXT-LINE 
ns_esc_next_line :: Input -> Result
ns_esc_next_line inp =
    matchCp 78 inp

-- [56] NS-ESC-NON-BREAKING-SPACE 
ns_esc_non_breaking_space :: Input -> Result
ns_esc_non_breaking_space inp =
    matchCp 95 inp

-- [57] NS-ESC-LINE-SEPARATOR 
ns_esc_line_separator :: Input -> Result
ns_esc_line_separator inp =
    matchCp 76 inp

-- [58] NS-ESC-PARAGRAPH-SEPARATOR 
ns_esc_paragraph_separator :: Input -> Result
ns_esc_paragraph_separator inp =
    matchCp 80 inp

-- [59] NS-ESC-8-BIT 
ns_esc_8_bit :: Input -> Result
ns_esc_8_bit inp =
    pegSeq [(\inp -> matchCp 120 inp), (\inp -> repP 2 (\inp -> ns_hex_digit inp) inp)] inp

-- [60] NS-ESC-16-BIT 
ns_esc_16_bit :: Input -> Result
ns_esc_16_bit inp =
    pegSeq [(\inp -> matchCp 117 inp), (\inp -> repP 4 (\inp -> ns_hex_digit inp) inp)] inp

-- [61] NS-ESC-32-BIT 
ns_esc_32_bit :: Input -> Result
ns_esc_32_bit inp =
    pegSeq [(\inp -> matchCp 85 inp), (\inp -> repP 8 (\inp -> ns_hex_digit inp) inp)] inp

-- [62] C-NS-ESC-CHAR 
c_ns_esc_char :: Input -> Result
c_ns_esc_char inp =
    pegSeq [
        (\inp -> c_escape inp),
        (\inp -> pegAlt [
            (\inp -> ns_esc_null inp),
            (\inp -> ns_esc_bell inp),
            (\inp -> ns_esc_backspace inp),
            (\inp -> ns_esc_horizontal_tab inp),
            (\inp -> ns_esc_line_feed inp),
            (\inp -> ns_esc_vertical_tab inp),
            (\inp -> ns_esc_form_feed inp),
            (\inp -> ns_esc_carriage_return inp),
            (\inp -> ns_esc_escape inp),
            (\inp -> ns_esc_space inp),
            (\inp -> ns_esc_double_quote inp),
            (\inp -> ns_esc_slash inp),
            (\inp -> ns_esc_backslash inp),
            (\inp -> ns_esc_next_line inp),
            (\inp -> ns_esc_non_breaking_space inp),
            (\inp -> ns_esc_line_separator inp),
            (\inp -> ns_esc_paragraph_separator inp),
            (\inp -> ns_esc_8_bit inp),
            (\inp -> ns_esc_16_bit inp),
            (\inp -> ns_esc_32_bit inp)] inp)] inp

-- [63] S-INDENT 
s_indent :: Input -> Int -> Result
s_indent inp n =
    repP n (\inp -> s_space inp) inp

-- [64] S-INDENT-LT 
s_indent_lt :: Input -> Int -> Result
s_indent_lt inp n =
    starP (\inp -> s_space inp) inp

-- [65] S-INDENT-LE 
s_indent_le :: Input -> Int -> Result
s_indent_le inp n =
    starP (\inp -> s_space inp) inp

-- [66] S-SEPARATE-IN-LINE 
s_separate_in_line :: Input -> Result
s_separate_in_line inp =
    pegAlt [(\inp -> plusP (\inp -> s_white inp) inp), (\inp -> okR inp)] inp

-- [67] S-LINE-PREFIX 
s_line_prefix :: Input -> Int -> String -> Result
s_line_prefix inp n c =
    (\_ -> if c == "BLOCK-IN" then s_block_line_prefix inp n else if c == "BLOCK-OUT" then s_block_line_prefix inp n else if c == "FLOW-IN" then s_flow_line_prefix inp n else if c == "FLOW-OUT" then s_flow_line_prefix inp n else failR inp "no case") ()

-- [68] S-BLOCK-LINE-PREFIX 
s_block_line_prefix :: Input -> Int -> Result
s_block_line_prefix inp n =
    s_indent inp n

-- [69] S-FLOW-LINE-PREFIX 
s_flow_line_prefix :: Input -> Int -> Result
s_flow_line_prefix inp n =
    pegSeq [(\inp -> s_indent inp n), (\inp -> optP (\inp -> s_separate_in_line inp) inp)] inp

-- [70] L-EMPTY 
l_empty :: Input -> Int -> String -> Result
l_empty inp n c =
    pegSeq [
        (\inp -> pegAlt [(\inp -> s_line_prefix inp n c), (\inp -> s_indent_lt inp n)] inp),
        (\inp -> b_as_line_feed inp)] inp

-- [71] B-L-TRIMMED 
b_l_trimmed :: Input -> Int -> String -> Result
b_l_trimmed inp n c =
    pegSeq [(\inp -> b_non_content inp), (\inp -> plusP (\inp -> l_empty inp n c) inp)] inp

-- [72] B-AS-SPACE 
b_as_space :: Input -> Result
b_as_space inp =
    b_break inp

-- [73] B-L-FOLDED 
b_l_folded :: Input -> Int -> String -> Result
b_l_folded inp n c =
    pegAlt [(\inp -> b_l_trimmed inp n c), (\inp -> b_as_space inp)] inp

-- [74] S-FLOW-FOLDED 
s_flow_folded :: Input -> Int -> Result
s_flow_folded inp n =
    pegSeq [
        (\inp -> optP (\inp -> s_separate_in_line inp) inp),
        (\inp -> b_l_folded inp n "FLOW-IN"),
        (\inp -> s_flow_line_prefix inp n)] inp

-- [75] C-NB-COMMENT-TEXT 
c_nb_comment_text :: Input -> Result
c_nb_comment_text inp =
    pegSeq [(\inp -> c_comment inp), (\inp -> starP (\inp -> nb_char inp) inp)] inp

-- [76] B-COMMENT 
b_comment :: Input -> Result
b_comment inp =
    pegAlt [(\inp -> b_non_content inp), (\inp -> okR inp)] inp

-- [77] S-B-COMMENT 
s_b_comment :: Input -> Result
s_b_comment inp =
    pegSeq [
        (\inp -> optP (\inp -> pegSeq [(\inp -> s_separate_in_line inp), (\inp -> optP (\inp -> c_nb_comment_text inp) inp)] inp) inp),
        (\inp -> b_comment inp)] inp

-- [78] L-COMMENT 
l_comment :: Input -> Result
l_comment inp =
    pegSeq [
        (\inp -> s_separate_in_line inp),
        (\inp -> optP (\inp -> c_nb_comment_text inp) inp),
        (\inp -> b_non_content inp)] inp

-- [79] S-L-COMMENTS 
s_l_comments :: Input -> Result
s_l_comments inp =
    pegSeq [
        (\inp -> pegAlt [(\inp -> s_b_comment inp), (\inp -> okR inp)] inp),
        (\inp -> starP (\inp -> l_comment inp) inp)] inp

-- [80] S-SEPARATE 
s_separate :: Input -> Int -> String -> Result
s_separate inp n c =
    (\_ -> if c == "BLOCK-OUT" then s_separate_lines inp n else if c == "BLOCK-IN" then s_separate_lines inp n else if c == "FLOW-OUT" then s_separate_lines inp n else if c == "FLOW-IN" then s_separate_lines inp n else if c == "BLOCK-KEY" then s_separate_in_line inp else if c == "FLOW-KEY" then s_separate_in_line inp else failR inp "no case") ()

-- [81] S-SEPARATE-LINES 
s_separate_lines :: Input -> Int -> Result
s_separate_lines inp n =
    pegAlt [
        (\inp -> pegSeq [(\inp -> s_l_comments inp), (\inp -> s_flow_line_prefix inp n)] inp),
        (\inp -> s_separate_in_line inp)] inp

-- [82] L-DIRECTIVE 
l_directive :: Input -> Result
l_directive inp =
    pegSeq [
        (\inp -> c_directive inp),
        (\inp -> pegAlt [
            (\inp -> ns_yaml_directive inp),
            (\inp -> ns_tag_directive inp),
            (\inp -> ns_reserved_directive inp)] inp),
        (\inp -> s_l_comments inp)] inp

-- [83] NS-RESERVED-DIRECTIVE 
ns_reserved_directive :: Input -> Result
ns_reserved_directive inp =
    pegSeq [
        (\inp -> ns_directive_name inp),
        (\inp -> starP (\inp -> pegSeq [(\inp -> s_separate_in_line inp), (\inp -> ns_directive_parameter inp)] inp) inp)] inp

-- [84] NS-DIRECTIVE-NAME 
ns_directive_name :: Input -> Result
ns_directive_name inp =
    plusP (\inp -> ns_char inp) inp

-- [85] NS-DIRECTIVE-PARAMETER 
ns_directive_parameter :: Input -> Result
ns_directive_parameter inp =
    plusP (\inp -> ns_char inp) inp

-- [86] NS-YAML-DIRECTIVE 
ns_yaml_directive :: Input -> Result
ns_yaml_directive inp =
    pegSeq [
        (\inp -> matchStr "YAML" inp),
        (\inp -> s_separate_in_line inp),
        (\inp -> ns_yaml_version inp)] inp

-- [87] NS-YAML-VERSION 
ns_yaml_version :: Input -> Result
ns_yaml_version inp =
    pegSeq [
        (\inp -> plusP (\inp -> ns_dec_digit inp) inp),
        (\inp -> matchCp 46 inp),
        (\inp -> plusP (\inp -> ns_dec_digit inp) inp)] inp

-- [88] NS-TAG-DIRECTIVE 
ns_tag_directive :: Input -> Result
ns_tag_directive inp =
    pegSeq [
        (\inp -> matchStr "TAG" inp),
        (\inp -> s_separate_in_line inp),
        (\inp -> c_tag_handle inp),
        (\inp -> s_separate_in_line inp),
        (\inp -> ns_tag_prefix inp)] inp

-- [89] C-TAG-HANDLE 
c_tag_handle :: Input -> Result
c_tag_handle inp =
    pegAlt [
        (\inp -> c_named_tag_handle inp),
        (\inp -> c_secondary_tag_handle inp),
        (\inp -> c_primary_tag_handle inp)] inp

-- [90] C-PRIMARY-TAG-HANDLE 
c_primary_tag_handle :: Input -> Result
c_primary_tag_handle inp =
    matchCp 33 inp

-- [91] C-SECONDARY-TAG-HANDLE 
c_secondary_tag_handle :: Input -> Result
c_secondary_tag_handle inp =
    matchStr "!!" inp

-- [92] C-NAMED-TAG-HANDLE 
c_named_tag_handle :: Input -> Result
c_named_tag_handle inp =
    pegSeq [
        (\inp -> matchCp 33 inp),
        (\inp -> plusP (\inp -> ns_word_char inp) inp),
        (\inp -> matchCp 33 inp)] inp

-- [93] NS-TAG-PREFIX 
ns_tag_prefix :: Input -> Result
ns_tag_prefix inp =
    pegAlt [(\inp -> c_ns_local_tag_prefix inp), (\inp -> ns_global_tag_prefix inp)] inp

-- [94] C-NS-LOCAL-TAG-PREFIX 
c_ns_local_tag_prefix :: Input -> Result
c_ns_local_tag_prefix inp =
    pegSeq [(\inp -> matchCp 33 inp), (\inp -> starP (\inp -> ns_uri_char inp) inp)] inp

-- [95] NS-GLOBAL-TAG-PREFIX 
ns_global_tag_prefix :: Input -> Result
ns_global_tag_prefix inp =
    pegSeq [(\inp -> ns_tag_char inp), (\inp -> starP (\inp -> ns_uri_char inp) inp)] inp

-- [96] C-NS-PROPERTIES 
c_ns_properties :: Input -> Int -> String -> Result
c_ns_properties inp n c =
    pegAlt [
        (\inp -> pegSeq [
            (\inp -> c_ns_tag_property inp),
            (\inp -> optP (\inp -> pegSeq [(\inp -> s_separate inp n c), (\inp -> c_ns_anchor_property inp)] inp) inp)] inp),
        (\inp -> pegSeq [
            (\inp -> c_ns_anchor_property inp),
            (\inp -> optP (\inp -> pegSeq [(\inp -> s_separate inp n c), (\inp -> c_ns_tag_property inp)] inp) inp)] inp)] inp

-- [97] C-NS-TAG-PROPERTY 
c_ns_tag_property :: Input -> Result
c_ns_tag_property inp =
    pegAlt [
        (\inp -> c_verbatim_tag inp),
        (\inp -> c_ns_shorthand_tag inp),
        (\inp -> c_non_specific_tag inp)] inp

-- [98] C-VERBATIM-TAG 
c_verbatim_tag :: Input -> Result
c_verbatim_tag inp =
    pegSeq [
        (\inp -> matchStr "!<" inp),
        (\inp -> plusP (\inp -> ns_uri_char inp) inp),
        (\inp -> matchCp 62 inp)] inp

-- [99] C-NS-SHORTHAND-TAG 
c_ns_shorthand_tag :: Input -> Result
c_ns_shorthand_tag inp =
    pegSeq [(\inp -> c_tag_handle inp), (\inp -> plusP (\inp -> ns_tag_char inp) inp)] inp

-- [100] C-NON-SPECIFIC-TAG 
c_non_specific_tag :: Input -> Result
c_non_specific_tag inp =
    matchCp 33 inp

-- [101] C-NS-ANCHOR-PROPERTY 
c_ns_anchor_property :: Input -> Result
c_ns_anchor_property inp =
    buildP "ANCHOR" (\inp -> pegSeq [(\inp -> c_anchor inp), (\inp -> scalarP (\inp -> ns_anchor_name inp) inp)] inp) inp

-- [102] NS-ANCHOR-CHAR 
ns_anchor_char :: Input -> Result
ns_anchor_char inp =
    minusP (\inp -> ns_char inp) (\inp -> c_flow_indicator inp) inp

-- [103] NS-ANCHOR-NAME 
ns_anchor_name :: Input -> Result
ns_anchor_name inp =
    plusP (\inp -> ns_anchor_char inp) inp

-- [104] C-NS-ALIAS-NODE 
c_ns_alias_node :: Input -> Result
c_ns_alias_node inp =
    buildP "ALIAS" (\inp -> pegSeq [(\inp -> c_alias inp), (\inp -> scalarP (\inp -> ns_anchor_name inp) inp)] inp) inp

-- [105] E-SCALAR 
e_scalar :: Input -> Result
e_scalar inp =
    okR inp

-- [106] E-NODE 
e_node :: Input -> Result
e_node inp =
    e_scalar inp

-- [107] NB-DOUBLE-CHAR 
nb_double_char :: Input -> Result
nb_double_char inp =
    pegAlt [
        (\inp -> c_ns_esc_char inp),
        (\inp -> minusP (\inp -> nb_json inp) (\inp -> pegAlt [(\inp -> matchCp 92 inp), (\inp -> matchCp 34 inp)] inp) inp)] inp

-- [108] NS-DOUBLE-CHAR 
ns_double_char :: Input -> Result
ns_double_char inp =
    minusP (\inp -> nb_double_char inp) (\inp -> s_white inp) inp

-- [109] C-DOUBLE-QUOTED 
c_double_quoted :: Input -> Int -> String -> Result
c_double_quoted inp n c =
    scalarP (\inp -> pegSeq [(\inp -> matchCp 34 inp), (\inp -> nb_double_text inp n c), (\inp -> matchCp 34 inp)] inp) inp

-- [110] NB-DOUBLE-TEXT 
nb_double_text :: Input -> Int -> String -> Result
nb_double_text inp n c =
    (\_ -> if c == "FLOW-OUT" then nb_double_multi_line inp n else if c == "FLOW-IN" then nb_double_multi_line inp n else if c == "BLOCK-KEY" then nb_double_one_line inp else if c == "FLOW-KEY" then nb_double_one_line inp else failR inp "no case") ()

-- [111] NB-DOUBLE-ONE-LINE 
nb_double_one_line :: Input -> Result
nb_double_one_line inp =
    starP (\inp -> nb_double_char inp) inp

-- [112] S-DOUBLE-ESCAPED 
s_double_escaped :: Input -> Int -> Result
s_double_escaped inp n =
    pegSeq [
        (\inp -> starP (\inp -> s_white inp) inp),
        (\inp -> matchCp 92 inp),
        (\inp -> b_non_content inp),
        (\inp -> starP (\inp -> l_empty inp n "FLOW-IN") inp),
        (\inp -> s_flow_line_prefix inp n)] inp

-- [113] S-DOUBLE-BREAK 
s_double_break :: Input -> Int -> Result
s_double_break inp n =
    pegAlt [(\inp -> s_double_escaped inp n), (\inp -> s_flow_folded inp n)] inp

-- [114] NB-NS-DOUBLE-IN-LINE 
nb_ns_double_in_line :: Input -> Result
nb_ns_double_in_line inp =
    starP (\inp -> pegSeq [(\inp -> starP (\inp -> s_white inp) inp), (\inp -> ns_double_char inp)] inp) inp

-- [115] S-DOUBLE-NEXT-LINE 
s_double_next_line :: Input -> Int -> Result
s_double_next_line inp n =
    pegSeq [
        (\inp -> s_double_break inp n),
        (\inp -> optP (\inp -> pegSeq [
            (\inp -> ns_double_char inp),
            (\inp -> nb_ns_double_in_line inp),
            (\inp -> pegAlt [(\inp -> s_double_next_line inp n), (\inp -> starP (\inp -> s_white inp) inp)] inp)] inp) inp)] inp

-- [116] NB-DOUBLE-MULTI-LINE 
nb_double_multi_line :: Input -> Int -> Result
nb_double_multi_line inp n =
    pegSeq [
        (\inp -> nb_ns_double_in_line inp),
        (\inp -> pegAlt [(\inp -> s_double_next_line inp n), (\inp -> starP (\inp -> s_white inp) inp)] inp)] inp

-- [117] C-QUOTED-QUOTE 
c_quoted_quote :: Input -> Result
c_quoted_quote inp =
    matchStr "''" inp

-- [118] NB-SINGLE-CHAR 
nb_single_char :: Input -> Result
nb_single_char inp =
    pegAlt [
        (\inp -> c_quoted_quote inp),
        (\inp -> minusP (\inp -> nb_json inp) (\inp -> matchCp 39 inp) inp)] inp

-- [119] NS-SINGLE-CHAR 
ns_single_char :: Input -> Result
ns_single_char inp =
    minusP (\inp -> nb_single_char inp) (\inp -> s_white inp) inp

-- [120] C-SINGLE-QUOTED 
c_single_quoted :: Input -> Int -> String -> Result
c_single_quoted inp n c =
    scalarP (\inp -> pegSeq [(\inp -> matchCp 39 inp), (\inp -> nb_single_text inp n c), (\inp -> matchCp 39 inp)] inp) inp

-- [121] NB-SINGLE-TEXT 
nb_single_text :: Input -> Int -> String -> Result
nb_single_text inp n c =
    (\_ -> if c == "FLOW-OUT" then nb_single_multi_line inp n else if c == "FLOW-IN" then nb_single_multi_line inp n else if c == "BLOCK-KEY" then nb_single_one_line inp else if c == "FLOW-KEY" then nb_single_one_line inp else failR inp "no case") ()

-- [122] NB-SINGLE-ONE-LINE 
nb_single_one_line :: Input -> Result
nb_single_one_line inp =
    starP (\inp -> nb_single_char inp) inp

-- [123] NS-SINGLE-IN-LINE 
ns_single_in_line :: Input -> Result
ns_single_in_line inp =
    starP (\inp -> pegSeq [(\inp -> starP (\inp -> s_white inp) inp), (\inp -> ns_single_char inp)] inp) inp

-- [124] S-SINGLE-NEXT-LINE 
s_single_next_line :: Input -> Int -> Result
s_single_next_line inp n =
    pegSeq [
        (\inp -> s_flow_folded inp n),
        (\inp -> optP (\inp -> pegSeq [
            (\inp -> ns_single_char inp),
            (\inp -> ns_single_in_line inp),
            (\inp -> pegAlt [(\inp -> s_single_next_line inp n), (\inp -> starP (\inp -> s_white inp) inp)] inp)] inp) inp)] inp

-- [125] NB-SINGLE-MULTI-LINE 
nb_single_multi_line :: Input -> Int -> Result
nb_single_multi_line inp n =
    pegSeq [
        (\inp -> ns_single_in_line inp),
        (\inp -> pegAlt [(\inp -> s_single_next_line inp n), (\inp -> starP (\inp -> s_white inp) inp)] inp)] inp

-- [126] NS-PLAIN-FIRST 
ns_plain_first :: Input -> String -> Result
ns_plain_first inp c =
    pegAlt [
        (\inp -> minusP (\inp -> ns_char inp) (\inp -> c_indicator inp) inp),
        (\inp -> pegSeq [
            (\inp -> pegAlt [(\inp -> matchCp 63 inp), (\inp -> matchCp 58 inp), (\inp -> matchCp 45 inp)] inp),
            (\inp -> aheadP (\inp -> ns_plain_safe inp c) inp)] inp)] inp

-- [127] NS-PLAIN-SAFE 
ns_plain_safe :: Input -> String -> Result
ns_plain_safe inp c =
    (\_ -> if c == "FLOW-OUT" then ns_plain_safe_out inp else if c == "FLOW-IN" then ns_plain_safe_in inp else if c == "BLOCK-KEY" then ns_plain_safe_out inp else if c == "FLOW-KEY" then ns_plain_safe_in inp else failR inp "no case") ()

-- [128] NS-PLAIN-SAFE-OUT 
ns_plain_safe_out :: Input -> Result
ns_plain_safe_out inp =
    ns_char inp

-- [129] NS-PLAIN-SAFE-IN 
ns_plain_safe_in :: Input -> Result
ns_plain_safe_in inp =
    minusP (\inp -> ns_char inp) (\inp -> c_flow_indicator inp) inp

-- [130] NS-PLAIN-CHAR 
ns_plain_char :: Input -> String -> Result
ns_plain_char inp c =
    pegAlt [
        (\inp -> minusP (\inp -> ns_plain_safe inp c) (\inp -> pegAlt [(\inp -> matchCp 58 inp), (\inp -> matchCp 35 inp)] inp) inp),
        (\inp -> pegSeq [(\inp -> behindP (\inp -> ns_char inp) inp), (\inp -> matchCp 35 inp)] inp),
        (\inp -> pegSeq [(\inp -> matchCp 58 inp), (\inp -> aheadP (\inp -> ns_plain_safe inp c) inp)] inp)] inp

-- [131] NS-PLAIN 
ns_plain :: Input -> Int -> String -> Result
ns_plain inp n c =
    scalarP (\inp -> (\_ -> if c == "FLOW-OUT" then ns_plain_multi_line inp n c else if c == "FLOW-IN" then ns_plain_multi_line inp n c else if c == "BLOCK-KEY" then ns_plain_one_line inp c else if c == "FLOW-KEY" then ns_plain_one_line inp c else failR inp "no case") ()) inp

-- [132] NB-NS-PLAIN-IN-LINE 
nb_ns_plain_in_line :: Input -> String -> Result
nb_ns_plain_in_line inp c =
    starP (\inp -> pegSeq [(\inp -> starP (\inp -> s_white inp) inp), (\inp -> ns_plain_char inp c)] inp) inp

-- [133] NS-PLAIN-ONE-LINE 
ns_plain_one_line :: Input -> String -> Result
ns_plain_one_line inp c =
    pegSeq [(\inp -> ns_plain_first inp c), (\inp -> nb_ns_plain_in_line inp c)] inp

-- [134] S-NS-PLAIN-NEXT-LINE 
s_ns_plain_next_line :: Input -> Int -> String -> Result
s_ns_plain_next_line inp n c =
    pegSeq [
        (\inp -> s_flow_folded inp n),
        (\inp -> negP (\inp -> c_forbidden inp) inp),
        (\inp -> ns_plain_char inp c),
        (\inp -> nb_ns_plain_in_line inp c)] inp

-- [135] NS-PLAIN-MULTI-LINE 
ns_plain_multi_line :: Input -> Int -> String -> Result
ns_plain_multi_line inp n c =
    pegSeq [
        (\inp -> ns_plain_one_line inp c),
        (\inp -> starP (\inp -> s_ns_plain_next_line inp n c) inp)] inp

-- [137] C-FLOW-SEQUENCE 
c_flow_sequence :: Input -> Int -> String -> Result
c_flow_sequence inp n c =
    buildP "SEQUENCE" (\inp -> pegSeq [
        (\inp -> matchCp 91 inp),
        (\inp -> optP (\inp -> s_separate inp n c) inp),
        (\inp -> optP (\inp -> collectP (\inp -> ns_s_flow_seq_entries inp n (inFlow c)) inp) inp),
        (\inp -> matchCp 93 inp)] inp) inp

-- [138] NS-S-FLOW-SEQ-ENTRIES 
ns_s_flow_seq_entries :: Input -> Int -> String -> Result
ns_s_flow_seq_entries inp n c =
    pegSeq [
        (\inp -> ns_flow_seq_entry inp n c),
        (\inp -> optP (\inp -> s_separate inp n c) inp),
        (\inp -> optP (\inp -> pegSeq [
            (\inp -> matchCp 44 inp),
            (\inp -> optP (\inp -> s_separate inp n c) inp),
            (\inp -> optP (\inp -> ns_s_flow_seq_entries inp n c) inp)] inp) inp)] inp

-- [139] NS-FLOW-SEQ-ENTRY 
ns_flow_seq_entry :: Input -> Int -> String -> Result
ns_flow_seq_entry inp n c =
    pegAlt [(\inp -> ns_flow_pair inp n c), (\inp -> ns_flow_node inp n c)] inp

-- [140] C-FLOW-MAPPING 
c_flow_mapping :: Input -> Int -> String -> Result
c_flow_mapping inp n c =
    buildP "MAPPING" (\inp -> pegSeq [
        (\inp -> matchCp 123 inp),
        (\inp -> optP (\inp -> s_separate inp n c) inp),
        (\inp -> optP (\inp -> collectP (\inp -> ns_s_flow_map_entries inp n (inFlow c)) inp) inp),
        (\inp -> matchCp 125 inp)] inp) inp

-- [141] NS-S-FLOW-MAP-ENTRIES 
ns_s_flow_map_entries :: Input -> Int -> String -> Result
ns_s_flow_map_entries inp n c =
    pegSeq [
        (\inp -> ns_flow_map_entry inp n c),
        (\inp -> optP (\inp -> s_separate inp n c) inp),
        (\inp -> optP (\inp -> pegSeq [
            (\inp -> matchCp 44 inp),
            (\inp -> optP (\inp -> s_separate inp n c) inp),
            (\inp -> optP (\inp -> ns_s_flow_map_entries inp n c) inp)] inp) inp)] inp

-- [142] NS-FLOW-MAP-ENTRY 
ns_flow_map_entry :: Input -> Int -> String -> Result
ns_flow_map_entry inp n c =
    pegAlt [
        (\inp -> pegSeq [
            (\inp -> matchCp 63 inp),
            (\inp -> s_separate inp n c),
            (\inp -> ns_flow_map_explicit_entry inp n c)] inp),
        (\inp -> ns_flow_map_implicit_entry inp n c)] inp

-- [143] NS-FLOW-MAP-EXPLICIT-ENTRY 
ns_flow_map_explicit_entry :: Input -> Int -> String -> Result
ns_flow_map_explicit_entry inp n c =
    pegAlt [
        (\inp -> ns_flow_map_implicit_entry inp n c),
        (\inp -> pegSeq [(\inp -> e_node inp), (\inp -> e_node inp)] inp)] inp

-- [144] NS-FLOW-MAP-IMPLICIT-ENTRY 
ns_flow_map_implicit_entry :: Input -> Int -> String -> Result
ns_flow_map_implicit_entry inp n c =
    buildP "PAIR" (\inp -> pegAlt [
        (\inp -> ns_flow_map_yaml_key_entry inp n c),
        (\inp -> c_ns_flow_map_empty_key_entry inp n c),
        (\inp -> c_ns_flow_map_json_key_entry inp n c)] inp) inp

-- [145] NS-FLOW-MAP-YAML-KEY-ENTRY 
ns_flow_map_yaml_key_entry :: Input -> Int -> String -> Result
ns_flow_map_yaml_key_entry inp n c =
    pegSeq [
        (\inp -> ns_flow_yaml_node inp n c),
        (\inp -> pegAlt [
            (\inp -> pegSeq [
                (\inp -> optP (\inp -> s_separate inp n c) inp),
                (\inp -> c_ns_flow_map_separate_value inp n c)] inp),
            (\inp -> e_node inp)] inp)] inp

-- [146] C-NS-FLOW-MAP-EMPTY-KEY-ENTRY 
c_ns_flow_map_empty_key_entry :: Input -> Int -> String -> Result
c_ns_flow_map_empty_key_entry inp n c =
    pegSeq [(\inp -> e_node inp), (\inp -> c_ns_flow_map_separate_value inp n c)] inp

-- [147] C-NS-FLOW-MAP-SEPARATE-VALUE 
c_ns_flow_map_separate_value :: Input -> Int -> String -> Result
c_ns_flow_map_separate_value inp n c =
    pegSeq [
        (\inp -> matchCp 58 inp),
        (\inp -> negP (\inp -> ns_plain_safe inp c) inp),
        (\inp -> pegAlt [
            (\inp -> pegSeq [(\inp -> s_separate inp n c), (\inp -> ns_flow_node inp n c)] inp),
            (\inp -> e_node inp)] inp)] inp

-- [148] C-NS-FLOW-MAP-JSON-KEY-ENTRY 
c_ns_flow_map_json_key_entry :: Input -> Int -> String -> Result
c_ns_flow_map_json_key_entry inp n c =
    pegSeq [
        (\inp -> c_flow_json_node inp n c),
        (\inp -> pegAlt [
            (\inp -> pegSeq [
                (\inp -> optP (\inp -> s_separate inp n c) inp),
                (\inp -> c_ns_flow_map_adjacent_value inp n c)] inp),
            (\inp -> e_node inp)] inp)] inp

-- [149] C-NS-FLOW-MAP-ADJACENT-VALUE 
c_ns_flow_map_adjacent_value :: Input -> Int -> String -> Result
c_ns_flow_map_adjacent_value inp n c =
    pegSeq [
        (\inp -> matchCp 58 inp),
        (\inp -> pegAlt [
            (\inp -> pegSeq [(\inp -> optP (\inp -> s_separate inp n c) inp), (\inp -> ns_flow_node inp n c)] inp),
            (\inp -> e_node inp)] inp)] inp

-- [150] NS-FLOW-PAIR 
ns_flow_pair :: Input -> Int -> String -> Result
ns_flow_pair inp n c =
    pegAlt [
        (\inp -> pegSeq [
            (\inp -> matchCp 63 inp),
            (\inp -> s_separate inp n c),
            (\inp -> ns_flow_map_explicit_entry inp n c)] inp),
        (\inp -> ns_flow_pair_entry inp n c)] inp

-- [151] NS-FLOW-PAIR-ENTRY 
ns_flow_pair_entry :: Input -> Int -> String -> Result
ns_flow_pair_entry inp n c =
    pegAlt [
        (\inp -> ns_flow_pair_yaml_key_entry inp n c),
        (\inp -> c_ns_flow_map_empty_key_entry inp n c),
        (\inp -> c_ns_flow_pair_json_key_entry inp n c)] inp

-- [152] NS-FLOW-PAIR-YAML-KEY-ENTRY 
ns_flow_pair_yaml_key_entry :: Input -> Int -> String -> Result
ns_flow_pair_yaml_key_entry inp n c =
    pegSeq [
        (\inp -> ns_s_implicit_yaml_key inp "FLOW-KEY"),
        (\inp -> c_ns_flow_map_separate_value inp n c)] inp

-- [153] C-NS-FLOW-PAIR-JSON-KEY-ENTRY 
c_ns_flow_pair_json_key_entry :: Input -> Int -> String -> Result
c_ns_flow_pair_json_key_entry inp n c =
    pegSeq [
        (\inp -> c_s_implicit_json_key inp "FLOW-KEY"),
        (\inp -> c_ns_flow_map_adjacent_value inp n c)] inp

-- [154] NS-S-IMPLICIT-YAML-KEY 
ns_s_implicit_yaml_key :: Input -> String -> Result
ns_s_implicit_yaml_key inp c =
    pegSeq [
        (\inp -> ns_flow_yaml_node inp 0 c),
        (\inp -> optP (\inp -> s_separate_in_line inp) inp)] inp

-- [155] C-S-IMPLICIT-JSON-KEY 
c_s_implicit_json_key :: Input -> String -> Result
c_s_implicit_json_key inp c =
    pegSeq [(\inp -> c_flow_json_node inp 0 c), (\inp -> optP (\inp -> s_separate_in_line inp) inp)] inp

-- [156] NS-FLOW-YAML-CONTENT 
ns_flow_yaml_content :: Input -> Int -> String -> Result
ns_flow_yaml_content inp n c =
    ns_plain inp n c

-- [157] C-FLOW-JSON-CONTENT 
c_flow_json_content :: Input -> Int -> String -> Result
c_flow_json_content inp n c =
    pegAlt [
        (\inp -> c_flow_sequence inp n c),
        (\inp -> c_flow_mapping inp n c),
        (\inp -> c_single_quoted inp n c),
        (\inp -> c_double_quoted inp n c)] inp

-- [158] NS-FLOW-CONTENT 
ns_flow_content :: Input -> Int -> String -> Result
ns_flow_content inp n c =
    pegAlt [(\inp -> ns_flow_yaml_content inp n c), (\inp -> c_flow_json_content inp n c)] inp

-- [159] NS-FLOW-YAML-NODE 
ns_flow_yaml_node :: Input -> Int -> String -> Result
ns_flow_yaml_node inp n c =
    pegAlt [
        (\inp -> c_ns_alias_node inp),
        (\inp -> ns_flow_yaml_content inp n c),
        (\inp -> pegSeq [
            (\inp -> c_ns_properties inp n c),
            (\inp -> pegAlt [
                (\inp -> pegSeq [(\inp -> s_separate inp n c), (\inp -> ns_flow_yaml_content inp n c)] inp),
                (\inp -> e_scalar inp)] inp)] inp)] inp

-- [160] C-FLOW-JSON-NODE 
c_flow_json_node :: Input -> Int -> String -> Result
c_flow_json_node inp n c =
    pegSeq [
        (\inp -> optP (\inp -> pegSeq [(\inp -> c_ns_properties inp n c), (\inp -> s_separate inp n c)] inp) inp),
        (\inp -> c_flow_json_content inp n c)] inp

-- [161] NS-FLOW-NODE 
ns_flow_node :: Input -> Int -> String -> Result
ns_flow_node inp n c =
    pegAlt [
        (\inp -> c_ns_alias_node inp),
        (\inp -> ns_flow_content inp n c),
        (\inp -> pegSeq [
            (\inp -> c_ns_properties inp n c),
            (\inp -> pegAlt [
                (\inp -> pegSeq [(\inp -> s_separate inp n c), (\inp -> ns_flow_content inp n c)] inp),
                (\inp -> e_scalar inp)] inp)] inp)] inp

-- [162] C-B-BLOCK-HEADER 
c_b_block_header :: Input -> Int -> Result
c_b_block_header inp n =
    pegAlt [
        (\inp -> bindInt (\m inp -> bindCtx (\t inp -> s_b_comment inp) (\inp -> pegAlt [
            (\inp -> parseSymP (\inp -> matchCp 45 inp) "STRIP" inp),
            (\inp -> parseSymP (\inp -> matchCp 43 inp) "KEEP" inp),
            (\inp -> valP "CLIP" inp)] inp) inp) (\inp -> pegAlt [(\inp -> parseIntP (\inp -> ns_dec_digit inp) inp), (\inp -> detectIndent n inp)] inp) inp),
        (\inp -> bindCtx (\t inp -> bindInt (\m inp -> s_b_comment inp) (\inp -> pegAlt [(\inp -> parseIntP (\inp -> ns_dec_digit inp) inp), (\inp -> detectIndent n inp)] inp) inp) (\inp -> pegAlt [
            (\inp -> parseSymP (\inp -> matchCp 45 inp) "STRIP" inp),
            (\inp -> parseSymP (\inp -> matchCp 43 inp) "KEEP" inp),
            (\inp -> valP "CLIP" inp)] inp) inp)] inp

-- [163] C-INDENTATION-INDICATOR 
c_indentation_indicator :: Input -> Int -> Result
c_indentation_indicator inp n =
    pegAlt [(\inp -> ns_dec_digit inp), (\inp -> okR inp)] inp

-- [164] C-CHOMPING-INDICATOR 
c_chomping_indicator :: Input -> Result
c_chomping_indicator inp =
    pegAlt [(\inp -> matchCp 45 inp), (\inp -> matchCp 43 inp), (\inp -> okR inp)] inp

-- [165] B-CHOMPED-LAST 
b_chomped_last :: Input -> String -> Result
b_chomped_last inp t =
    (\_ -> if t == "STRIP" then b_non_content inp else if t == "CLIP" then b_as_line_feed inp else if t == "KEEP" then b_as_line_feed inp else failR inp "no case") ()

-- [166] L-CHOMPED-EMPTY 
l_chomped_empty :: Input -> Int -> String -> Result
l_chomped_empty inp n t =
    (\_ -> if t == "STRIP" then l_strip_empty inp n else if t == "CLIP" then l_strip_empty inp n else if t == "KEEP" then l_keep_empty inp n else failR inp "no case") ()

-- [167] L-STRIP-EMPTY 
l_strip_empty :: Input -> Int -> Result
l_strip_empty inp n =
    pegSeq [
        (\inp -> starP (\inp -> pegSeq [(\inp -> s_indent_le inp n), (\inp -> b_non_content inp)] inp) inp),
        (\inp -> optP (\inp -> l_trail_comments inp n) inp)] inp

-- [168] L-KEEP-EMPTY 
l_keep_empty :: Input -> Int -> Result
l_keep_empty inp n =
    pegSeq [
        (\inp -> starP (\inp -> l_empty inp n "BLOCK-IN") inp),
        (\inp -> optP (\inp -> l_trail_comments inp n) inp)] inp

-- [169] L-TRAIL-COMMENTS 
l_trail_comments :: Input -> Int -> Result
l_trail_comments inp n =
    pegSeq [
        (\inp -> s_indent_lt inp n),
        (\inp -> c_nb_comment_text inp),
        (\inp -> b_comment inp),
        (\inp -> starP (\inp -> l_comment inp) inp)] inp

-- [170] C-L+LITERAL 
c_lliteral :: Input -> Int -> Result
c_lliteral inp n =
    pegSeq [
        (\inp -> matchCp 124 inp),
        (\inp -> bindInt (\m inp -> bindCtx (\t inp -> pegSeq [(\inp -> s_b_comment inp), (\inp -> l_literal_content inp (n + m) t)] inp) (\inp -> pegAlt [
            (\inp -> parseSymP (\inp -> matchCp 45 inp) "STRIP" inp),
            (\inp -> parseSymP (\inp -> matchCp 43 inp) "KEEP" inp),
            (\inp -> valP "CLIP" inp)] inp) inp) (\inp -> pegAlt [(\inp -> parseIntP (\inp -> ns_dec_digit inp) inp), (\inp -> detectIndent n inp)] inp) inp)] inp

-- [171] L-NB-LITERAL-TEXT 
l_nb_literal_text :: Input -> Int -> Result
l_nb_literal_text inp n =
    pegSeq [
        (\inp -> starP (\inp -> l_empty inp n "BLOCK-IN") inp),
        (\inp -> s_indent inp n),
        (\inp -> plusP (\inp -> nb_char inp) inp)] inp

-- [172] B-NB-LITERAL-NEXT 
b_nb_literal_next :: Input -> Int -> Result
b_nb_literal_next inp n =
    pegSeq [(\inp -> b_as_line_feed inp), (\inp -> l_nb_literal_text inp n)] inp

-- [173] L-LITERAL-CONTENT 
l_literal_content :: Input -> Int -> String -> Result
l_literal_content inp n t =
    scalarP (\inp -> pegSeq [
        (\inp -> optP (\inp -> pegSeq [
            (\inp -> l_nb_literal_text inp n),
            (\inp -> starP (\inp -> b_nb_literal_next inp n) inp),
            (\inp -> b_chomped_last inp t)] inp) inp),
        (\inp -> l_chomped_empty inp n t)] inp) inp

-- [174] C-L+FOLDED 
c_lfolded :: Input -> Int -> Result
c_lfolded inp n =
    pegSeq [
        (\inp -> matchCp 62 inp),
        (\inp -> bindInt (\m inp -> bindCtx (\t inp -> pegSeq [(\inp -> s_b_comment inp), (\inp -> l_folded_content inp (n + m) t)] inp) (\inp -> pegAlt [
            (\inp -> parseSymP (\inp -> matchCp 45 inp) "STRIP" inp),
            (\inp -> parseSymP (\inp -> matchCp 43 inp) "KEEP" inp),
            (\inp -> valP "CLIP" inp)] inp) inp) (\inp -> pegAlt [(\inp -> parseIntP (\inp -> ns_dec_digit inp) inp), (\inp -> detectIndent n inp)] inp) inp)] inp

-- [175] S-NB-FOLDED-TEXT 
s_nb_folded_text :: Input -> Int -> Result
s_nb_folded_text inp n =
    pegSeq [
        (\inp -> s_indent inp n),
        (\inp -> ns_char inp),
        (\inp -> starP (\inp -> nb_char inp) inp)] inp

-- [176] L-NB-FOLDED-LINES 
l_nb_folded_lines :: Input -> Int -> Result
l_nb_folded_lines inp n =
    pegSeq [
        (\inp -> s_nb_folded_text inp n),
        (\inp -> starP (\inp -> pegSeq [(\inp -> b_l_folded inp n "BLOCK-IN"), (\inp -> s_nb_folded_text inp n)] inp) inp)] inp

-- [177] S-NB-SPACED-TEXT 
s_nb_spaced_text :: Input -> Int -> Result
s_nb_spaced_text inp n =
    pegSeq [
        (\inp -> s_indent inp n),
        (\inp -> s_white inp),
        (\inp -> starP (\inp -> nb_char inp) inp)] inp

-- [178] B-L-SPACED 
b_l_spaced :: Input -> Int -> Result
b_l_spaced inp n =
    pegSeq [(\inp -> b_as_line_feed inp), (\inp -> starP (\inp -> l_empty inp n "BLOCK-IN") inp)] inp

-- [179] L-NB-SPACED-LINES 
l_nb_spaced_lines :: Input -> Int -> Result
l_nb_spaced_lines inp n =
    pegSeq [
        (\inp -> s_nb_spaced_text inp n),
        (\inp -> starP (\inp -> pegSeq [(\inp -> b_l_spaced inp n), (\inp -> s_nb_spaced_text inp n)] inp) inp)] inp

-- [180] L-NB-SAME-LINES 
l_nb_same_lines :: Input -> Int -> Result
l_nb_same_lines inp n =
    pegSeq [
        (\inp -> starP (\inp -> l_empty inp n "BLOCK-IN") inp),
        (\inp -> pegAlt [(\inp -> l_nb_folded_lines inp n), (\inp -> l_nb_spaced_lines inp n)] inp)] inp

-- [181] L-NB-DIFF-LINES 
l_nb_diff_lines :: Input -> Int -> Result
l_nb_diff_lines inp n =
    pegSeq [
        (\inp -> l_nb_same_lines inp n),
        (\inp -> starP (\inp -> pegSeq [(\inp -> b_as_line_feed inp), (\inp -> l_nb_same_lines inp n)] inp) inp)] inp

-- [182] L-FOLDED-CONTENT 
l_folded_content :: Input -> Int -> String -> Result
l_folded_content inp n t =
    scalarP (\inp -> pegSeq [
        (\inp -> optP (\inp -> pegSeq [(\inp -> l_nb_diff_lines inp n), (\inp -> b_chomped_last inp t)] inp) inp),
        (\inp -> l_chomped_empty inp n t)] inp) inp

-- [183] L+BLOCK-SEQUENCE 
lblock_sequence :: Input -> Int -> Result
lblock_sequence inp n =
    buildP "SEQUENCE" (\inp -> bindInt (\m inp -> collectP (\inp -> plusP (\inp -> pegSeq [(\inp -> s_indent inp (n + m)), (\inp -> c_l_block_seq_entry inp (n + m))] inp) inp) inp) (\inp -> detectIndent n inp) inp) inp

-- [184] C-L-BLOCK-SEQ-ENTRY 
c_l_block_seq_entry :: Input -> Int -> Result
c_l_block_seq_entry inp n =
    pegSeq [
        (\inp -> matchCp 45 inp),
        (\inp -> negP (\inp -> ns_char inp) inp),
        (\inp -> s_lblock_indented inp n "BLOCK-IN")] inp

-- [185] S-L+BLOCK-INDENTED 
s_lblock_indented :: Input -> Int -> String -> Result
s_lblock_indented inp n c =
    pegAlt [
        (\inp -> bindInt (\m inp -> pegSeq [
            (\inp -> s_indent inp m),
            (\inp -> pegAlt [
                (\inp -> ns_l_compact_sequence inp (n + 1 + m)),
                (\inp -> ns_l_compact_mapping inp (n + 1 + m))] inp)] inp) (\inp -> detectIndent 0 inp) inp),
        (\inp -> s_lblock_node inp n c),
        (\inp -> pegSeq [(\inp -> e_node inp), (\inp -> s_l_comments inp)] inp)] inp

-- [186] NS-L-COMPACT-SEQUENCE 
ns_l_compact_sequence :: Input -> Int -> Result
ns_l_compact_sequence inp n =
    pegSeq [
        (\inp -> c_l_block_seq_entry inp n),
        (\inp -> starP (\inp -> pegSeq [(\inp -> s_indent inp n), (\inp -> c_l_block_seq_entry inp n)] inp) inp)] inp

-- [187] L+BLOCK-MAPPING 
lblock_mapping :: Input -> Int -> Result
lblock_mapping inp n =
    buildP "MAPPING" (\inp -> bindInt (\m inp -> collectP (\inp -> plusP (\inp -> pegSeq [(\inp -> s_indent inp (n + m)), (\inp -> ns_l_block_map_entry inp (n + m))] inp) inp) inp) (\inp -> detectIndent n inp) inp) inp

-- [188] NS-L-BLOCK-MAP-ENTRY 
ns_l_block_map_entry :: Input -> Int -> Result
ns_l_block_map_entry inp n =
    pegAlt [
        (\inp -> c_l_block_map_explicit_entry inp n),
        (\inp -> ns_l_block_map_implicit_entry inp n)] inp

-- [189] C-L-BLOCK-MAP-EXPLICIT-ENTRY 
c_l_block_map_explicit_entry :: Input -> Int -> Result
c_l_block_map_explicit_entry inp n =
    pegSeq [
        (\inp -> c_l_block_map_explicit_key inp n),
        (\inp -> pegAlt [(\inp -> l_block_map_explicit_value inp n), (\inp -> e_node inp)] inp)] inp

-- [190] C-L-BLOCK-MAP-EXPLICIT-KEY 
c_l_block_map_explicit_key :: Input -> Int -> Result
c_l_block_map_explicit_key inp n =
    pegSeq [(\inp -> matchCp 63 inp), (\inp -> s_lblock_indented inp n "BLOCK-OUT")] inp

-- [191] L-BLOCK-MAP-EXPLICIT-VALUE 
l_block_map_explicit_value :: Input -> Int -> Result
l_block_map_explicit_value inp n =
    pegSeq [
        (\inp -> s_indent inp n),
        (\inp -> matchCp 58 inp),
        (\inp -> s_lblock_indented inp n "BLOCK-OUT")] inp

-- [192] NS-L-BLOCK-MAP-IMPLICIT-ENTRY 
ns_l_block_map_implicit_entry :: Input -> Int -> Result
ns_l_block_map_implicit_entry inp n =
    buildP "PAIR" (\inp -> pegSeq [
        (\inp -> scalarP (\inp -> pegAlt [(\inp -> ns_s_block_map_implicit_key inp), (\inp -> e_node inp)] inp) inp),
        (\inp -> c_l_block_map_implicit_value inp n)] inp) inp

-- [193] NS-S-BLOCK-MAP-IMPLICIT-KEY 
ns_s_block_map_implicit_key :: Input -> Result
ns_s_block_map_implicit_key inp =
    pegAlt [
        (\inp -> c_s_implicit_json_key inp "BLOCK-KEY"),
        (\inp -> ns_s_implicit_yaml_key inp "BLOCK-KEY")] inp

-- [194] C-L-BLOCK-MAP-IMPLICIT-VALUE 
c_l_block_map_implicit_value :: Input -> Int -> Result
c_l_block_map_implicit_value inp n =
    pegSeq [
        (\inp -> matchCp 58 inp),
        (\inp -> pegAlt [
            (\inp -> s_lblock_node inp n "BLOCK-OUT"),
            (\inp -> scalarP (\inp -> pegSeq [(\inp -> e_node inp), (\inp -> s_l_comments inp)] inp) inp)] inp)] inp

-- [195] NS-L-COMPACT-MAPPING 
ns_l_compact_mapping :: Input -> Int -> Result
ns_l_compact_mapping inp n =
    pegSeq [
        (\inp -> ns_l_block_map_entry inp n),
        (\inp -> starP (\inp -> pegSeq [(\inp -> s_indent inp n), (\inp -> ns_l_block_map_entry inp n)] inp) inp)] inp

-- [196] S-L+BLOCK-NODE 
s_lblock_node :: Input -> Int -> String -> Result
s_lblock_node inp n c =
    pegAlt [(\inp -> s_lblock_in_block inp n c), (\inp -> s_lflow_in_block inp n)] inp

-- [197] S-L+FLOW-IN-BLOCK 
s_lflow_in_block :: Input -> Int -> Result
s_lflow_in_block inp n =
    pegSeq [
        (\inp -> s_separate inp (n + 1) "FLOW-OUT"),
        (\inp -> ns_flow_node inp (n + 1) "FLOW-OUT"),
        (\inp -> s_l_comments inp)] inp

-- [198] S-L+BLOCK-IN-BLOCK 
s_lblock_in_block :: Input -> Int -> String -> Result
s_lblock_in_block inp n c =
    pegAlt [(\inp -> s_lblock_scalar inp n c), (\inp -> s_lblock_collection inp n c)] inp

-- [199] S-L+BLOCK-SCALAR 
s_lblock_scalar :: Input -> Int -> String -> Result
s_lblock_scalar inp n c =
    pegSeq [
        (\inp -> s_separate inp (n + 1) c),
        (\inp -> optP (\inp -> pegSeq [(\inp -> c_ns_properties inp (n + 1) c), (\inp -> s_separate inp (n + 1) c)] inp) inp),
        (\inp -> pegAlt [(\inp -> c_lliteral inp n), (\inp -> c_lfolded inp n)] inp)] inp

-- [200] S-L+BLOCK-COLLECTION 
s_lblock_collection :: Input -> Int -> String -> Result
s_lblock_collection inp n c =
    pegSeq [
        (\inp -> optP (\inp -> pegSeq [(\inp -> s_separate inp (n + 1) c), (\inp -> c_ns_properties inp (n + 1) c)] inp) inp),
        (\inp -> s_l_comments inp),
        (\inp -> pegAlt [(\inp -> lblock_sequence inp (seqSpaces n c)), (\inp -> lblock_mapping inp n)] inp)] inp

-- [202] L-DOCUMENT-PREFIX 
l_document_prefix :: Input -> Result
l_document_prefix inp =
    pegSeq [
        (\inp -> optP (\inp -> c_byte_order_mark inp) inp),
        (\inp -> starP (\inp -> l_comment inp) inp)] inp

-- [203] C-DIRECTIVES-END 
c_directives_end :: Input -> Result
c_directives_end inp =
    matchStr "---" inp

-- [204] C-DOCUMENT-END 
c_document_end :: Input -> Result
c_document_end inp =
    matchStr "..." inp

-- [205] L-DOCUMENT-SUFFIX 
l_document_suffix :: Input -> Result
l_document_suffix inp =
    pegSeq [(\inp -> c_document_end inp), (\inp -> s_l_comments inp)] inp

-- [206] C-FORBIDDEN 
c_forbidden :: Input -> Result
c_forbidden inp =
    pegSeq [
        (\inp -> solP inp),
        (\inp -> pegAlt [(\inp -> c_directives_end inp), (\inp -> c_document_end inp)] inp),
        (\inp -> pegAlt [(\inp -> b_char inp), (\inp -> s_white inp), (\inp -> eofP inp)] inp)] inp

-- [207] L-BARE-DOCUMENT 
l_bare_document :: Input -> Result
l_bare_document inp =
    buildP "DOC" (\inp -> s_lblock_node inp (-1) "BLOCK-IN") inp

-- [208] L-EXPLICIT-DOCUMENT 
l_explicit_document :: Input -> Result
l_explicit_document inp =
    buildP "DOC" (\inp -> pegSeq [
        (\inp -> c_directives_end inp),
        (\inp -> pegAlt [
            (\inp -> l_bare_document inp),
            (\inp -> pegSeq [(\inp -> e_node inp), (\inp -> s_l_comments inp)] inp)] inp)] inp) inp

-- [209] L-DIRECTIVE-DOCUMENT 
l_directive_document :: Input -> Result
l_directive_document inp =
    pegSeq [(\inp -> plusP (\inp -> l_directive inp) inp), (\inp -> l_explicit_document inp)] inp

-- [210] L-ANY-DOCUMENT 
l_any_document :: Input -> Result
l_any_document inp =
    pegAlt [
        (\inp -> l_directive_document inp),
        (\inp -> l_explicit_document inp),
        (\inp -> l_bare_document inp)] inp

-- [211] L-YAML-STREAM 
l_yaml_stream :: Input -> Result
l_yaml_stream inp =
    buildP "STREAM" (\inp -> pegSeq [
        (\inp -> starP (\inp -> l_document_prefix inp) inp),
        (\inp -> optP (\inp -> l_any_document inp) inp),
        (\inp -> starP (\inp -> pegAlt [
            (\inp -> pegSeq [
                (\inp -> plusP (\inp -> l_document_suffix inp) inp),
                (\inp -> starP (\inp -> l_document_prefix inp) inp),
                (\inp -> optP (\inp -> l_any_document inp) inp)] inp),
            (\inp -> pegSeq [
                (\inp -> starP (\inp -> l_document_prefix inp) inp),
                (\inp -> optP (\inp -> l_explicit_document inp) inp)] inp)] inp) inp)] inp) inp

-- ── API ──

printAst :: Ast -> Int -> IO ()
printAst node depth = do
  let indent = replicate (depth * 2) ' '
  case node of
    Leaf t -> putStrLn (indent ++ "SCALAR: \"" ++ t ++ "\"")
    Branch t cs -> do
      putStrLn (indent ++ t)
      mapM_ (\c -> printAst c (depth + 1)) cs

-- ── Native Value Type ──

-- ── Native Value Type ──

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
yStr _ = ""

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
isMap _ = False

-- ── Schema Coercion ──

coerceScalar :: String -> YamlValue
coerceScalar s
  | s `elem` ["null", "Null", "NULL", "~", ""] = YNull
  | s `elem` ["true", "True", "TRUE"] = YBool True
  | s `elem` ["false", "False", "FALSE"] = YBool False
  | s `elem` [".inf", ".Inf", ".INF", "+.inf"] = YFloat (1/0)
  | s `elem` ["-.inf", "-.Inf", "-.INF"] = YFloat (-1/0)
  | s `elem` [".nan", ".NaN", ".NAN"] = YFloat (0/0)
  | Just i <- tryParseInt s = YInt i
  | Just f <- tryParseFloat s = YFloat f
  | otherwise = YStr s

tryParseInt :: String -> Maybe Integer
tryParseInt s
  | null s = Nothing
  | take 2 s == "0x" || take 2 s == "0X" =
      let hex = drop 2 s
      in if not (null hex) && all isHexDigit hex
         then Just (read ("0x" ++ hex))
         else Nothing
  | take 2 s == "0o" || take 2 s == "0O" =
      let oct = drop 2 s
      in if not (null oct) && all (\c -> c >= '0' && c <= '7') oct
         then Just (read ("0o" ++ oct))
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
  [(f, "")] -> Just f
  _          -> Nothing

-- ── AST → Native Conversion with Anchor Resolution ──

type AnchorMap = Map.Map String YamlValue

convert :: AnchorMap -> Ast -> (YamlValue, AnchorMap)
convert anchors node = case node of
  Leaf t -> (coerceScalar t, anchors)
  Branch t cs -> case t of
    "ANCHOR" ->
      let (name, val, anc) = foldl anchorStep ("", YNull, anchors) cs
      in (val, if null name then anc else Map.insert name val anc)
    "ALIAS" ->
      let found = foldr (\c acc -> case c of
            Leaf txt -> case Map.lookup txt anchors of
              Just v  -> v
              Nothing -> acc
            _ -> acc) YNull cs
      in (found, anchors)
    "MAPPING" ->
      let (pairs, anc) = foldl mapStep ([], anchors) cs
      in (YMap (reverse pairs), anc)
    "SEQUENCE" ->
      let (items, anc) = foldl seqStep ([], anchors) cs
      in (YSeq (reverse items), anc)
    "DOC" -> convertChildren anchors cs
    "STREAM" -> convertChildren anchors cs
    "PAIR" ->
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
  | astTag c == "PAIR" && length (astChildren c) >= 2 =
      let (key, anc1) = convert anc (astChildren c !! 0)
          (val, anc2) = convert anc1 (astChildren c !! 1)
      in if yStr key == "<<" && isMap val
         then case val of
           YMap mp -> (foldl (\ps (mk, mv) ->
             if any (\(k,_) -> k == mk) ps then ps else (mk,mv):ps) pairs mp, anc2)
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
      in if length xs == 1 then (head xs, anc') else (YSeq xs, anc')

-- ── Public API ──

yamlLoad :: String -> YamlValue
yamlLoad text =
  let inp = mkInput text
      r = l_yaml_stream inp
  in if failed r then YNull
     else case ast r of
       Just a  -> fst (convert Map.empty a)
       Nothing -> YNull

-- ── Main ──

main :: IO ()
main = do
  args <- getArgs
  text <- case args of
    (f:_) -> readFile f
    []    -> hGetContents stdin
  let inp = mkInput text
      r = l_yaml_stream inp
  if not (failed r) then do
    putStrLn ("OK: " ++ show (pos (rest r)) ++ " chars")
    case ast r of
      Just a  -> printAst a 0
      Nothing -> return ()
  else do
    putStrLn ("FAIL @" ++ show (pos (rest r)) ++ ": " ++ err r)
