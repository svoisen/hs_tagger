module Tagger.Data 
( Tag(..)
, Pair(Pair)
, Lexicon
, buildLexicon
, getWord
, getTag
, isNoun
) where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

-- Lexicon
type Lexicon = M.Map String [Tag]

buildLexicon :: B.ByteString -> Lexicon
buildLexicon = M.fromList . makeTuples . B.lines

makeTuples :: [B.ByteString] -> [(String, [Tag])]
makeTuples = map makeTuple

makeTuple :: B.ByteString -> (String, [Tag])
makeTuple line = 
  (word, map readTag tags :: [Tag])
  where (word:tags) = words $ B.unpack line

-- Tag
data Tag = 
  CC   | 
  CD   |
  DT   |
  EX   |
  FW   |
  IN   |
  JJ   |
  JJR  |
  JJS  |
  LS   |
  MD   |
  NN   |
  NNS  |
  NNP  |
  NNPS |
  PDT  |
  POS  |
  PRP  |
  PRPS |
  RB   |
  RBR  |
  RBS  |
  RP   |
  SYM  |
  TO   |
  UH   |
  VB   |
  VBD  |
  VBG  |
  VBN  |
  VBP  |
  VBZ  |
  WDT  |
  WP   |
  WPS  |
  WRB  deriving (Eq, Read, Show)

showTag :: Tag -> String
showTag t
  | t == NNPS = "NNP$"
  | t == PRPS = "PRP$"
  | otherwise = show t

readTag :: String -> Tag
readTag s
  | s == "NNP$" = NNPS
  | s == "PRP$" = PRPS
  | otherwise = read s :: Tag

isNoun :: Tag -> Bool
isNoun t 
  | t == NN = True
  | t == NNS = True
  | t == NNP = True
  | t == NNPS = True
  | otherwise = False

-- Pair
data Pair = Pair String Tag

instance Show Pair where
  show (Pair word tag) = word ++ "/" ++ showTag tag

getWord :: Pair -> String
getWord (Pair word _) = word

getTag :: Pair -> Tag
getTag (Pair _ tag) = tag
