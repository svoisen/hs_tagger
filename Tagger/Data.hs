module Tagger.Data 
( Tag(..)
, Pair(Pair)
, Lexicon
, buildLexicon
, word
, tag
, isNoun
) where

import qualified Data.Map as M

-- Lexicon
type Lexicon = M.Map String [Tag]

buildLexicon :: String -> Lexicon
buildLexicon = M.fromList . makeTuples . lines

makeTuples :: [String] -> [(String, [Tag])]
makeTuples = map makeTuple

makeTuple :: String -> (String, [Tag])
makeTuple line = 
  (word, map read tags :: [Tag])
  where (word:tags) = words line

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
  WRB  deriving (Show, Read, Eq)

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
  show (Pair word tag) = word ++ "/" ++ show tag

--instance Read Pair where
--  read str
--    | "PRP$" = PRPS
--    | otherwise = 

word :: Pair -> String
word (Pair word _) = word

tag :: Pair -> Tag
tag (Pair _ tag) = tag
