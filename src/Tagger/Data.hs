module Tagger.Data 
( Tag
, Pair
, Lexicon
, buildLexicon
) where

import Data.Map as M

-- Lexicon
type Lexicon = Map String [Tag]

buildLexicon :: String -> Lexicon
buildLexicon = M.fromList . makeTuples . lines

makeTuples :: [String] -> [(String, [Tag])]
makeTuples []         = []
makeTuples (line:rest) = 
  let lineWords = words line
  in (head lineWords, Prelude.map read $ tail lineWords) : makeTuples rest

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

-- Pair
data Pair = Pair String Tag 

instance Show Pair where
  show (Pair word tag) = show word ++ "/" ++ show tag
