module Tagger.Tag 
( assignTags
, applyRules
) where

import Data.Maybe
import qualified Data.Map as M
import Tagger.Data
import Text.Regex.PCRE

assignTags :: [String] -> Lexicon -> [Pair]
assignTags words lexicon = 
  map (\word -> assignTag word lexicon) words

applyRules :: [Pair] -> [Pair]
applyRules = unTrigramize . map allRules . trigramize

allRules :: [Pair] -> [Pair]
allRules = rule1 . rule3

assignTag :: String -> Lexicon -> Pair
assignTag word lexicon = 
  Pair word (head $ fromJust result :: Tag)
  where result = M.lookup word lexicon :: Maybe [Tag]

trigramize :: [Pair] -> [[Pair]]
trigramize []    = []
trigramize pairs = filter (\xs -> length xs == 3) $ take 3 pairs : trigramize (tail pairs)

unTrigramize :: [[Pair]] -> [Pair]
unTrigramize trigrams = 
  (firstPair : map (!! 1) trigrams) ++ [lastPair]
  where firstPair = head $ head trigrams
        lastPair = last $ last trigrams

rule1 :: [Pair] -> [Pair]
rule1 (prev:cur:next:_) = 
  if prevTag == DT && (curTag == VB || curTag == VBP || curTag == VBD)
    then [prev, Pair curWord NN, next]
    else [prev, cur, next]
  where prevTag = getTag prev
        curTag  = getTag cur
        curWord = getWord cur

rule3 :: [Pair] -> [Pair]
rule3 (prev:cur:next:_) =
  if isNoun curTag && curWord =~ "ed$"
    then [prev, Pair curWord VBN, next]
    else [prev, cur, next]
  where curTag  = getTag cur
        curWord = getWord cur
