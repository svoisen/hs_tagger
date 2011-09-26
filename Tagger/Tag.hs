module Tagger.Tag 
( assignTags
, applyRules
, trigramize
) where

import Data.Maybe
import qualified Data.Map as M
import Tagger.Data
import Text.Regex.PCRE
import Distribution.Simple.Utils

assignTags :: [String] -> Lexicon -> [Pair]
assignTags words lexicon = 
  map (\word -> assignTag word lexicon) words

applyRules :: [Pair] -> [Pair]
applyRules = unTrigramize . map allRules . trigramize
  where allRules = rule6 . rule5 . rule4 . rule3 . rule1

assignTag :: String -> Lexicon -> Pair
assignTag word lexicon = 
  Pair word (head $ fromJust result :: Tag)
  where result = M.lookup word lexicon :: Maybe [Tag]

trigramize :: [Pair] -> [[Pair]]
trigramize []       = []
trigramize [p]      = (take 3 $ repeat p) : []
trigramize [p1, p2] = [p1, p2, p2] : [take 3 $ repeat p2]
trigramize pairs    = filter (\xs -> length xs == 3) $ take 3 pairs : trigramize (tail pairs)

unTrigramize :: [[Pair]] -> [Pair]
unTrigramize []       = []
unTrigramize [t]      = head t : []
unTrigramize [t1, t2] = head t1 : [head t2] 
unTrigramize trigrams = 
  firstPair : map (!! 1) (init trigrams)
  where firstPair = head $ head trigrams

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

rule4 :: [Pair] -> [Pair]
rule4 (prev:cur:next:_) =
  if curWord =~ "ly$"
    then [prev, Pair curWord RB, next]
    else [prev, cur, next]
  where curWord = getWord cur

rule5 :: [Pair] -> [Pair]
rule5 (prev:cur:next:_) =
  if isNoun curTag && curWord =~ "al$"
    then [prev, Pair curWord JJ, next]
    else [prev, cur, next]
  where curTag  = getTag cur
        curWord = getWord cur

rule6 :: [Pair] -> [Pair]
rule6 (prev:cur:next:_) = 
  if isNoun curTag && lowercase prevWord == "would" 
    then [prev, Pair curWord VB, next]
    else [prev, cur, next]
  where curTag   = getTag cur
        prevWord = getWord prev
        curWord  = getWord cur
