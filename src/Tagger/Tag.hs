module Tagger.Tag 
( assignTags
, assignTag
) where

import Data.Maybe
import Data.Map as M
import Tagger.Data

assignTags :: [String] -> Lexicon -> [Pair]
assignTags words lexicon = 
  Prelude.map (\word -> assignTag word lexicon) words

assignTag :: String -> Lexicon -> Pair
assignTag word lexicon = 
  Pair word (head $ fromJust result :: Tag)
  where result = M.lookup word lexicon :: Maybe [Tag]
