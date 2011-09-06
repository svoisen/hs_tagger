module Tagger.Tag 
( assignTags
, assignTag
) where

import Data.Maybe

assignTags :: [String] -> Map String String -> [(String, String)]
assignTags words lexiconMap = Prelude.map (\word -> assignTag word lexiconMap) words

assignTag :: String -> Map String String -> (String, String)
assignTag word lexiconMap = (word, fromJust result) where result = M.lookup word lexiconMap
