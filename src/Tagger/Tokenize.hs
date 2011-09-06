module Tagger.Tokenize 
( tokenize
) where

import Data.List.Split as S

tokenize :: String -> [String]
tokenize = S.split (dropDelims . dropBlanks $ oneOf " ,.;:'")
