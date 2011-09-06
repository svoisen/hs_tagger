import System.Environment
import Tagger.Tokenize
import Tagger.Data
import Data.Map as M

main = do
  (sentence:_) <- getArgs
  f <- readFile "data/lexicon.txt"
  let lexicon = buildLexicon f
  putStrLn $ show $ lexicon ! "dog"
