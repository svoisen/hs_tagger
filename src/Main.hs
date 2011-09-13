import System.Environment
import Tagger.Tokenize
import Tagger.Tag
import Tagger.Data

main = do
  (input:_) <- getArgs
  f <- readFile "data/lexicon.txt"
  let lexicon   = buildLexicon f
      tokenized = tokenize input 
  putStrLn $ unwords $ map show $ assignTags tokenized lexicon
