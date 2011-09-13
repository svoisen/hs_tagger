import System.Environment
import Tagger.Tokenize
import Tagger.Tag
import Tagger.Data

main :: IO ()
main = do
  (input:_) <- getArgs
  f         <- readFile $ getDataFileName "lexicon.txt"
  let lexicon   = buildLexicon f
      tokenized = tokenize input 
  putStrLn $ unwords $ map show $ assignTags tokenized lexicon
