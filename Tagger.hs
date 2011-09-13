import System.Environment
import Tagger.Tokenize
import Tagger.Tag
import Tagger.Data
import Paths_tagger

main :: IO ()
main = do
  (input:_)     <- getArgs
  dataFileName  <- getDataFileName "lexicon.txt"
  f             <- readFile $ dataFileName
  let lexicon   = buildLexicon f
      tokenized = tokenize input 
  putStrLn $ unwords $ map show $ assignTags tokenized lexicon
