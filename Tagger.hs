import System.Environment
import qualified Data.ByteString.Char8 as B
import Tagger.Tokenize
import Tagger.Tag
import Tagger.Data
import Paths_tagger

main :: IO ()
main = do
  (input:_)     <- getArgs
  dataFileName  <- getDataFileName "lexicon.txt"
  f             <- B.readFile $ dataFileName
  let lexicon   = buildLexicon f
      tokenized = tokenize input 
  putStrLn $ unwords $ map show $ applyRules $ assignTags tokenized lexicon
