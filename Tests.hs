import Control.Monad
import Test.QuickCheck
import Text.Printf
import Tagger.Tag
import Tagger.Data

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_trigramHas3 pairs 
  | null pairs = True
  | otherwise = all (\t -> length t == 3) $ trigramize pairs

tests = [("trigram/has3", quickCheck prop_trigramHas3)]

instance Arbitrary Tag where
  arbitrary = elements [NN, NNP]

instance Arbitrary Pair where
  arbitrary = liftM2 Pair arbitrary arbitrary
