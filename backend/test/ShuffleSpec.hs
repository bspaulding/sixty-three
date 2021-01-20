module ShuffleSpec (spec) where

import qualified Data.Set as Set
import Shuffle
import System.Random
import Test.Hspec
import Test.QuickCheck

prop_same_elements :: Int -> [Int] -> Property
prop_same_elements i xs =
  Set.fromList xs === Set.fromList (fst (Shuffle.shuffle xs (mkStdGen i)))

spec :: Spec
spec = do
  describe "shuffle" $ do
    it "has all the same elements" $ property prop_same_elements