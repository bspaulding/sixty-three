{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThreeSpec (spec) where

import Import
import Shuffle
import SixtyThree
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Util

hasAceOrFace :: Cards -> Bool
hasAceOrFace (Cards cards) = not $ null acesAndFaces
  where
    acesAndFaces = filter isAceOrFace cards
    isAceOrFace :: Card -> Bool
    isAceOrFace (FaceCard _ Ace) = True
    isAceOrFace (FaceCard _ King) = True
    isAceOrFace (FaceCard _ Queen) = True
    isAceOrFace (FaceCard _ Jack) = True
    isAceOrFace _ = False

spec :: Spec
spec = do
  describe "deal" $ do
    it "returns four hands, kitty, and rest" $ do
      let (Cards hand1, Cards hand2, Cards hand3, Cards hand4, Cards kitty) = deal deck
      length hand1 `shouldBe` 12
      length hand2 `shouldBe` 12
      length hand3 `shouldBe` 12
      length hand4 `shouldBe` 12
      length kitty `shouldBe` 5
    prop "ace or face" $ \i ->
      let (Cards deckCards) = deck
       in let (cards, _) = shuffle deckCards (mkStdGen i)
           in let (hand1, hand2, hand3, hand4, kitty) = deal (Cards cards)
               in all hasAceOrFace [hand1, hand2, hand3, hand4] `shouldBe` True
