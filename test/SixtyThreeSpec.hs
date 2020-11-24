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
  describe "initial state" $ do
    it "should set bids to 0" $ do
      getBid initialGameState `shouldBe` (PlayerFour, 25)
  describe "happy path game" $ do
    it "can set bid" $ do
      let bid1 = reducer initialGameState (PlayerOne, Bid 25)
      getBid bid1 `shouldBe` (PlayerOne, 25)
      getCurrentPlayer bid1 `shouldBe` PlayerTwo
      let bid2 = reducer bid1 (PlayerTwo, Bid 26)
      getBid bid2 `shouldBe` (PlayerTwo, 26)
      getCurrentPlayer bid2 `shouldBe` PlayerThree
      let bid3 = reducer bid2 (PlayerThree, Bid 27)
      getBid bid3 `shouldBe` (PlayerThree, 27)
      getCurrentPlayer bid3 `shouldBe` PlayerFour
      let bid4 = reducer bid3 (PlayerFour, Bid 28)
      getBid bid4 `shouldBe` (PlayerFour, 28)
      getCurrentPlayer bid4 `shouldBe` PlayerOne
    it "can't set bid if not your turn" $ do
      getBid (reducer initialGameState (PlayerTwo, Bid 25)) `shouldBe` getBid initialGameState
    it "can't bid less than max bid" $ do
      let bid1 = reducer initialGameState (PlayerOne, Bid 40)
      getBid bid1 `shouldBe` (PlayerOne, 40)
      getCurrentPlayer bid1 `shouldBe` PlayerTwo
      let bid2 = reducer bid1 (PlayerTwo, Bid 39)
      bid2 `shouldBe` bid1
