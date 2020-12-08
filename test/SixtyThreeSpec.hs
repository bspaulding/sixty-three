{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SixtyThreeSpec (spec) where

import           Import
import           Prelude               (foldl)
import           Shuffle
import           SixtyThree
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Util

playGame :: [(Player, GameAction)] -> GameState
playGame = foldl reducer initialGameState

hasAceOrFace :: Cards -> Bool
hasAceOrFace (Cards cards) = not $ null acesAndFaces
  where
    acesAndFaces = filter isAceOrFace cards
    isAceOrFace :: Card -> Bool
    isAceOrFace (FaceCard _ Ace)   = True
    isAceOrFace (FaceCard _ King)  = True
    isAceOrFace (FaceCard _ Queen) = True
    isAceOrFace (FaceCard _ Jack)  = True
    isAceOrFace _                  = False

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

    it "deal action deals cards" $ do
      let state = playGame [(dealer initialGameState, Deal)]
      getBiddingComplete state `shouldBe` False
      length (getHand PlayerOne state) `shouldBe` 12
      length (getHand PlayerTwo state) `shouldBe` 12
      length (getHand PlayerThree state) `shouldBe` 12
      length (getHand PlayerFour state) `shouldBe` 12
      length (getKitty state) `shouldBe` 5

    prop "ace or face" $ \i ->
      let (Cards deckCards) = deck
       in let (cards, _) = shuffle deckCards (mkStdGen i)
           in let (hand1, hand2, hand3, hand4, kitty) = deal (Cards cards)
               in all hasAceOrFace [hand1, hand2, hand3, hand4] `shouldBe` True
  describe "initial state" $ do
    it "no bids" $ do
      getBid initialGameState `shouldBe` Nothing
    it "starts with PlayerOne" $ do
      getCurrentPlayer initialGameState `shouldBe` PlayerOne
    it "starts bidding" $ do
      getBiddingComplete initialGameState `shouldBe` False

  describe "bidding" $ do
    it "can set bid" $ do
      let bid1 = reducer initialGameState (PlayerOne, Bid 25)
      getBid bid1 `shouldBe` Just (PlayerOne, 25)
      getCurrentPlayer bid1 `shouldBe` PlayerTwo

      let bid2 = reducer bid1 (PlayerTwo, Bid 26)
      getBid bid2 `shouldBe` Just (PlayerTwo, 26)
      getCurrentPlayer bid2 `shouldBe` PlayerThree

      let bid3 = reducer bid2 (PlayerThree, Bid 27)
      getBid bid3 `shouldBe` Just (PlayerThree, 27)
      getCurrentPlayer bid3 `shouldBe` PlayerFour

      let bid4 = reducer bid3 (PlayerFour, Bid 28)
      getBid bid4 `shouldBe` Just (PlayerFour, 28)
      getCurrentPlayer bid4 `shouldBe` PlayerOne

    it "can't set bid if not your turn" $ do
      getBid (reducer initialGameState (PlayerTwo, Bid 25)) `shouldBe` getBid initialGameState

    it "can't bid less than max bid" $ do
      let bid1 = reducer initialGameState (PlayerOne, Bid 40)
      getBid bid1 `shouldBe` Just (PlayerOne, 40)
      getCurrentPlayer bid1 `shouldBe` PlayerTwo

      let bid2 = reducer bid1 (PlayerTwo, Bid 39)
      bid2 `shouldBe` bid1

    it "can't bid less than 25" $ do
      reducer initialGameState (PlayerOne, Bid 24) `shouldBe` initialGameState

    it "can't bid > 63 except for 126" $ do
      reducer initialGameState (PlayerOne, Bid 64) `shouldBe` initialGameState
      reducer initialGameState (PlayerOne, Bid 125) `shouldBe` initialGameState
      reducer initialGameState (PlayerOne, Bid 127) `shouldBe` initialGameState
      let double = reducer initialGameState (PlayerOne, Bid 126)
      getBid double `shouldBe` Just (PlayerOne, 126)

    it "bid is won when three players pass" $ do
      let actions = [(PlayerOne, Bid 25), (PlayerTwo, Bid 30), (PlayerThree, BidPass), (PlayerFour, BidPass), (PlayerOne, BidPass)]
      let state = foldl reducer initialGameState actions
      getBid state `shouldBe` Just (PlayerTwo, 30)
      getBiddingComplete state `shouldBe` True

    it "bid is defaulted to the dealer at 25 if the others pass and no bid" $ do
      let actions = [(PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass)]
      let state = foldl reducer initialGameState actions
      getDealer state `shouldBe` PlayerFour
      getBid state `shouldBe` Just (PlayerFour, 25)

    it "bid is won when double 63 is bid" $ do
      let actions = [(PlayerOne, Bid 25), (PlayerTwo, Bid 126)]
      let state = foldl reducer initialGameState actions
      getBid state `shouldBe` Just (PlayerTwo, 126)
      getBiddingComplete state `shouldBe` True

  describe "tricking" $ do
    it "disallows playing a card not in the player's hand" $ do
      let actions = [(PlayerOne, Bid 25), (PlayerTwo, Bid 30), (PlayerThree, BidPass), (PlayerFour, BidPass), (PlayerOne, BidPass), (PlayerTwo, Play (FaceCard Hearts Two))]
      let state = playGame actions
      state `shouldBe` state

    it "cannot play a card if you already have a card in play" $ do
      pending

    it "playing a card puts it in play and removes it from your hand" $ do
      pending

    it "happy path game" $ do
      pendingWith "add more player actions here and assert the final round state/score/etc."
      let actions = [(PlayerFour, Deal), (PlayerOne, Bid 25), (PlayerTwo, Bid 30), (PlayerThree, BidPass), (PlayerFour, BidPass), (PlayerOne, BidPass)]
      let state = playGame actions
      getBiddingComplete state `shouldBe` True
