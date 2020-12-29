{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThreeSpec (spec) where

import qualified Data.Map as Map
import Import
import Shuffle
import SixtyThree
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Util
import Prelude (foldl, head)

playGame :: [(Player, GameAction)] -> GameState
playGame = foldl reducer initialGameState

prop_ace_or_face i =
  withMaxSuccess 1000 $
    all hasAceOrFace [hand1, hand2, hand3, hand4] `shouldBe` True
  where
    (hand1, hand2, hand3, hand4, kitty) = fst $ deal deck (mkStdGen i)

spec :: Spec
spec = do
  describe "card points" $ do
    it "scores cards according to trump" $ do
      cardScore Hearts (FaceCard Hearts Ace) `shouldBe` 1
      cardScore Hearts (FaceCard Hearts King) `shouldBe` 25
      cardScore Hearts (FaceCard Hearts Jack) `shouldBe` 1
      cardScore Hearts (FaceCard Hearts Ten) `shouldBe` 1
      cardScore Hearts (FaceCard Hearts Nine) `shouldBe` 9
      cardScore Hearts (FaceCard Hearts Five) `shouldBe` 5
      cardScore Hearts (FaceCard Diamonds Five) `shouldBe` 5
      cardScore Hearts (FaceCard Hearts Two) `shouldBe` 1
      cardScore Hearts Joker `shouldBe` 15

  describe "deal" $ do
    it "returns four hands, kitty, and rest" $ do
      let (hand1, hand2, hand3, hand4, kitty) = fst $ deal deck (mkStdGen 1)
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

    it "always deals hands with ace or face" $ property $ prop_ace_or_face

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

  describe "discarding" $ do
    it "cannot discard to less than 6 cards" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playGame actions
      let player = getCurrentPlayer state
      let cardsToDiscard = take 7 (getHand player state)
      let stateDiscarded = reducer state (player, SixtyThree.Discard cardsToDiscard)

      state `shouldBe` stateDiscarded

    it "cannot discard to more than 6 cards" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playGame actions
      let player = getCurrentPlayer state
      let cardsToDiscard = take 5 (getHand player state)
      let stateDiscarded = reducer state (player, SixtyThree.Discard cardsToDiscard)

      state `shouldBe` stateDiscarded

  describe "tricking" $ do
    it "can't play a card until trump is declared" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass)]
      let state = playGame actions
      let card = Prelude.head $ getHand PlayerFour state
      -- nothing should change, action should be ignored
      reducer state (PlayerFour, Play card) `shouldBe` state

      let stateTrump = reducer state (PlayerFour, PickTrump Hearts)
      getTrump stateTrump `shouldBe` Just Hearts

    it "disallows playing a card not in the player's hand" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass)]
      let state = playGame actions
      getCurrentPlayer state `shouldBe` PlayerFour
      let notYourCard = Prelude.head $ getHand PlayerOne state

      let state1 = reducer state (PlayerFour, Play notYourCard)
      getCardInPlay PlayerFour state1 `shouldBe` Nothing
      getHand PlayerOne state1 `shouldSatisfy` any (notYourCard ==)
      getHand PlayerFour state1 `shouldSatisfy` not . any (notYourCard ==)
      getCurrentPlayer state1 `shouldBe` PlayerFour

    it "cannot play a card if you already have a card in play" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playAllDiscards $ playGame actions
      let card = Prelude.head $ getHand PlayerFour state
      let state1 = reducer state (PlayerFour, Play card)
      -- sanity check there's a card in play
      getCardInPlay PlayerFour state1 `shouldBe` Just card

      -- now try to play another card
      let card2 = Prelude.head $ getHand PlayerFour state1
      card2 /= card `shouldBe` True
      let state2 = reducer state1 (PlayerFour, Play card2)
      getCardInPlay PlayerFour state2 `shouldBe` Just card
      getHand PlayerFour state2 `shouldSatisfy` any (card /=)
      getHand PlayerFour state2 `shouldSatisfy` any (card2 ==)

    it "playing a card puts it in play and removes it from your hand" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playAllDiscards $ playGame actions
      let card = Prelude.head $ getHand PlayerFour state
      let state1 = reducer state (PlayerFour, Play card)
      getHand PlayerFour state1 `shouldSatisfy` (not . any (card ==))
      getCardInPlay PlayerFour state1 `shouldBe` Just card
      getCurrentPlayer state1 `shouldSatisfy` not . (PlayerFour ==)
      length (getHand PlayerFour state1) `shouldBe` length (getHand PlayerFour state) - 1

    it "happy path game" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playGame actions
      getBiddingComplete state `shouldBe` True
      getCurrentPlayer state `shouldBe` PlayerFour

      -- we've each got 12 cards before discarding
      length (getHand PlayerOne state) `shouldBe` 12
      length (getHand PlayerTwo state) `shouldBe` 12
      length (getHand PlayerThree state) `shouldBe` 12
      length (getHand PlayerFour state) `shouldBe` 12

      let stateDiscarded = playAllDiscards state
      getAllPlayersDiscarded stateDiscarded `shouldBe` True

      -- we've each got 6 cards after discarding
      length (getHand PlayerOne stateDiscarded) `shouldBe` 6
      length (getHand PlayerTwo stateDiscarded) `shouldBe` 6
      length (getHand PlayerThree stateDiscarded) `shouldBe` 6
      length (getHand PlayerFour stateDiscarded) `shouldBe` 6

      -- sidebar here...
      let state4 = playTrickRound stateDiscarded
      -- after one round of plays, each player should have one less card in their hands
      length (getHand PlayerOne state4) `shouldBe` 5
      length (getHand PlayerTwo state4) `shouldBe` 5
      length (getHand PlayerThree state4) `shouldBe` 5
      length (getHand PlayerFour state4) `shouldBe` 5
      getTricks state4 `shouldBe` [Map.fromList [(PlayerOne, Prelude.head $ getHand PlayerOne stateDiscarded), (PlayerTwo, Prelude.head $ getHand PlayerTwo stateDiscarded), (PlayerThree, Prelude.head $ getHand PlayerThree stateDiscarded), (PlayerFour, Prelude.head $ getHand PlayerFour stateDiscarded)]]

      -- ok let's start from zero and play all the tricks
      let state5 = playAllTricks stateDiscarded
      getHand PlayerOne state5 `shouldBe` []
      getHand PlayerTwo state5 `shouldBe` []
      getHand PlayerThree state5 `shouldBe` []
      getHand PlayerFour state5 `shouldBe` []
      getCardInPlay PlayerOne state5 `shouldBe` Nothing

      pendingWith "add more player actions here and assert the final round state/score/etc."

-- game playing helper functions

-- play a trick turn, the current player simply plays the first card in their hand
playTurn :: GameState -> GameState
playTurn state = reducer state (player, Play card)
  where
    player = getCurrentPlayer state
    card = Prelude.head $ getHand player state

-- play a one trick round
playTrickRound :: GameState -> GameState
playTrickRound state = foldl (\s _ -> playTurn s) state [0 .. 3]

playAllTricks :: GameState -> GameState
playAllTricks state = foldl (\s _ -> playTrickRound s) state [0 .. 11]

playDiscard :: GameState -> GameState
playDiscard state =
  reducer state (player, SixtyThree.Discard discardedCards)
  where
    discardedCards = take 6 (getHand player state)
    player = getCurrentPlayer state

playAllDiscards :: GameState -> GameState
playAllDiscards state =
  foldl (\s _ -> playDiscard s) state [0 .. 3]