{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThreeSpec (spec) where

import Card
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import GameAction
import GameState
import Import
import Player
import Shuffle
import SixtyThree
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude (foldl, head, print, putStrLn)

playGame :: [(Player, GameAction)] -> GameState
playGame = foldl reducer initialGameState

playGameFrom :: GameState -> [(Player, GameAction)] -> GameState
playGameFrom = foldl reducer

prop_ace_or_face i =
  withMaxSuccess 1000 $
    all hasAceOrFace [hand1, hand2, hand3, hand4] `shouldBe` True
  where
    ((hand1, _), (hand2, _), (hand3, _), (hand4, _), _) = fst $ deal deck (mkStdGen i)

prop_is_trump :: Suit -> Card -> Property
prop_is_trump trumpSuit card =
  classify (card == Joker) "joker" $
    classify
      ( case card of
          FaceCard suit _ -> suit == trumpSuit
          _ -> False
      )
      "in suit"
      $ classify
        ( case card of
            FaceCard suit Five -> suit == oppositeTrump trumpSuit
            _ -> False
        )
        "opposite five"
        $ classify
          ( case card of
              FaceCard suit _ -> suit /= trumpSuit
              _ -> False
          )
          "out of suit"
          $ isTrump trumpSuit card
            === case card of
              Joker -> True
              FaceCard suit face -> suit == trumpSuit || (suit == oppositeTrump trumpSuit && face == Five)

instance Arbitrary Suit where
  arbitrary = elements suits

instance Arbitrary Face where
  arbitrary = elements faces

instance Arbitrary Card where
  arbitrary = frequency [(1, return Joker), (51, FaceCard <$> arbitrary <*> arbitrary)]

instance Arbitrary Player where
  arbitrary = elements players

instance Arbitrary GameAction where
  arbitrary =
    oneof
      [ return Deal,
        return BidPass,
        Bid <$> arbitrary,
        Play <$> arbitrary,
        PickTrump <$> arbitrary,
        -- TODO: state prop here, can't discard cards you don't have!
        GameAction.Discard <$> arbitrary,
        -- TODO: state prop here, can't pass cards you don't have!
        PassCards <$> arbitrary
      ]

gameOverState :: GameState
gameOverState = initialGameState {previousRounds = rounds}
  where
    rounds = [round, round]
    round :: Round
    round = ((PlayerOne, 25), Map.fromList [(PlayerOne, 126)])

classifyAction :: GameAction -> String
classifyAction a =
  case a of
    Deal -> "Deal"
    BidPass -> "BidPass"
    Bid _ -> "Bid"
    Play _ -> "Play"
    PickTrump _ -> "PickTrump"
    GameAction.Discard _ -> "Discard"
    PassCards _ -> "PassCards"

prop_game_over :: Player -> GameAction -> Property
prop_game_over player action =
  classify True (classifyAction action) $
    reducer gameOverState (player, action) === gameOverState

spec :: Spec
spec = do
  describe "initializer" $ do
    let connIds = ["abcd", "efgh", "ijkl", "mnop"]
    let expectedPlayerMap =
          Map.fromList [ ("abcd", PlayerOne)
                       , ("efgh", PlayerTwo)
                       , ("ijkl", PlayerThree)
                       , ("mnop", PlayerFour)
                       ]
    let g = mkStdGen 0

    it "should fail if less than four conns" $ do
      initializer g (take 3 connIds) `shouldBe` Left "Need at least four people to start a game!"

    it "should map players if four conns" $ do
      playersByConnId <$> initializer g (reverse connIds) `shouldBe` Right (Map.fromList (zip (reverse connIds) players))

    it "should map first four players if more than four conns" $ do
      playersByConnId <$> initializer g (connIds ++ ["qrst"]) `shouldBe` Right expectedPlayerMap

    it "should ensure connIds are unique" $ do
      initializer g (take 2 connIds ++ take 2 connIds) `shouldBe` Left "Connection IDs must be unique! Got: [\"abcd\",\"efgh\",\"abcd\",\"efgh\"]"

  describe "partner" $ do
    it "should always be your partners' partner" $
      property $
        \player -> partner (partner player) === player

  describe "isTrump" $ do
    it "returns true if card is trump" $ property prop_is_trump

  describe "compareCards" $ do
    it "orders them by trump then face rank" $ do
      let a = FaceCard Hearts Ace
      let b = FaceCard Hearts Five
      let c = FaceCard Diamonds Five
      let d = FaceCard Spades Jack

      compareCards Hearts a b `shouldBe` GT
      compareCards Hearts a c `shouldBe` GT
      compareCards Hearts a d `shouldBe` GT
      compareCards Hearts b a `shouldBe` LT
      compareCards Hearts b c `shouldBe` GT
      compareCards Hearts b d `shouldBe` GT
      compareCards Hearts c a `shouldBe` LT
      compareCards Hearts c b `shouldBe` LT
      compareCards Hearts c d `shouldBe` GT
      compareCards Hearts d a `shouldBe` LT
      compareCards Hearts d b `shouldBe` LT
      compareCards Hearts d c `shouldBe` LT

      compareCards Hearts Joker d `shouldBe` GT
      compareCards Hearts Joker (FaceCard Hearts Two) `shouldBe` LT
      compareCards Hearts (FaceCard Hearts Two) Joker `shouldBe` GT

  describe "scoreTrick" $ do
    it "returns the winning player and the total points" $ do
      let trick = Map.fromList [(PlayerFour, FaceCard Hearts King), (PlayerOne, FaceCard Hearts Two), (PlayerTwo, FaceCard Hearts Ten), (PlayerThree, FaceCard Hearts Jack)]
      scoreTrick Hearts trick `shouldBe` (PlayerFour, 28)
      let trick' =
            Map.fromList
              [ (PlayerFour, FaceCard Hearts Ace),
                (PlayerOne, FaceCard Hearts Five),
                (PlayerTwo, FaceCard Diamonds Five),
                (PlayerThree, FaceCard Spades Jack)
              ]
      scoreTrick Hearts trick' `shouldBe` (PlayerFour, 11)

  describe "scoreTricks" $ do
    it "returns a map of player to score for all the tricks" $ do
      let tricks =
            [ Map.fromList [(PlayerFour, FaceCard Hearts King), (PlayerOne, FaceCard Hearts Two), (PlayerTwo, FaceCard Hearts Ten), (PlayerThree, FaceCard Hearts Jack)],
              Map.fromList [(PlayerFour, FaceCard Hearts Ace), (PlayerOne, FaceCard Hearts Five), (PlayerTwo, FaceCard Diamonds Five), (PlayerThree, FaceCard Spades Jack)]
            ]
      scoreTricks Hearts tricks `shouldBe` Map.fromList [(PlayerFour, 39)]

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
      let ((hand1, kitty1), (hand2, kitty2), (hand3, kitty3), (hand4, kitty4), kitty) = fst $ deal deck (mkStdGen 1)
      length hand1 `shouldBe` 9
      length hand2 `shouldBe` 9
      length hand3 `shouldBe` 9
      length hand4 `shouldBe` 9
      length kitty1 `shouldBe` 3
      length kitty2 `shouldBe` 3
      length kitty3 `shouldBe` 3
      length kitty4 `shouldBe` 3
      length kitty `shouldBe` 5

    it "deal action deals cards" $ do
      let state = playGame [(dealer initialGameState, Deal)]
      getBiddingComplete state `shouldBe` False
      length (getHand PlayerOne state) `shouldBe` 9
      length (getHand PlayerTwo state) `shouldBe` 9
      length (getHand PlayerThree state) `shouldBe` 9
      length (getHand PlayerFour state) `shouldBe` 9
      length (getPlayerKitty PlayerOne state) `shouldBe` 3
      length (getPlayerKitty PlayerTwo state) `shouldBe` 3
      length (getPlayerKitty PlayerThree state) `shouldBe` 3
      length (getPlayerKitty PlayerFour state) `shouldBe` 3
      length (getKitty state) `shouldBe` 5

    it "always deals hands with ace or face" $ property prop_ace_or_face

    it "can optionally deal with misdeals, and dealer loses turn" $ do
      pending

  describe "initial state" $ do
    it "no bids" $ do
      getBid initialGameState `shouldBe` Nothing
    it "starts with PlayerOne" $ do
      getCurrentPlayer initialGameState `shouldBe` PlayerOne
    it "starts bidding" $ do
      getBiddingComplete initialGameState `shouldBe` False

  describe "dealing" $ do
    it "cannot redeal if game in progress!" $ do
      pending

  describe "bidding" $ do
    it "can configure a minimum bid" $ do
      pending

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
      let actions = [(PlayerOne, BidPass), (PlayerTwo, Bid 30), (PlayerThree, BidPass), (PlayerFour, BidPass)]
      let state = foldl reducer initialGameState actions
      getBid state `shouldBe` Just (PlayerTwo, 30)
      getBiddingComplete state `shouldBe` True
      getCurrentPlayer state `shouldBe` PlayerTwo

    it "bid is defaulted to the dealer at 25 if the others pass and no bid" $ do
      let actions = [(PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass)]
      let state = foldl reducer initialGameState actions
      getDealer state `shouldBe` PlayerFour
      getBid state `shouldBe` Just (PlayerFour, 25)
      getCurrentPlayer state `shouldBe` PlayerFour

    it "bid is won when double 63 is bid" $ do
      let actions = [(PlayerOne, Bid 25), (PlayerTwo, Bid 126)]
      let state = foldl reducer initialGameState actions
      getBid state `shouldBe` Just (PlayerTwo, 126)
      getBiddingComplete state `shouldBe` True
      getCurrentPlayer state `shouldBe` PlayerTwo

    it "cannot pick trump if you did not win the bid" $ do
      let actions = [(PlayerOne, Bid 25), (PlayerTwo, Bid 126)]
      let state = foldl reducer initialGameState actions
      getBid state `shouldBe` Just (PlayerTwo, 126)
      getBiddingComplete state `shouldBe` True
      getCurrentPlayer state `shouldBe` PlayerTwo

      reducerSafe state (PlayerThree, PickTrump Spades) `shouldBe` Left "It is not your turn!"

  describe "discarding" $ do
    it "cannot discard a trump worth points, if you must discard trump" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let diamonds = map (FaceCard Diamonds) faces
      let clubs = map (FaceCard Clubs) faces
      let spades = map (FaceCard Spades) faces
      let hearts = map (FaceCard Hearts) faces
      let newHands =
            Map.fromList
              [ (PlayerOne, drop 1 diamonds),
                (PlayerTwo, drop 1 clubs),
                (PlayerThree, drop 1 spades),
                (PlayerFour, drop 1 hearts)
              ]
      let state = (playGame actions) {hands = newHands, trump = Just Hearts}
      getCurrentPlayer state `shouldBe` PlayerFour
      let nineOfHearts = FaceCard Hearts Nine
      List.find (== nineOfHearts) (getHand PlayerFour state) `shouldBe` Just nineOfHearts

      let discardedPoints = reducerSafe state (PlayerFour, GameAction.Discard [FaceCard Hearts Nine])
      discardedPoints `shouldBe` Left "cannot discard trump worth points!"

      let discardedNoPoints = reducerSafe state (PlayerFour, GameAction.Discard [FaceCard Hearts Three])
      getHand PlayerFour <$> discardedNoPoints `shouldBe` Right (drop 2 hearts)

    it "cannot pass the joker if you do not have the ace" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts), (PlayerFour, GameAction.Discard [])]
      let state = playGame actions
      let playerWithJoker = List.find (\p -> Set.member Joker (Set.fromList (getHand p state))) players
      playerWithJoker `shouldBe` Just PlayerOne
      let playerWithAce = List.find (\p -> Set.member (FaceCard Hearts Ace) (Set.fromList (getHand p state))) players
      playerWithAce `shouldBe` Just PlayerThree

      getCurrentPlayer state `shouldBe` PlayerFour

      let statePassed = reducerSafe state (PlayerOne, PassCards [Joker])
      statePassed `shouldNotBe` Right state
      statePassed `shouldBe` Left "You cannot pass the joker if you do not have the ace."

    it "can pass the joker if you have the ace" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let diamonds = map (FaceCard Diamonds) faces
      let clubs = map (FaceCard Clubs) faces
      let spades = map (FaceCard Spades) faces
      let hearts = map (FaceCard Hearts) faces
      let newHands =
            Map.fromList
              [ (PlayerOne, drop 1 diamonds),
                (PlayerTwo, drop 1 clubs),
                (PlayerThree, drop 1 spades),
                (PlayerFour, drop 1 hearts ++ [Joker])
              ]
      let state = (playGame actions) {hands = newHands, trump = Just Hearts}
      let playerWithJoker = List.find (\p -> Set.member Joker (Set.fromList (getHand p state))) players
      playerWithJoker `shouldBe` Just PlayerFour
      let playerWithAce = List.find (\p -> Set.member (FaceCard Hearts Ace) (Set.fromList (getHand p state))) players
      playerWithAce `shouldBe` Just PlayerFour

      getCurrentPlayer state `shouldBe` PlayerFour

      let statePassed = reducerSafe state (PlayerFour, PassCards [Joker])
      statePassed `shouldNotBe` Right state
      Set.member Joker <$> Set.fromList <$> getHand PlayerTwo <$> statePassed `shouldBe` Right True
      Set.member Joker <$> Set.fromList <$> getHand PlayerFour <$> statePassed `shouldBe` Right False

    it "cannot discard to less than 6 cards" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playGame actions
      let player = getCurrentPlayer state
      let hand = getHand player state
      let cardsToDiscard = filter (\c -> cardScore Hearts c == 0) hand
      let stateDiscarded = reducerSafe state (player, GameAction.Discard cardsToDiscard)

      stateDiscarded `shouldBe` Left "You must keep at least six cards in your hand."

    it "can discard to more than 6 cards" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playGame actions
      let player = getCurrentPlayer state
      let hand = getHand player state
      let cardsToDiscard = take 5 $ filter (\c -> cardScore Hearts c == 0) hand
      let stateDiscarded = reducerSafe state (player, GameAction.Discard cardsToDiscard)

      Set.fromList . getHand player <$> stateDiscarded `shouldBe` Right (Set.difference (Set.fromList hand) (Set.fromList cardsToDiscard))

  describe "tricking" $ do
    it "winner of the trick should become controlling player" $ do
      let state = playAllDiscards $ playGame [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      state `shouldSatisfy` getAllPlayersDiscarded

      let state2 = playTrickRound state
      (length . tricks) state2 `shouldBe` 1
      let (winner, _) = scoreTrick (Maybe.fromMaybe Hearts $ trump state2) $ (head . tricks) state2
      getCurrentPlayer state2 `shouldBe` winner

      let state3 = playTrickRound state2
      (length . tricks) state3 `shouldBe` 2
      let (winner, _) = scoreTrick (Maybe.fromMaybe Hearts $ trump state3) $ (head . tricks) state3
      getCurrentPlayer state3 `shouldBe` winner

    it "must lead trump on the first round" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]
      let state = playAllDiscards $ playGame actions
      state `shouldSatisfy` getAllPlayersDiscarded
      let hand = getHand PlayerFour state

      let notTrump = head $ filter (not . (isTrump Hearts)) hand
      let playedNotTrump = reducerSafe state (PlayerFour, Play notTrump)
      playedNotTrump `shouldBe` Left "You must lead with trump on the first round."

      let trump = head $ filter (isTrump Hearts) hand
      let playedTrump = reducerSafe state (PlayerFour, Play trump)
      playedTrump `shouldSatisfy` isRight

    it "can't play a card until trump is declared and all players have discarded" $ do
      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass)]
      let state = playGame actions
      let card = Prelude.head $ getHand PlayerFour state
      reducerSafe state (PlayerFour, Play card) `shouldBe` Left "You cannot play a card until trump is selected."

      let stateTrump = reducer state (PlayerFour, PickTrump Hearts)
      getTrump stateTrump `shouldBe` Just Hearts

      let stateNotDoneDiscarding = reducerSafe stateTrump (PlayerFour, Play card)
      stateNotDoneDiscarding `shouldBe` Left "You cannot play a card until everyone has discarded."

      let stateDiscarded = playAllDiscards stateTrump
      let card' = Prelude.head $ getHand PlayerFour stateDiscarded
      let statePlayed = reducerSafe stateDiscarded (PlayerFour, Play card')
      getCardInPlay PlayerFour <$> statePlayed `shouldBe` Right (Just card')

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

    let getNotTrump = \state player -> Prelude.head $ filter (not . (isTrump Hearts)) (getHand player state)
    let getTrump = \state player -> Prelude.head $ filter (isTrump Hearts) (getHand player state)

    it "can play off trump if lead with off trump" $ do
      let state = playTrickRound $ playAllDiscards $ playGame [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]

      getCurrentPlayer state `shouldBe` PlayerFour
      let cardOne = getNotTrump state PlayerFour
      let cardOnePlayed = reducer state (PlayerFour, Play cardOne)
      Map.lookup PlayerFour (cardsInPlay cardOnePlayed) `shouldBe` Just cardOne

      getCurrentPlayer cardOnePlayed `shouldBe` PlayerOne
      let cardTwo =  getNextCardToPlay (trump cardOnePlayed) (getHand PlayerOne cardOnePlayed)
      let cardTwoPlayed = reducer cardOnePlayed (PlayerOne, Play cardTwo)

      getCurrentPlayer cardTwoPlayed `shouldBe` PlayerTwo
      let cardThree = getNotTrump cardTwoPlayed PlayerTwo
      let cardThreePlayed = reducerSafe cardTwoPlayed (PlayerTwo, Play cardThree)
      getCurrentPlayer <$> cardThreePlayed `shouldBe` Right PlayerThree
      cardThreePlayed `shouldSatisfy` isRight
      (\s -> Map.lookup PlayerTwo (cardsInPlay s)) <$> cardThreePlayed `shouldBe` Right (Just cardThree)

    it "cannot play off trump if lead with trump" $ do
      let state = playAllDiscards $ playGame [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass), (PlayerFour, PickTrump Hearts)]

      let cardOne = getTrump state PlayerFour
      let cardOnePlayed = reducer state (PlayerFour, Play cardOne)
      firstCardPlayed cardOnePlayed `shouldBe` Just cardOne

      let cardTwo =  getNextCardToPlay (trump cardOnePlayed) (getHand PlayerOne cardOnePlayed)
      let cardTwoPlayed = reducer cardOnePlayed (PlayerOne, Play cardTwo)
      firstCardPlayed cardTwoPlayed `shouldBe` Just cardOne

      let cardThree = getNotTrump cardTwoPlayed PlayerTwo
      let cardThreePlayed = reducerSafe cardTwoPlayed (PlayerTwo, Play cardThree)
      cardThreePlayed `shouldBe` Left "You cannot play off trump if trump was lead."

    it "have no trump left but lead with trump" $ do
      -- TODO: irl you would just discard and not play any further, this reveals to all that you have no trump
      pendingWith "this appears to be covered by chance in the happy path game"

    it "happy path game" $ do
      let dealt = reducer initialGameState (getDealer initialGameState, Deal)
      state1 <- playHappyPathRound dealt
      getTotalScore state1 `shouldBe` (47, -25)
      getGameOver state1 `shouldBe` False

      state2 <- playHappyPathRound state1
      getTotalScore state2 `shouldBe` (22, 24)
      getGameOver state2 `shouldBe` False

      state3 <- playHappyPathRound state2
      getTotalScore state3 `shouldBe` (28, 81)
      getGameOver state3 `shouldBe` False

      state4 <- playHappyPathRound state3
      getTotalScore state4 `shouldBe` (60, 112)
      getGameOver state4 `shouldBe` False

      state5 <- playHappyPathRound state4
      getTotalScore state5 `shouldBe` (66, 169)
      getGameOver state5 `shouldBe` False

      state6 <- playHappyPathRound state5
      getTotalScore state6 `shouldBe` (129, 169)
      getGameOver state6 `shouldBe` False

      -- TODO: Maybe auto-play the last round? This might be weird for users.
      state7 <- playHappyPathRound state6
      getTotalScore state7 `shouldBe` (166, 195)
      getGameOver state7 `shouldBe` False

      state8 <- playHappyPathRound state7
      getTotalScore state8 `shouldBe` (227, 197)
      getGameOver state8 `shouldBe` True

    it "disallows any actions if the game is over" $ property prop_game_over

    it "passing cards because you have too many trump" $ do
      let hand1 = map (FaceCard Hearts) (drop 1 faces)
      let hand2 = map (FaceCard Diamonds) (drop 1 faces)
      let hand3 = map (FaceCard Spades) (drop 1 faces)
      let hand4 = map (FaceCard Clubs) (drop 1 faces)
      let kitty = map (`FaceCard` Two) suits ++ [Joker]

      let actions = [(dealer initialGameState, Deal), (PlayerOne, BidPass), (PlayerTwo, BidPass), (PlayerThree, BidPass)]
      let state' =
            (playGame actions)
              { kitty = kitty,
                hands = Map.fromList [(PlayerOne, hand1), (PlayerTwo, hand2), (PlayerThree, hand3), (PlayerFour, hand4)]
              }
      let state = reducer state' (PlayerFour, PickTrump Clubs)

      getCurrentPlayer state `shouldBe` PlayerFour

      let cardsToPass = [FaceCard Clubs Three, FaceCard Clubs Four, FaceCard Clubs Five]
      let passedState = foldl reducer state [(PlayerFour, PassCards cardsToPass)]
      getHand PlayerTwo passedState `shouldBe` hand2 ++ cardsToPass
      getHand PlayerFour passedState `shouldBe` List.sort (kitty ++ drop 3 hand4)

  describe "calcTotalScore" $ do
    it "total scores for each pair" $ do
      calcTotalScore [] `shouldBe` (0, 0)
      calcTotalScore [((PlayerOne, 25), Map.fromList [(PlayerOne, 60), (PlayerTwo, 0), (PlayerThree, 3), (PlayerFour, 0)])] `shouldBe` (63, 0)
      calcTotalScore [((PlayerOne, 25), Map.fromList [(PlayerOne, 50), (PlayerTwo, 9), (PlayerThree, 3), (PlayerFour, 1)])] `shouldBe` (53, 10)
      calcTotalScore
        [ ((PlayerOne, 25), Map.fromList [(PlayerOne, 60), (PlayerTwo, 0), (PlayerThree, 3), (PlayerFour, 0)]),
          ((PlayerOne, 25), Map.fromList [(PlayerOne, 50), (PlayerTwo, 9), (PlayerThree, 3), (PlayerFour, 1)])
        ]
        `shouldBe` (116, 10)
      calcTotalScore
        [ ((PlayerOne, 25), Map.fromList [(PlayerOne, 60), (PlayerTwo, 0), (PlayerThree, 3), (PlayerFour, 0)]),
          ((PlayerOne, 25), Map.fromList [(PlayerOne, 50), (PlayerTwo, 9), (PlayerThree, 3), (PlayerFour, 1)]),
          ((PlayerOne, 25), Map.fromList [(PlayerOne, 50), (PlayerTwo, 9), (PlayerThree, 3), (PlayerFour, 1)]),
          ((PlayerOne, 25), Map.fromList [(PlayerOne, 31), (PlayerTwo, 32), (PlayerThree, 0), (PlayerFour, 0)])
        ]
        `shouldBe` (200, 52)

    it "records a negative score if a team did not make their bid" $ do
      calcTotalScore
        [((PlayerOne, 63), Map.fromList [(PlayerOne, 50), (PlayerTwo, 9), (PlayerThree, 3), (PlayerFour, 1)])]
        `shouldBe` (-63, 10)
      calcTotalScore
        [((PlayerTwo, 63), Map.fromList [(PlayerOne, 50), (PlayerTwo, 9), (PlayerThree, 3), (PlayerFour, 1)])]
        `shouldBe` (53, -63)

-- game playing helper functions

playHappyPathRound initialState = do
  getBiddingComplete initialState `shouldBe` False
  let theDealer = dealer initialState
  let actions = [(enumNext theDealer, BidPass), ((enumNext . enumNext) theDealer, BidPass), ((enumNext . enumNext . enumNext) theDealer, BidPass), (theDealer, PickTrump Hearts)]
  let state = playGameFrom initialState actions
  getBiddingComplete state `shouldBe` True
  getCurrentPlayer state `shouldBe` theDealer

  -- we've each got 12 cards before discarding,
  -- unless you won the bid, then you have 17
  length (getHand PlayerOne state) `shouldBe` if theDealer == PlayerOne then 17 else 12
  length (getHand PlayerTwo state) `shouldBe` if theDealer == PlayerTwo then 17 else 12
  length (getHand PlayerThree state) `shouldBe` if theDealer == PlayerThree then 17 else 12
  length (getHand PlayerFour state) `shouldBe` if theDealer == PlayerFour then 17 else 12
  getKitty state `shouldBe` []

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
  let expectedTrick =
        Map.fromList
          [ (PlayerOne, getNextCardToPlay (trump stateDiscarded) $ getHand PlayerOne stateDiscarded)
          , (PlayerTwo, getNextCardToPlay (trump stateDiscarded) $ getHand PlayerTwo stateDiscarded)
          , (PlayerThree, getNextCardToPlay (trump stateDiscarded) $ getHand PlayerThree stateDiscarded)
          , (PlayerFour, getNextCardToPlay (trump stateDiscarded) $ getHand PlayerFour stateDiscarded)
          ]
  getTricks state4 `shouldBe` [expectedTrick]

  -- ok let's start from zero and play all the tricks
  let state5 = playAllTricks stateDiscarded

  let scores = snd <$> getLastRound state5
  let totalScore = foldl (+) 0 <$> Map.elems <$> scores
  totalScore `shouldBe` Just 63

  -- assert round was reset and ready for next
  getDealer state5 `shouldBe` enumNext (dealer initialState)
  getCurrentPlayer state5 `shouldBe` enumNext (enumNext (dealer initialState))
  getCardInPlay PlayerOne state5 `shouldBe` Nothing

  return state5

-- play a trick turn, the current player simply plays the first card in their hand
getNextCardToPlay :: Maybe Suit -> [Card] -> Card
getNextCardToPlay maybeTrump hand = Prelude.head $ trumpCards ++ notTrumpCards
  where
    trumpCards = filter (isTrumpMaybe maybeTrump) hand
    notTrumpCards = filter (not . (isTrumpMaybe maybeTrump)) hand

playTurn :: GameState -> GameState
playTurn state = reducer state (player, Play card)
  where
    player = getCurrentPlayer state
    hand = getHand player state
    card = getNextCardToPlay (trump state) hand

-- play a one trick round
playTrickRound :: GameState -> GameState
playTrickRound state = foldl (\s _ -> playTurn s) state [0 .. 3]

playAllTricks :: GameState -> GameState
playAllTricks state = foldl (\s _ -> playTrickRound s) state [0 .. 11]

-- TODO: prefer to discard higher faced off trump
playDiscard :: GameState -> Player -> GameState
playDiscard state player =
  case getTrump state of
    Nothing -> state -- no trump wtf are we doing?
    Just t ->
      reducer state (player, GameAction.Discard discardedCards)
      where
        -- TODO: there's a bug here i think if you don't have enough non trump cards need to pass
        discardedCards = take (length hand - 6) nonTrumpCards
        nonTrumpCards = filter (not . isTrump t) hand
        hand = getHand player state

playAllDiscards :: GameState -> GameState
playAllDiscards state =
  foldl playDiscard state players
