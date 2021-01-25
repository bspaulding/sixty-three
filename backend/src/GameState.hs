{-# LANGUAGE DeriveGeneric #-}

module GameState where

import Card
import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import Player
import System.Random
import Util (safeHead)

-- lists of cards should probably be sets of cards
data GameState = GameState
  { dealer :: Player,
    currentBid :: Maybe (Player, Integer),
    bidPassed :: Map.Map Player Bool,
    hands :: Map.Map Player [Card],
    kitty :: [Card],
    tricks :: [Map.Map Player Card],
    playerInControl :: Player,
    firstCardPlayed :: Maybe Card,
    cardsInPlay :: Map.Map Player Card,
    discarded :: [Card],
    trump :: Maybe Suit,
    previousRounds :: [Round],
    playersByConnId :: Map.Map String Player,
    g :: StdGen
  }
  deriving (Eq, Generic, Show)

instance ToJSON StdGen where
  toJSON _ = object []

instance ToJSON GameState

type Round = ((Player, Integer), Map.Map Player Integer)

initialGameState :: GameState
initialGameState =
  GameState
    { dealer = PlayerFour,
      currentBid = Nothing,
      bidPassed = Map.empty,
      hands = Map.empty,
      kitty = [],
      tricks = [],
      playerInControl = PlayerOne,
      firstCardPlayed = Nothing,
      cardsInPlay = Map.empty,
      discarded = [],
      trump = Nothing,
      previousRounds = [],
      playersByConnId = Map.empty,
      g = mkStdGen 0
    }

-- selectors

getPlayer :: GameState -> String -> Maybe Player
getPlayer state connId = Map.lookup connId (playersByConnId state)

getDealer :: GameState -> Player
getDealer = dealer

getBid :: GameState -> Maybe (Player, Integer)
getBid = currentBid

getCurrentPlayer :: GameState -> Player
getCurrentPlayer = playerInControl

getBiddingComplete :: GameState -> Bool
getBiddingComplete state =
  Just 126 == fmap snd (getBid state)
    || 3 == length (filter id $ Map.elems (bidPassed state))

getKitty :: GameState -> [Card]
getKitty = kitty

getHand :: Player -> GameState -> [Card]
getHand player state =
  Map.findWithDefault [] player (hands state)

getCardInPlay :: Player -> GameState -> Maybe Card
getCardInPlay player state = Map.lookup player (cardsInPlay state)

getTricks :: GameState -> [Map.Map Player Card]
getTricks = tricks

getTrump :: GameState -> Maybe Suit
getTrump = trump

getAllPlayersDiscarded :: GameState -> Bool
getAllPlayersDiscarded state =
  all (<= 6) $ Map.elems $ Map.map length (hands state)

getLastRound :: GameState -> Maybe Round
getLastRound = safeHead . previousRounds

getTotalScore :: GameState -> (Integer, Integer)
getTotalScore state = calcTotalScore (previousRounds state)

calcTotalScore :: [Round] -> (Integer, Integer)
calcTotalScore rounds = (teamOdd, teamEven)
  where
    roundScores = map calcRoundScore rounds
    addTuples (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
    (teamOdd, teamEven) = foldl addTuples (0, 0) roundScores

calcRoundScore :: Round -> (Integer, Integer)
calcRoundScore ((bidder, bid), scores) = (teamOdd, teamEven)
  where
    playerOne = Map.findWithDefault 0 PlayerOne scores
    playerTwo = Map.findWithDefault 0 PlayerTwo scores
    playerThree = Map.findWithDefault 0 PlayerThree scores
    playerFour = Map.findWithDefault 0 PlayerFour scores
    teamOdd' = playerOne + playerThree
    teamOdd = if (bidder == PlayerOne || bidder == PlayerThree) && teamOdd' < bid then - bid else teamOdd'
    teamEven' = playerTwo + playerFour
    teamEven = if (bidder == PlayerTwo || bidder == PlayerFour) && teamEven' < bid then - bid else teamEven'

getGameOver :: GameState -> Bool
getGameOver state = teamOddScore >= 200 || teamEvenScore >= 200
  where
    (teamOddScore, teamEvenScore) = getTotalScore state
