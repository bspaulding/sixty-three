{-# LANGUAGE DeriveGeneric #-}

module GameState where

import Card
import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics
import System.Random

-- lists of cards should probably be sets of cards
data GameState = GameState
  { dealer :: Player,
    currentBid :: Maybe (Player, Integer),
    bidPassed :: Map.Map Player Bool,
    hands :: Map.Map Player [Card],
    kitty :: [Card],
    tricks :: [Map.Map Player Card],
    playerInControl :: Player,
    cardsInPlay :: Map.Map Player Card,
    discarded :: [Card],
    trump :: Maybe Suit,
    previousRounds :: [Round],
    g :: StdGen
  }
  deriving (Eq, Generic, Show)

instance ToJSON StdGen where
  toJSON _ = object []

instance ToJSON GameState where
  toJSON = genericToJSON defaultOptions

data Player = PlayerOne | PlayerTwo | PlayerThree | PlayerFour
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance ToJSON Player

instance ToJSONKey Player

type Round = ((Player, Integer), Map.Map Player Integer)
