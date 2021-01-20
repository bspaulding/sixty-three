{-# LANGUAGE DeriveGeneric #-}

module GameAction where

import Card
import Data.Aeson
import GHC.Generics

data GameAction
  = Deal
  | BidPass
  | Bid Integer
  | Play Card
  | PickTrump Suit
  | Discard [Card]
  | PassCards [Card]
  deriving (Eq, Generic, Show)

instance ToJSON GameAction
instance FromJSON GameAction
