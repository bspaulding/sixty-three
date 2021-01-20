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

options :: Options
options = defaultOptions
  { sumEncoding =
      TaggedObject
        { tagFieldName = "type",
          contentsFieldName = "payload"
        }
  }

instance ToJSON GameAction where
  toJSON = genericToJSON options

instance FromJSON GameAction where
  parseJSON = genericParseJSON options
