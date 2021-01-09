{-# LANGUAGE DeriveGeneric #-}

module Card where

import Data.Aeson
import GHC.Generics

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Enum, Eq, Generic, Ord, Show)

instance ToJSON Suit where
  toJSON = genericToJSON defaultOptions

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Enum, Eq, Generic, Show)

instance ToJSON Face where
  toJSON = genericToJSON defaultOptions

rank :: Face -> Integer
rank f = case f of
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 11
  Queen -> 12
  King -> 13
  Ace -> 14

instance Ord Face where
  compare a b = compare (rank a) (rank b)

data Card = FaceCard Suit Face | Joker
  deriving (Eq, Generic, Ord, Show)

instance ToJSON Card where
  toJSON = genericToJSON defaultOptions
