{-# LANGUAGE DeriveGeneric #-}

module Player where

import Data.Aeson
import GHC.Generics

data Player = PlayerOne | PlayerTwo | PlayerThree | PlayerFour
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance ToJSON Player

instance ToJSONKey Player

instance FromJSON Player

