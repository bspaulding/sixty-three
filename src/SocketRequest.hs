{-# LANGUAGE DeriveGeneric #-}

module SocketRequest where

import Data.Aeson
import GHC.Generics

data SocketRequest a
  = CreateRoom
  | JoinRoom String
  | SetPlayerName String
  | GameAction a
  deriving (Generic, Show)

options :: Options
options =
  defaultOptions
    { sumEncoding =
        TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "payload"
          }
    }

instance FromJSON a => FromJSON (SocketRequest a) where
  parseJSON = genericParseJSON options
