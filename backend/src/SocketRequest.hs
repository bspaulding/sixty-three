{-# LANGUAGE DeriveGeneric #-}

module SocketRequest where

import Data.Aeson
import GHC.Generics

data SocketRequest a
  = CreateRoom
  | JoinRoom String
  | SetPlayerName String
  | InitRoom String
  | GameAction a
  deriving (Eq, Generic, Show)

options :: Options
options =
  defaultOptions
    { sumEncoding =
        TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "payload"
          }
    }

instance ToJSON a => ToJSON (SocketRequest a) where
  toJSON = genericToJSON options

instance FromJSON a => FromJSON (SocketRequest a) where
  parseJSON = genericParseJSON options
