{-# LANGUAGE DeriveGeneric #-}

module SocketRequest where

import Data.Aeson
import GHC.Generics

data SocketRequest
  = CreateRoom
  | JoinRoom String
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

instance FromJSON SocketRequest where
  parseJSON = genericParseJSON options