{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketResponse where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics

data SocketResponse a
  = ErrorResponse String
  | IdentifyConnection String
  | JoinedRoom {roomId :: String, playerNamesById :: Map.Map String String}
  | PlayerJoinedRoom {connId :: String, name :: String}
  | PlayerNameChanged {connId :: String, name :: String}
  | State a
  deriving (Eq, Generic, Show)

socketResponseToJSONOptions :: Options
socketResponseToJSONOptions =
  defaultOptions
    { sumEncoding =
        TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "payload"
          }
    }

instance ToJSON a => ToJSON (SocketResponse a) where
  toJSON = genericToJSON socketResponseToJSONOptions
  toEncoding = genericToEncoding socketResponseToJSONOptions
