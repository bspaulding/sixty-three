{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SocketResponse where

import Data.Aeson
import GHC.Generics

data SocketResponse a
  = ErrorResponse String
  | IdentifyConnection String
  | State a
  deriving (Generic, Show)

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