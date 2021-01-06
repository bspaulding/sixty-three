module ServerState where

import qualified Data.Map as Map
import qualified Network.WebSockets as WS

type RoomId = String

type ConnId = String

type Client = (ConnId, WS.Connection)

data ServerState a = ServerState
  { clientsById :: Map.Map ConnId Client,
    rooms :: Map.Map RoomId [ConnId],
    roomIdByConnId :: Map.Map ConnId RoomId,
    lobby :: [ConnId],
    stateByRoom :: Map.Map RoomId a
  }
  deriving (Show)

instance Show WS.Connection where
  show conn = "<wsconn>"
