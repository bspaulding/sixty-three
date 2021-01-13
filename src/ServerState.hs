module ServerState where

import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Network.WebSockets as WS
import SocketRequest
import qualified SocketResponse
import System.Random
import Util

type RoomId = String

type ConnId = String

type Client = (ConnId, WS.Connection)

data ServerState a = ServerState
  { rooms :: Map.Map RoomId [ConnId],
    roomIdByConnId :: Map.Map ConnId RoomId,
    lobby :: [ConnId],
    names :: Map.Map ConnId String,
    stateByRoom :: Map.Map RoomId a
  }
  deriving (Show)

data ServerStateWS a = ServerStateWS
  { clientsById :: Map.Map ConnId Client
  , serverState :: ServerState a
  }

instance Show WS.Connection where
  show conn = "<wsconn>"

newServerStateWS :: ServerStateWS a
newServerStateWS =
  ServerStateWS
    { clientsById = Map.empty,
      serverState = newServerState
    }

newServerState :: ServerState a
newServerState =
  ServerState
    { rooms = Map.empty,
      lobby = [],
      names = Map.empty,
      roomIdByConnId = Map.empty,
      stateByRoom = Map.empty
    }

addClient :: Client -> ServerStateWS a -> ServerStateWS a
addClient client s = s {clientsById = Map.insert (fst client) client (clientsById s)}

addToLobby :: ConnId -> ServerState a -> ServerState a
addToLobby connId s = s {lobby = connId : lobby s}

connect :: Client -> ServerStateWS a -> ServerStateWS a
connect client state = addClient client state
  { serverState = addToLobby (fst client) (serverState state)}

removeFromLobby :: ConnId -> ServerState a -> ServerState a
removeFromLobby connId s = s {lobby = Prelude.filter (/= connId) (lobby s)}

removeFromRooms :: ConnId -> ServerState a -> ServerState a
removeFromRooms connId s =
  s
    { rooms = Map.map (Prelude.filter (/= connId)) (rooms s),
      roomIdByConnId = Map.delete connId (roomIdByConnId s)
    }

removeClient :: ConnId -> ServerStateWS a -> ServerStateWS a
removeClient connId s = s
  { clientsById = Map.delete connId (clientsById s)
  , serverState = removeFromLobby connId (removeFromRooms connId (serverState s))
  }

addToRoom :: RoomId -> ConnId -> ServerState a -> ServerState a
addToRoom roomId connId s =
  s
    { rooms = Map.insertWith (++) roomId [connId] (rooms s),
      roomIdByConnId = Map.insert connId roomId (roomIdByConnId s)
    }

moveClientToRoom :: RoomId -> ConnId -> ServerState a -> ServerState a
moveClientToRoom roomId connId s = addToRoom roomId connId (removeFromLobby connId s)

getRoomId :: ConnId -> ServerState a -> Maybe RoomId
getRoomId connId state = Map.lookup connId (roomIdByConnId state)

getClients :: [ConnId] -> ServerStateWS a -> [Client]
getClients connIds s = Maybe.mapMaybe (\connId -> Map.lookup connId (clientsById s)) connIds

getRoomConnIds :: RoomId -> ServerState a -> [ConnId]
getRoomConnIds roomId s = Map.findWithDefault [] roomId (rooms s)

getRoomPlayerNames :: RoomId -> ServerState a -> Map.Map ConnId String
getRoomPlayerNames roomId state = Map.fromList $ Prelude.map (\connId -> (connId, playerName connId state)) roomConnIds
  where roomConnIds = getRoomConnIds roomId state

setStateInRoom :: RoomId -> a -> ServerState a -> ServerState a
setStateInRoom roomId game s = s {stateByRoom = Map.insert roomId game (stateByRoom s)}

updatePlayerName :: ConnId -> String -> ServerState a -> ServerState a
updatePlayerName connId name s = s {names = Map.insert connId name (names s)}

playerName :: ConnId -> ServerState a -> String
playerName connId s = Map.findWithDefault "Unknown" connId (names s)

serverStateReducer :: RandomGen g => g -> ServerState a -> ConnId -> SocketRequest action -> (a -> action -> Either String a) -> Either String ((ServerState a), [(ConnId, SocketResponse.SocketResponse a) ])
serverStateReducer g s connId r roomReducer =
  case r of
    CreateRoom ->
      let
        roomId = fst $ makeRoomId g
        nextState = moveClientToRoom roomId connId s
        msgs = [(connId, SocketResponse.JoinedRoom roomId (getRoomPlayerNames roomId nextState))]
      in Right (nextState, msgs)
    JoinRoom roomId_ ->
      let
        roomId = map toLower roomId_
        nextState = moveClientToRoom roomId connId s
        msg = SocketResponse.PlayerJoinedRoom connId (playerName connId nextState)
        msgs = broadcast msg roomId nextState ++ [(connId, SocketResponse.JoinedRoom roomId (getRoomPlayerNames roomId nextState))]
      in Right (nextState, msgs)
    SetPlayerName name ->
      let
        nextState = updatePlayerName connId name s
        msg = SocketResponse.PlayerNameChanged connId name
        msgs = maybe [(connId, msg)]
          (\roomId -> broadcast msg roomId nextState)
          (getRoomId connId nextState)
      in Right (nextState, msgs)

broadcast :: SocketResponse.SocketResponse a -> RoomId -> ServerState a -> [(ConnId, SocketResponse.SocketResponse a)]
broadcast msg roomId nextState =
  map (\c -> (c, msg)) (getRoomConnIds roomId nextState)
