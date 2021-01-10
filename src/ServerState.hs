module ServerState where

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
  { clientsById :: Map.Map ConnId Client,
    rooms :: Map.Map RoomId [ConnId],
    roomIdByConnId :: Map.Map ConnId RoomId,
    lobby :: [ConnId],
    names :: Map.Map ConnId String,
    stateByRoom :: Map.Map RoomId a
  }
  deriving (Show)

instance Show WS.Connection where
  show conn = "<wsconn>"

newServerState :: ServerState a
newServerState =
  ServerState
    { clientsById = Map.empty,
      rooms = Map.empty,
      lobby = [],
      names = Map.empty,
      roomIdByConnId = Map.empty,
      stateByRoom = Map.empty
    }

addClient :: Client -> ServerState a -> ServerState a
addClient client s = s {clientsById = Map.insert (fst client) client (clientsById s)}

addToLobby :: Client -> ServerState a -> ServerState a
addToLobby client s = s {lobby = fst client : lobby s}

removeFromLobby :: Client -> ServerState a -> ServerState a
removeFromLobby client s = s {lobby = Prelude.filter (/= fst client) (lobby s)}

removeFromRooms :: Client -> ServerState a -> ServerState a
removeFromRooms client s =
  s
    { rooms = Map.map (Prelude.filter (/= fst client)) (rooms s),
      roomIdByConnId = Map.delete (fst client) (roomIdByConnId s)
    }

removeClient :: Client -> ServerState a -> ServerState a
removeClient client s =
  removeFromLobby
    client
    (removeFromRooms client s)
      { clientsById = Map.delete (fst client) (clientsById s)
      }

addToRoom :: RoomId -> Client -> ServerState a -> ServerState a
addToRoom roomId client s =
  s
    { rooms = Map.insertWith (++) roomId [fst client] (rooms s),
      roomIdByConnId = Map.insert (fst client) roomId (roomIdByConnId s)
    }

moveClientToRoom :: RoomId -> Client -> ServerState a -> ServerState a
moveClientToRoom roomId client s = addToRoom roomId client (removeFromLobby client s)

getRoomId :: ConnId -> ServerState a -> Maybe RoomId
getRoomId connId state = Map.lookup connId (roomIdByConnId state)

getClients :: [ConnId] -> ServerState a -> [Client]
getClients connIds s = Maybe.mapMaybe (\connId -> Map.lookup connId (clientsById s)) connIds

getRoomClients :: RoomId -> ServerState a -> [Client]
getRoomClients roomId s = getClients connIds s
  where
    connIds = Map.findWithDefault [] roomId (rooms s)

getRoomPlayerNames :: RoomId -> ServerState a -> Map.Map ConnId String
getRoomPlayerNames roomId state = Map.fromList $ Prelude.map (\client -> (fst client, playerName client state)) roomClients
  where roomClients = getRoomClients roomId state

setStateInRoom :: RoomId -> a -> ServerState a -> ServerState a
setStateInRoom roomId game s = s {stateByRoom = Map.insert roomId game (stateByRoom s)}

updatePlayerName :: Client -> String -> ServerState a -> ServerState a
updatePlayerName (connId, _) name s = s {names = Map.insert connId name (names s)}

playerName :: Client -> ServerState a -> String
playerName (connId, _) s = Map.findWithDefault "Unknown" connId (names s)

serverStateReducer :: RandomGen g => g -> ServerState a -> Client -> SocketRequest -> (a -> action -> Either String a) -> Either String ((ServerState a), [(Client, SocketResponse.SocketResponse a) ])
serverStateReducer g s client r roomReducer =
  case r of
    CreateRoom ->
      let
        roomId = fst $ makeRoomId g
        nextState = moveClientToRoom roomId client s
        msgs = [(client, SocketResponse.JoinedRoom roomId (getRoomPlayerNames roomId nextState))]
      in Right (nextState, msgs)
    JoinRoom roomId ->
      let
        nextState = moveClientToRoom roomId client s
        msg = SocketResponse.PlayerJoinedRoom (fst client) (playerName client nextState)
        broadcast = map (\c -> (c, msg)) (getRoomClients roomId nextState)
        msgs = broadcast ++ [(client, SocketResponse.JoinedRoom roomId (getRoomPlayerNames roomId nextState))]
      in Right (nextState, msgs)
