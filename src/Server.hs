{-# LANGUAGE OverloadedStrings #-}

-- Server and ServerState should be generic, not tied to this specific game.
-- Here's the vision:
-- - You have a ServerState GameState, which holds the lobby/room operations/room state access
-- - The Server takes a "safe" reducer for GameState, ie GameState -> GameAction -> Either String GameState
-- - Server forwards all game actions to the reducer, updating the mVar for the room if Right

module Server (app) where

import Control.Concurrent (MVar, modifyMVar_)
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import GameState -- TODO: remove
import Network.HTTP.Types ()
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import ServerState
import SocketRequest
import SocketResponse
import WaiAppStatic.Types (unsafeToPiece)

app :: Show s => ToJSON s => MVar (ServerState s) -> (s -> a -> Either String s) -> Application
app stateM roomStateReducer = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      putStrLn "got a pending connection!"
      conn <- acceptRequest pending_conn
      id <- UUID.toString <$> UUID.nextRandom
      let client = (id, conn)
      let idMsg = IdentifyConnection id :: SocketResponse GameState
      sendTextData conn (encode idMsg)
      putStrLn "accepted request, sending hello"
      sendTextData conn ("Hello, client!" :: T.Text)
      flip finally disconnect $
        withPingThread conn 30 (return ()) $
          forever $ do
            msg <- receiveData conn
            case decode msg :: Maybe SocketRequest of
              Just socketRequest -> do
                print socketRequest
                modifyMVar_ stateM $ \state -> do
                  case serverStateReducer state socketRequest roomStateReducer of
                    Left err -> do
                      sendError conn err
                      return state
                    Right nextState -> do
                      print nextState
                      return nextState
              Nothing -> do
                putStrLn $ "Failed to parse message: " ++ show msg
                sendError conn "Failed to parse message."

    sendError conn err =
      sendTextData conn (encode (ErrorResponse err :: SocketResponse GameState))

    disconnect = do
      putStrLn "Client disconnected."

    staticAppSettings = (defaultWebAppSettings "frontend/build") {ssIndices = [unsafeToPiece "index.html"]}

    backupApp :: Application
    backupApp = staticApp staticAppSettings