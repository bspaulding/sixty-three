{-# LANGUAGE OverloadedStrings #-}

-- Server and ServerState should be generic, not tied to this specific game.
-- Here's the vision:
-- - You have a ServerState GameState, which holds the lobby/room operations/room state access
-- - The Server takes a "safe" reducer for GameState, ie GameState -> GameAction -> Either String GameState
-- - Server forwards all game actions to the reducer, updating the mVar for the room if Right

module Server (app) where

import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import GameAction
import GameState
import Network.HTTP.Types ()
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Player
import ServerState
import SocketRequest
import SocketResponse
import System.Random
import WaiAppStatic.Types (unsafeToPiece)

-- TODO: why do i have to fix action and state here?!
-- app :: (ToJSON s, ToJSON a, FromJSON a, Show a, Show s) => MVar (ServerStateWS s) -> (s -> a -> Either String s) -> ([ConnId] -> s) -> Application
app :: MVar (ServerStateWS GameState) -> (GameState -> ConnId -> GameAction -> Either String GameState) -> (StdGen -> [ConnId] -> Either String GameState) -> Application
app stateM roomStateReducer roomStateInitializer = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      putStrLn "got a pending connection!"
      conn <- acceptRequest pending_conn
      connId <- UUID.toString <$> UUID.nextRandom
      let client = (connId, conn)
      -- TODO: why do i have to fix the inner SocketResponse a here?
      let idMsg = IdentifyConnection connId :: SocketResponse GameState
      modifyMVar_ stateM $ \state -> do
        return $ connect client state
      sendTextData conn (encode idMsg)
      flip finally (disconnect stateM connId) $
        withPingThread conn 30 (return ()) $
          forever $ do
            msg <- receiveData conn
            case (decode msg :: Maybe (SocketRequest GameAction)) of
              Just socketRequest -> do
                print socketRequest
                modifyMVar_ stateM $ \state -> do
                  stdGen <- getStdGen
                  case serverStateReducer roomStateReducer roomStateInitializer stdGen (serverState state) connId socketRequest of
                    Left err -> do
                      sendError conn err
                      return state
                    Right (nextState, responses) -> do
                      forM_ responses (sendResponse (clientsById state))
                      return state {serverState = nextState}
                _ <- readMVar stateM
                return ()
              Nothing -> do
                putStrLn $ "Failed to parse message: " ++ show msg
                sendError conn "Failed to parse message."

    sendError conn err =
      sendTextData conn (encode (ErrorResponse err :: SocketResponse GameState))

    sendResponse clients (connId', response) =
      case Map.lookup connId' clients of
        Just (_, conn) -> sendTextData conn (encode response)
        Nothing -> do
          putStrLn $ "sendResponse called with no connection for id " ++ connId' ++ "\n" ++ show clients
          return ()

    disconnect stateM connId = do
      modifyMVar_ stateM $ \state -> do
        return $ removeClient connId state
      _ <- readMVar stateM
      putStrLn $ "Client " ++ connId ++ "disconnected."

    staticAppSettings = (defaultWebAppSettings "frontend/build") {ssIndices = [unsafeToPiece "index.html"]}

    backupApp :: Application
    backupApp = staticApp staticAppSettings
