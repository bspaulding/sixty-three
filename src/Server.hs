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
import System.Random
import WaiAppStatic.Types (unsafeToPiece)

app :: Show s => ToJSON s => MVar (ServerState s) -> (s -> a -> Either String s) -> Application
app stateM roomStateReducer = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      putStrLn "got a pending connection!"
      conn <- acceptRequest pending_conn
      connId <- UUID.toString <$> UUID.nextRandom
      let client = (connId, conn)
      let idMsg = IdentifyConnection connId :: SocketResponse GameState
      modifyMVar_ stateM $ \state -> do
        return $ connect client state
      sendTextData conn (encode idMsg)
      flip finally disconnect $
        withPingThread conn 30 (return ()) $
          forever $ do
            msg <- receiveData conn
            case decode msg :: Maybe SocketRequest of
              Just socketRequest -> do
                print socketRequest
                modifyMVar_ stateM $ \state -> do
                  stdGen <- getStdGen
                  case serverStateReducer stdGen state client socketRequest roomStateReducer of
                    Left err -> do
                      sendError conn err
                      return state
                    Right (nextState, responses) -> do
                      print nextState
                      print responses
                      forM_ responses sendResponse
                      return nextState
                _ <- readMVar stateM
                return ()
              Nothing -> do
                putStrLn $ "Failed to parse message: " ++ show msg
                sendError conn "Failed to parse message."

    sendError conn err =
      sendTextData conn (encode (ErrorResponse err :: SocketResponse GameState))

    sendResponse ((_, conn), response) =
      sendTextData conn (encode response)

    disconnect = do
      putStrLn "Client disconnected."

    staticAppSettings = (defaultWebAppSettings "frontend/build") {ssIndices = [unsafeToPiece "index.html"]}

    backupApp :: Application
    backupApp = staticApp staticAppSettings
