{-# LANGUAGE OverloadedStrings #-}

-- Server and ServerState should be generic, not tied to this specific game.
-- Here's the vision:
-- - You have a ServerState GameState, which holds the lobby/room operations/room state access
-- - The Server takes a "safe" reducer for GameState, ie GameState -> GameAction -> Either String GameState
-- - Server forwards all game actions to the reducer, updating the mVar for the room if Right

module Server (app) where

import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import WaiAppStatic.Types (unsafeToPiece)

app :: Application
app = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      putStrLn "got a pending connection!"
      conn <- acceptRequest pending_conn
      putStrLn "accepted request, sending hello"
      sendTextData conn ("Hello, client!" :: T.Text)
      flip finally disconnect $
        withPingThread conn 30 (return ()) $
          forever $ do
            msg <- receiveData conn
            BS.putStrLn msg

    disconnect = do
      putStrLn "Client disconnected."

    staticAppSettings = (defaultWebAppSettings "frontend/build") {ssIndices = [unsafeToPiece "index.html"]}

    backupApp :: Application
    backupApp = staticApp staticAppSettings