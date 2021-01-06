{-# LANGUAGE OverloadedStrings #-}

-- Server and ServerState should be generic, not tied to this specific game.
-- Here's the vision:
-- - You have a ServerState GameState, which holds the lobby/room operations/room state access
-- - The Server takes a "safe" reducer for GameState, ie GameState -> GameAction -> Either String GameState
-- - Server forwards all game actions to the reducer, updating the mVar for the room if Right

module Server (app) where

import Data.Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.WebSockets

app :: Application
app = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      conn <- acceptRequest pending_conn
      sendTextData conn ("Hello, client!" :: Text)

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"