{-# LANGUAGE OverloadedStrings #-}

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