{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Network.Wai.Handler.Warp (runEnv)
import Server
import ServerState
import SixtyThree (initializer, reducerSafe)

run :: RIO App ()
run = do
  logInfo "Running!"
  initialState <- newMVar newServerStateWS
  liftIO $ runEnv 3000 (app initialState reducerSafe initializer)
