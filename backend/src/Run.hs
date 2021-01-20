{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Network.Wai.Handler.Warp (runEnv)
import Server
import ServerState
import SixtyThree (initializer, reducerSafeConns)

run :: RIO App ()
run = do
  logInfo "Running!"
  initialState <- newMVar newServerStateWS
  liftIO $ runEnv 3000 (app initialState reducerSafeConns initializer)
