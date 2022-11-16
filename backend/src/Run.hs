{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Network.Wai.Handler.Warp (runEnv)
import Server
import ServerState
import SixtyThree (initializer, reducerSafeConns)
import System.Random (getStdGen)

run :: RIO App ()
run = do
  logInfo "Running on port 3000"
  initialState <- newMVar newServerStateWS
  liftIO $ runEnv 3000 (app initialState reducerSafeConns initializer)
