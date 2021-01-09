{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Network.Wai.Handler.Warp (runEnv)
import Server
import SixtyThree (reducerSafe)

run :: RIO App ()
run = do
  logInfo "Running!"
  liftIO $ runEnv 3000 (app reducerSafe)
