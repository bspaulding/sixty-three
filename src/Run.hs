{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Network.Wai.Handler.Warp (runEnv)
import Server

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  liftIO $ runEnv 3000 app
