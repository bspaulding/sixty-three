{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Shuffle (shuffle)
import SixtyThree
import System.Random (getStdGen)

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  gen <- liftIO getStdGen
  let (shuffled, _) = shuffle deck gen
  logInfo $ display (Cards shuffled)
