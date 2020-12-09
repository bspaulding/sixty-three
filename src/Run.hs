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
  g <- liftIO getStdGen
  let (shuffled, _) = shuffle deck g
  logInfo $ display (Cards shuffled)
