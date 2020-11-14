{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Import
import Shuffle (shuffle)
import SixtyThree

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  let (Cards cards) = deck
  shuffled <- liftIO $ shuffle cards
  logInfo $ display (Cards shuffled)
