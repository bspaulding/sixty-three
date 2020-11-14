{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Shuffle (shuffle) where

import Import
import Prelude (splitAt)
import System.Random

shuffle :: [a] -> IO [a]
shuffle ls = shuffle' ls []
  where
    shuffle' [] acc = return acc
    shuffle' l acc = do
      k <- randomRIO(0, length l - 1)
      let (lead, x:xs) = splitAt k l
      shuffle' (lead ++ xs) (x:acc)
