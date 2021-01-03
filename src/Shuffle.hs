{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Shuffle (shuffle) where

import Import
import System.Random
import Prelude (head, splitAt, tail)

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle ls = shuffle' ls []
  where
    shuffle' :: RandomGen g => [a] -> [a] -> g -> ([a], g)
    shuffle' [] acc g = (acc, g)
    shuffle' l acc g = do
      let (k, g') = randomR (0, length l - 1) g
      let (lead, xs') = splitAt k l
      let x = head xs'
      let xs = tail xs'
      shuffle' (lead ++ xs) (x : acc) g'
