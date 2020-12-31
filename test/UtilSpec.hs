{-# LANGUAGE OverloadedStrings #-}

module UtilSpec (spec) where

import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "safeHead" $ do
    it "returns Just first element if non-empty" $ do
      safeHead [1] `shouldBe` Just 1
    it "returns nothing if empty" $ do
      safeHead ([] :: [Integer]) `shouldBe` Nothing