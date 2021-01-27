{-# LANGUAGE OverloadedStrings #-}

module SocketRequestSpec (spec) where

import Data.Aeson
import GameAction
import Player
import SocketRequest
import Test.Hspec

spec :: Spec
spec = do
  describe "SocketRequest serde" $ do
    it "can encode" $ do
      encode (GameAction BidPass) `shouldBe` "{\"payload\":{\"type\":\"BidPass\"},\"type\":\"GameAction\"}"

    it "can decode Bid"  $ do
      (decode "{\"type\":\"GameAction\",\"payload\":{\"type\":\"Bid\",\"payload\":25}}" :: Maybe (SocketRequest GameAction))
        `shouldBe` Just (GameAction (Bid 25))

    it "can decode BidPass"  $ do
      (decode "{\"type\":\"GameAction\",\"payload\":{\"type\":\"BidPass\"}}" :: Maybe (SocketRequest GameAction))
        `shouldBe` Just (GameAction BidPass)

    it "can decode (PlayerOne, BidPass)"  $ do
      (decode "{\"type\":\"GameAction\",\"payload\":[\"PlayerOne\", {\"type\":\"BidPass\"}]}" :: Maybe (SocketRequest (Player, GameAction)))
        `shouldBe` Just (GameAction (PlayerOne, BidPass))

    it "can encode 2" $ do
      encode (GameAction (PlayerOne, BidPass)) `shouldBe` "{\"payload\":[\"PlayerOne\",{\"type\":\"BidPass\"}],\"type\":\"GameAction\"}"
