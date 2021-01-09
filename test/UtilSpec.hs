{-# LANGUAGE OverloadedStrings #-}

module UtilSpec (spec) where

import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Util

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

prop_room_id :: StdGen -> Property
prop_room_id g =
  let (roomId, g1) = makeRoomId g
      (roomId2, _) = makeRoomId g1
   in roomId =/= roomId2

prop_room_id_length :: StdGen -> Property
prop_room_id_length g =
  let (roomId, _) = makeRoomId g
   in length roomId === 4

spec :: Spec
spec = do
  describe "safeHead" $ do
    it "returns Just first element if non-empty" $ do
      safeHead [1] `shouldBe` Just 1
    it "returns nothing if empty" $ do
      safeHead ([] :: [Integer]) `shouldBe` Nothing

  describe "makeRoomId" $ do
    it "returns a four character string" $
      property prop_room_id_length
    it "returns a unique string" $
      property prop_room_id