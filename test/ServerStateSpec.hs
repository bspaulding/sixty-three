module ServerStateSpec (spec) where

import qualified Data.Map as Map
import ServerState
import SixtyThree (reducerSafe)
import SocketRequest
import SocketResponse
import System.Random
import Test.Hspec

spec :: Spec
spec = do
  describe "serverStateReducer" $ do
    let g = mkStdGen 1
    let initialState = newServerState
    let connId = "abcdefg"
    let roomId = "abcd"

    describe "CreateRoom" $ do
      it "creates a room with a random id" $ do
        let result = serverStateReducer g newServerState connId SocketRequest.CreateRoom reducerSafe
        let state = fst <$> result
        getRoomId connId <$> state `shouldBe` Right (Just "lcbg")

        let msgs = snd <$> result
        msgs `shouldBe` Right [(connId, SocketResponse.JoinedRoom "lcbg" (Map.fromList [(connId, "Unknown")]))]

    describe "JoinRoom" $ do
      it "lower cases all room ids" $ do
        pending

    describe "SetPlayerName" $ do
      let request = SetPlayerName "Bradley"

      it "sets player name sends echo back if no room yet" $ do
        let result = serverStateReducer g initialState connId request reducerSafe
        let state = fst <$> result
        let msgs = snd <$> result
        playerName connId <$> state `shouldBe` Right "Bradley"
        msgs `shouldBe` Right [(connId, SocketResponse.PlayerNameChanged connId "Bradley")]

      it "sets player name and broadcasts to room, if room" $ do
        let initialState = moveClientToRoom roomId connId $ moveClientToRoom roomId "123" newServerState
        let result = serverStateReducer g initialState connId request reducerSafe
        let state = fst <$> result
        let msgs = snd <$> result
        playerName connId <$> state `shouldBe` Right "Bradley"
        let expectedMsg = SocketResponse.PlayerNameChanged connId "Bradley"
        msgs `shouldBe` Right [(connId, expectedMsg), ("123", expectedMsg)]
