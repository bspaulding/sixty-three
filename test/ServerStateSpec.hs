module ServerStateSpec (spec) where

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

    describe "SetPlayerName" $ do
      it "sets player name sends echo back if no room yet" $ do
        let request = SetPlayerName "Bradley"
        let result = serverStateReducer g initialState connId request reducerSafe
        let state = fst <$> result
        let msgs = snd <$> result
        playerName connId <$> state `shouldBe` Right "Bradley"
        msgs `shouldBe` Right [(connId, SocketResponse.PlayerNameChanged connId "Bradley")]

      it "sets player name and broadcasts to room, if room" $ do
        pending

    describe "JoinRoom" $ do
      it "lower cases all room ids" $ do
        pending
