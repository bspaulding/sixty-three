module ServerStateSpec (spec) where

import qualified Data.Map as Map
import ServerState
import SocketRequest
import SocketResponse
import System.Random
import Test.Hspec

data TestAction = Update | MakeError

data TestState = TestState {s :: String, connIds :: [ConnId]}
  deriving (Eq, Show)

testInitializer :: [ConnId] -> Either String TestState
testInitializer connIds =
  if null connIds
    then Left "nobody in the room yet!"
    else Right $ initialRoomState connIds

initialRoomState :: [ConnId] -> TestState
initialRoomState connIds = TestState {s = "initial", connIds = connIds}

testReducer :: TestState -> ConnId -> TestAction -> Either String TestState
testReducer s connId a =
  case a of
    Update -> Right s {s = "updated"}
    MakeError -> Left "oops"

spec :: Spec
spec = do
  describe "serverStateReducer" $ do
    let g = mkStdGen 1
    let reducer = serverStateReducer testReducer testInitializer g
    let connId = "abcdefg"
    let roomId = "abcd"

    describe "CreateRoom" $ do
      it "creates a room with a random id" $ do
        let result = reducer newServerState connId SocketRequest.CreateRoom
        let state = fst <$> result
        getRoomId connId <$> state `shouldBe` Right (Just "lcbg")

        let msgs = snd <$> result
        msgs `shouldBe` Right [(connId, SocketResponse.JoinedRoom "lcbg" (Map.fromList [(connId, "Unknown")]))]

    describe "JoinRoom" $ do
      let request = SocketRequest.JoinRoom "LCBG"

      it "lower cases all room ids" $ do
        let initialState = moveClientToRoom "lcbg" "creator" newServerState
        let result = reducer initialState connId request
        let state = fst <$> result
        getRoomId connId <$> state `shouldBe` Right (Just "lcbg")

      it "returns error if room does not exist" $ do
        let result = reducer newServerState connId request
        result `shouldBe` Left "No room with id 'lcbg' exists."

    describe "SetPlayerName" $ do
      let request = SetPlayerName "Bradley"

      it "sets player name sends echo back if no room yet" $ do
        let result = reducer newServerState connId request
        let state = fst <$> result
        let msgs = snd <$> result
        playerName connId <$> state `shouldBe` Right "Bradley"
        msgs `shouldBe` Right [(connId, SocketResponse.PlayerNameChanged connId "Bradley")]

      it "sets player name and broadcasts to room, if room" $ do
        let initialState = moveClientToRoom roomId connId $ moveClientToRoom roomId "123" newServerState
        let result = reducer initialState connId request
        let state = fst <$> result
        let msgs = snd <$> result
        playerName connId <$> state `shouldBe` Right "Bradley"
        let expectedMsg = SocketResponse.PlayerNameChanged connId "Bradley"
        msgs `shouldBe` Right [(connId, expectedMsg), ("123", expectedMsg)]

    describe "GameAction" $ do
      let roomState = "hello"
      let initialState = setStateInRoom roomId (initialRoomState [connId]) $ moveClientToRoom roomId connId newServerState

      it "updates room state with room reducer" $ do
        let result = reducer initialState connId (GameAction Update)
        let state = fst <$> result
        let expectedState = TestState {s = "updated", connIds = [connId]}
        getStateInRoom roomId <$> state `shouldBe` Right (Just expectedState)
        let msgs = snd <$> result
        msgs `shouldBe` Right [(connId, SocketResponse.State expectedState)]

      it "returns any left from room reducer" $ do
        let result = reducer initialState connId (GameAction MakeError)
        result `shouldBe` Left "oops"

    describe "InitRoom" $ do
      it "runs the initializer and updates the room state" $ do
        let initialState = moveClientToRoom roomId connId newServerState
        let result = reducer initialState connId (InitRoom roomId)
        let expectedState = testInitializer [connId]
        (getStateInRoom roomId . fst <$> result) `shouldBe` Just <$> expectedState
        snd <$> result `shouldBe` (\s -> [(connId, SocketResponse.State s)]) <$> expectedState

      it "forwards initializer error" $ do
        let result = reducer newServerState connId (InitRoom roomId)
        result `shouldBe` Left "nobody in the room yet!"
