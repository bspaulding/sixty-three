module ModelTests exposing (..)

import Dict
import Expect
import GamePlayer exposing (GamePlayer(..))
import Main exposing (Model, playerName_)
import Test exposing (..)


playersById =
    Dict.empty
        |> Dict.insert "id-one" { id = "id-one", name = "one" }
        |> Dict.insert "id-two" { id = "id-two", name = "two" }
        |> Dict.insert "id-three" { id = "id-three", name = "three" }
        |> Dict.insert "id-four" { id = "id-four", name = "four" }


gamePlayersByConnId =
    Dict.fromList
        [ ( "id-one", PlayerOne )
        , ( "id-two", PlayerTwo )
        , ( "id-three", PlayerThree )
        , ( "id-four", PlayerFour )
        ]


testPlayerName =
    playerName_ playersById gamePlayersByConnId


all : Test
all =
    describe "playerName"
        [ test "returns the name for the conn id: PlayerOne" <|
            \_ -> Expect.equal (testPlayerName PlayerOne) "one"
        , test "returns the name for the conn id: PlayerTwo" <|
            \_ -> Expect.equal (testPlayerName PlayerTwo) "two"
        , test "returns the name for the conn id: PlayerThree" <|
            \_ -> Expect.equal (testPlayerName PlayerThree) "three"
        , test "returns the name for the conn id: PlayerFour" <|
            \_ -> Expect.equal (testPlayerName PlayerFour) "four"
        ]
