module GameActionTests exposing (..)

import Card exposing (Card(..))
import Expect
import Face exposing (Face(..))
import GameAction exposing (GameAction(..))
import Json.Decode as D
import Suit exposing (Suit(..))
import Test exposing (..)


all : Test
all =
    describe "GameAction" <|
        List.map testActionSerde actions


testActionSerde : GameAction -> Test
testActionSerde action =
    test
        ("decode (encode action) == action where action = "
            ++ Debug.toString action
        )
    <|
        \_ ->
            Expect.equal
                (D.decodeValue GameAction.decode (GameAction.encode action))
                (Ok action)


actions : List GameAction
actions =
    [ Deal
    , BidPass
    , Bid 25
    , Play Joker
    , Play (FaceCard Diamonds Ace)
    , PickTrump Spades
    , Discard [ Joker, FaceCard Hearts Ace ]
    , PassCards [ Joker, FaceCard Clubs King ]
    ]
