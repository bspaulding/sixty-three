module RoundTests exposing (..)

import Dict
import Expect
import GamePlayer exposing (GamePlayer(..))
import Json.Decode as D
import Round
import Test exposing (..)


all : Test
all =
    describe "Round.decode"
        [ test "can decode a round" <|
            \_ ->
                Expect.equal
                    (D.decodeString Round.decode "[[\"PlayerOne\",40],[[\"PlayerOne\",63],[\"PlayerTwo\",0],[\"PlayerThree\",0],[\"PlayerFour\",0]]]")
                    (Ok ( ( PlayerOne, 40 ), [ ( "PlayerOne", 63 ), ( "PlayerTwo", 0 ), ( "PlayerThree", 0 ), ( "PlayerFour", 0 ) ] ))
        , test "can decode winner and bid" <|
            \_ ->
                Expect.equal
                    (D.decodeString Round.decodeWinnerAndBid "[\"PlayerOne\",40]")
                    (Ok ( PlayerOne, 40 ))
        , test "can decode players and points" <|
            \_ ->
                Expect.equal
                    (D.decodeString Round.decodePlayersAndPoints "[[\"PlayerOne\",63]]")
                    (Ok [ ( "PlayerOne", 63 ) ])
        ]
