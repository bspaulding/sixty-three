module Tests exposing (..)

import Card exposing (..)
import Expect
import Json.Decode as D
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "card decoders"
        [ test "face card" <|
            \_ ->
                Expect.equal
                    (D.decodeString cardDecoder "{\"tag\":\"FaceCard\",\"contents\":[\"Diamonds\",\"Seven\"]}")
                    (Ok (FaceCard Diamonds Seven))
        , test "joker" <|
            \_ ->
                Expect.equal
                    (D.decodeString cardDecoder "{\"tag\":\"Joker\"}")
                    (Ok Joker)
        ]
