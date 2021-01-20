module Tests exposing (..)

import Card exposing (..)
import Dict
import Expect
import Face exposing (Face(..))
import HaskellMapDecoder exposing (haskellMap)
import Json.Decode as D
import Suit exposing (..)
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
        , test "haskell map as list" <|
            \_ ->
                Expect.equal
                    (D.decodeString (haskellMap D.string D.int) "[[\"foo\", 0],[\"bar\",1]]")
                    (Ok (Dict.fromList [ ( "foo", 0 ), ( "bar", 1 ) ]))
        , test "haskell map of cards" <|
            \_ ->
                Expect.equal
                    (D.decodeString (haskellMap D.string (D.list cardDecoder)) "[[\"PlayerOne\",[{\"tag\":\"Joker\"}]]]")
                    (Ok (Dict.fromList [ ( "PlayerOne", [ Joker ] ) ]))
        ]
