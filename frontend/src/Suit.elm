module Suit exposing (..)

import Json.Decode as D
import Json.Encode as E


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


encode : Suit -> E.Value
encode suit =
    E.string (Debug.toString suit)


decode : D.Decoder Suit
decode =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Hearts" ->
                        D.succeed Hearts

                    "Diamonds" ->
                        D.succeed Diamonds

                    "Clubs" ->
                        D.succeed Clubs

                    "Spades" ->
                        D.succeed Spades

                    _ ->
                        D.fail ("Could not decode a Suit from '" ++ s ++ "'")
            )
