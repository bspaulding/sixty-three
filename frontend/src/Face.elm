module Face exposing (..)

import Json.Decode as D
import Json.Encode as E


type Face
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


faces : List Face
faces =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


toString : Face -> String
toString face =
    case face of
        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"

        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Jack ->
            "Jack"

        Queen ->
            "Queen"

        King ->
            "King"

        Ace ->
            "Ace"


encode : Face -> E.Value
encode face =
    E.string (toString face)


decode : D.Decoder Face
decode =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Two" ->
                        D.succeed Two

                    "Three" ->
                        D.succeed Three

                    "Four" ->
                        D.succeed Four

                    "Five" ->
                        D.succeed Five

                    "Six" ->
                        D.succeed Six

                    "Seven" ->
                        D.succeed Seven

                    "Eight" ->
                        D.succeed Eight

                    "Nine" ->
                        D.succeed Nine

                    "Ten" ->
                        D.succeed Ten

                    "Jack" ->
                        D.succeed Jack

                    "Queen" ->
                        D.succeed Queen

                    "King" ->
                        D.succeed King

                    "Ace" ->
                        D.succeed Ace

                    _ ->
                        D.fail ("Could not decode a Face from '" ++ s ++ "'")
            )


compare : Face -> Face -> Order
compare aface bface =
    Basics.compare (rank aface) (rank bface)


rank : Face -> Int
rank f =
    case f of
        Ace ->
            14

        King ->
            13

        Queen ->
            12

        Jack ->
            11

        Ten ->
            10

        Nine ->
            9

        Eight ->
            8

        Seven ->
            7

        Six ->
            6

        Five ->
            5

        Four ->
            4

        Three ->
            3

        Two ->
            2
