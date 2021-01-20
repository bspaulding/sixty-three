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


encode : Face -> E.Value
encode face =
    E.string (Debug.toString face)


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
