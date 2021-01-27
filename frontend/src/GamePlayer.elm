module GamePlayer exposing (..)

import Json.Decode as D


type GamePlayer
    = PlayerOne
    | PlayerTwo
    | PlayerThree
    | PlayerFour


decode : D.Decoder GamePlayer
decode =
    D.string
        |> D.andThen
            (\playerString ->
                case playerString of
                    "PlayerOne" ->
                        D.succeed PlayerOne

                    "PlayerTwo" ->
                        D.succeed PlayerTwo

                    "PlayerThree" ->
                        D.succeed PlayerThree

                    "PlayerFour" ->
                        D.succeed PlayerFour

                    _ ->
                        D.fail ("Could not decode a GamePlayer from '" ++ playerString ++ "'")
            )


toString : GamePlayer -> String
toString player =
    case player of
        PlayerOne ->
            "PlayerOne"

        PlayerTwo ->
            "PlayerTwo"

        PlayerThree ->
            "PlayerThree"

        PlayerFour ->
            "PlayerFour"
