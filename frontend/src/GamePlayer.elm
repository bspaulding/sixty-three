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
                case fromString playerString of
                    Just player ->
                        D.succeed player

                    Nothing ->
                        D.fail ("Could not decode a GamePlayer from '" ++ playerString ++ "'")
            )


fromString : String -> Maybe GamePlayer
fromString playerString =
    case playerString of
        "PlayerOne" ->
            Just PlayerOne

        "PlayerTwo" ->
            Just PlayerTwo

        "PlayerThree" ->
            Just PlayerThree

        "PlayerFour" ->
            Just PlayerFour

        _ ->
            Nothing


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
