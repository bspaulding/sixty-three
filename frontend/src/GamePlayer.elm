module GamePlayer exposing (..)


type GamePlayer
    = PlayerOne
    | PlayerTwo
    | PlayerThree
    | PlayerFour


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
