module Modal exposing (..)


type Modal
    = CardRanksModal


id : Modal -> String
id modal =
    case modal of
        CardRanksModal ->
            "CardRanksModal"
