module GameState exposing (..)

import Card
import Dict
import HaskellMapDecoder exposing (haskellMap)
import Json.Decode as D
import Suit exposing (Suit)


type GamePlayer
    = PlayerOne
    | PlayerTwo
    | PlayerThree
    | PlayerFour


type alias GameState =
    { dealer : GamePlayer
    , playerInControl : GamePlayer
    , hands : Dict.Dict String (List Card.Card)
    , cardsInPlay : Dict.Dict String Card.Card
    , trump : Maybe Suit
    , currentBid : Maybe ( GamePlayer, Int )
    , playersByConnId : Dict.Dict String GamePlayer
    }


gameStateDecoder : D.Decoder GameState
gameStateDecoder =
    D.map7 GameState
        (D.field "dealer" gamePlayerDecoder)
        (D.field "playerInControl" gamePlayerDecoder)
        (D.field "hands" (haskellMap D.string (D.list Card.cardDecoder)))
        (D.field "cardsInPlay" (haskellMap D.string Card.cardDecoder))
        (D.field "trump" (D.maybe Suit.decode))
        (D.field "currentBid" (D.maybe (D.map2 Tuple.pair gamePlayerDecoder D.int)))
        (D.field "playersByConnId" (haskellMap D.string gamePlayerDecoder))


gamePlayerDecoder : D.Decoder GamePlayer
gamePlayerDecoder =
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
