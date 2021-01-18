module GameState exposing (..)

import Card
import Dict
import HaskellMapDecoder exposing (haskellMap)
import Json.Decode as D
import List


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
    , trump : Maybe Card.Suit
    , currentBid : Maybe ( GamePlayer, Int )
    }


gameStateDecoder : D.Decoder GameState
gameStateDecoder =
    D.map6 GameState
        (D.field "dealer" player)
        (D.field "playerInControl" player)
        (D.field "hands" (haskellMap D.string (D.list Card.cardDecoder)))
        (D.field "cardsInPlay" (haskellMap D.string Card.cardDecoder))
        (D.field "trump" (D.maybe Card.suitDecoder))
        (D.field "currentBid" (D.maybe (D.map2 Tuple.pair player D.int)))


player : D.Decoder GamePlayer
player =
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
