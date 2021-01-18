module GameState exposing (..)

import Card
import Dict
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

    -- , cardsInPlay : Set.Set Card.Card
    , trump : Maybe Card.Suit

    -- , currentBid : Maybe ( Player, Int )
    }


gameStateDecoder : D.Decoder GameState
gameStateDecoder =
    D.map4 GameState
        (D.field "dealer" (D.string |> D.andThen player))
        (D.field "playerInControl" (D.string |> D.andThen player))
        (D.field "hands" (D.dict (D.list Card.cardDecoder)))
        (D.field "trump" (D.maybe Card.suitDecoder))


player : String -> D.Decoder GamePlayer
player playerString =
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
