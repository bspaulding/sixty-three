module GameState exposing (..)

import Card
import Json.Decode as D


type GamePlayer
    = PlayerOne
    | PlayerTwo
    | PlayerThree
    | PlayerFour


type alias GameState =
    { dealer : GamePlayer

    -- , hand : Set.Set Card.Card
    -- , cardsInPlay : Set.Set Card.Card
    , trump : Maybe Card.Suit

    -- , currentBid : Maybe ( Player, Int )
    }


gameStateDecoder : D.Decoder GameState
gameStateDecoder =
    D.map2 GameState
        (D.field "dealer" (D.string |> D.andThen player))
        (D.field "trump" (D.maybe (D.string |> D.andThen Card.suitDecoder)))


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
