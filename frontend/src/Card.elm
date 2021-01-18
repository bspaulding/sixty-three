module Card exposing (..)

import Json.Decode as D


type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


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


type Card
    = FaceCard Suit Face
    | Joker


suitDecoder : String -> D.Decoder Suit
suitDecoder s =
    case s of
        "Hearts" ->
            D.succeed Hearts

        "Diamonds" ->
            D.succeed Diamonds

        "Clubs" ->
            D.succeed Clubs

        "Spades" ->
            D.succeed Spades

        _ ->
            D.fail ("Could not decode a Suit from '" ++ s ++ "'")
