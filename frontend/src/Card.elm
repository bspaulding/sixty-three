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


suitDecoder : D.Decoder Suit
suitDecoder =
    D.string
        |> D.andThen
            (\s ->
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
            )


faceDecoder : D.Decoder Face
faceDecoder =
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


cardDecoder : D.Decoder Card
cardDecoder =
    D.oneOf [ jokerDecoder, faceCardDecoder ]


jokerDecoder : D.Decoder Card
jokerDecoder =
    D.field "tag" D.string
        |> D.andThen
            (\tag ->
                case tag of
                    "Joker" ->
                        D.succeed Joker

                    _ ->
                        D.fail <| "Unknown Card type '" ++ tag ++ "'"
            )


faceCardDecoder : D.Decoder Card
faceCardDecoder =
    let
        faceCardContents =
            D.map2 FaceCard (D.index 0 suitDecoder) (D.index 1 faceDecoder)
    in
    D.field "contents" faceCardContents
