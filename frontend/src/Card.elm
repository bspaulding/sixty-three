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


unicardBack : String
unicardBack =
    String.fromChar <| Char.fromCode 0x0001F0A0


unicard : Card -> String
unicard card =
    String.fromChar <|
        Char.fromCode <|
            case card of
                Joker ->
                    0x0001F0DF

                FaceCard suit face ->
                    0x0001F0A1 + suitInc suit + faceInc face


suitInc : Suit -> Int
suitInc suit =
    let
        m =
            case suit of
                Spades ->
                    0

                Hearts ->
                    1

                Diamonds ->
                    2

                Clubs ->
                    3
    in
    m * 0x10


faceInc : Face -> Int
faceInc face =
    case face of
        Two ->
            0x01

        Three ->
            0x02

        Four ->
            0x03

        Five ->
            0x04

        Six ->
            0x05

        Seven ->
            0x06

        Eight ->
            0x07

        Nine ->
            0x08

        Ten ->
            0x09

        Jack ->
            0x0A

        Queen ->
            0x0C

        King ->
            0x0D

        Ace ->
            0x00


cardDescription : Card -> String
cardDescription card =
    case card of
        Joker ->
            "Joker"

        FaceCard suit face ->
            faceDescription face ++ " of " ++ suitDescription suit


suitDescription : Suit -> String
suitDescription suit =
    case suit of
        Spades ->
            "Spades"

        Hearts ->
            "Hearts"

        Diamonds ->
            "Diamonds"

        Clubs ->
            "Clubs"


faceDescription : Face -> String
faceDescription face =
    case face of
        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"

        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Jack ->
            "Jack"

        Queen ->
            "Queen"

        King ->
            "King"

        Ace ->
            "Ace"
