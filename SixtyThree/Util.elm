module SixtyThree.Util exposing (..)

import Set exposing (Set)
import SixtyThree.Model exposing (..)


suitToString : Suit -> String
suitToString suit =
    case suit of
        Spades ->
            "Spades"

        Clubs ->
            "Clubs"

        Hearts ->
            "Hearts"

        Diamonds ->
            "Diamonds"


suitToLowerString : Suit -> String
suitToLowerString suit =
    String.toLower <| suitToString suit


cardValueToString : CardValue -> String
cardValueToString value =
    case value of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "Jack"

        Queen ->
            "Queen"

        King ->
            "King"

        Ace ->
            "Ace"


cardValueToLowerString : CardValue -> String
cardValueToLowerString value =
    String.toLower <| cardValueToString value


cardToString : Card -> String
cardToString card =
    case card of
        SuitedCard suit value ->
            cardValueToString value ++ " of " ++ suitToString suit

        Joker ->
            "Joker"


isOppositeFive : Suit -> Card -> Bool
isOppositeFive trump card =
    case card of
        Joker ->
            False

        SuitedCard suit value ->
            if value /= Five then
                False
            else
                case trump of
                    Spades ->
                        suit == Clubs

                    Clubs ->
                        suit == Spades

                    Hearts ->
                        suit == Diamonds

                    Diamonds ->
                        suit == Hearts


pointsForCard : Suit -> Card -> Int
pointsForCard trump card =
    if isOppositeFive trump card then
        5
    else
        case card of
            Joker ->
                15

            SuitedCard suit value ->
                if trump /= suit then
                    0
                else
                    case value of
                        Ace ->
                            1

                        King ->
                            25

                        Jack ->
                            1

                        Ten ->
                            1

                        Nine ->
                            9

                        Five ->
                            5

                        Two ->
                            1

                        _ ->
                            0


allPlayers : Set Int
allPlayers =
    Set.empty
        |> Set.insert 1
        |> Set.insert 2
        |> Set.insert 3
        |> Set.insert 4


playerIdToString : PlayerId -> String
playerIdToString playerId =
    case playerId of
        PlayerOne ->
            "Player One"

        PlayerTwo ->
            "Player Two"

        PlayerThree ->
            "Player Three"

        PlayerFour ->
            "Player Four"


playerIdToInt : PlayerId -> Int
playerIdToInt playerId =
    case playerId of
        PlayerOne ->
            1

        PlayerTwo ->
            2

        PlayerThree ->
            3

        PlayerFour ->
            4


nextBiddingPlayerId : PlayerId -> Set Int -> PlayerId
nextBiddingPlayerId playerId playersPassed =
    let
        nextPlayerId =
            case playerId of
                PlayerOne ->
                    PlayerTwo

                PlayerTwo ->
                    PlayerThree

                PlayerThree ->
                    PlayerFour

                PlayerFour ->
                    PlayerOne
    in
        if Set.member (playerIdToInt nextPlayerId) playersPassed then
            nextBiddingPlayerId nextPlayerId playersPassed
        else
            nextPlayerId
