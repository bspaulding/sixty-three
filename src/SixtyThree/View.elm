module SixtyThree.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SixtyThree.Model exposing (..)
import SixtyThree.Util exposing (cardToString, cardValueToLowerString, suitToLowerString, playerIdToString)


cardStyle : Attribute msg
cardStyle =
    style
        [ ( "height", "100px" ) ]


cardFace : Card -> Html Msg
cardFace card =
    case card of
        SuitedCard suit value ->
            img [ cardStyle, src ("svg-cards/" ++ (cardValueToLowerString value) ++ "_of_" ++ (suitToLowerString suit) ++ ".svg") ] []

        Joker ->
            img [ cardStyle, src "svg-cards/black_joker.svg" ] []


listCards : List Card -> Html Msg
listCards cards =
    ul [] (List.map (\card -> div [] [ cardFace card ]) cards)


cardsColumn : String -> List Card -> Html Msg
cardsColumn name cards =
    div []
        [ div [] [ text name ]
        , div [] [ text ((toString (List.length cards)) ++ " cards") ]
        , listCards cards
        ]


playerLabel : PlayerId -> String
playerLabel playerId =
    case playerId of
        PlayerOne ->
            "Player 1"

        PlayerTwo ->
            "Player 2"

        PlayerThree ->
            "Player 3"

        PlayerFour ->
            "Player 4"


biddingViewForPlayer : PlayerId -> Int -> Int -> Html Msg
biddingViewForPlayer playerId minBid bid =
    div []
        [ text ((playerLabel playerId) ++ " Bidding...")
        , input
            [ type_ "number"
            , attribute "min" (toString minBid)
            , attribute "max" "63"
            , value (toString bid)
            , onInput
                (\x ->
                    case String.toInt x of
                        Ok x ->
                            Bid x

                        Err _ ->
                            Bid bid
                )
            ]
            []
        , button [ onClick SubmitBid ] [ text "Submit Bid" ]
        , button [ onClick PassBid ] [ text "Pass Bid" ]
        ]


bidding : Model -> Html Msg
bidding model =
    biddingViewForPlayer model.currentlyBiddingPlayer model.currentBid model.tempBid


waitingForTrump : Model -> Html Msg
waitingForTrump model =
    div []
        [ text <| (playerIdToString model.currentlyBiddingPlayer) ++ " won the bid for " ++ (toString model.currentBid) ++ "."
        , button [ onClick <| SelectTrump Spades ] [ text "Spades" ]
        , button [ onClick <| SelectTrump Clubs ] [ text "Clubs" ]
        , button [ onClick <| SelectTrump Hearts ] [ text "Hearts" ]
        , button [ onClick <| SelectTrump Diamonds ] [ text "Diamonds" ]
        ]


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "display", "flex" ) ]
        ]
        [ div []
            [ div [] [ (text (toString <| List.length model.deck)) ]
            , button [ onClick ShuffleDeck ] [ text "Shuffle" ]
            , button [ onClick Deal ] [ text "Deal" ]
            , case model.phase of
                Bidding ->
                    bidding model

                WaitingForTrump ->
                    waitingForTrump model

                _ ->
                    text ""
            , listCards model.deck
            ]
        , cardsColumn "Player 1" model.playerOneHand
        , cardsColumn "Player 2" model.playerTwoHand
        , cardsColumn "Player 3" model.playerThreeHand
        , cardsColumn "Player 4" model.playerFourHand
        , cardsColumn "Kiddy" model.kiddy
        ]
