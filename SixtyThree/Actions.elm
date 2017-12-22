module SixtyThree.Actions exposing (..)

import Set exposing (Set)
import SixtyThree.Model exposing (..)
import SixtyThree.Util exposing (nextBiddingPlayerId, playerIdToInt)


dealRecur : ( Model, List Card ) -> ( Model, List Card )
dealRecur ( model, cards ) =
    case ( model, cards ) of
        ( _, [] ) ->
            ( model, [] )

        ( model, cards ) ->
            dealRecur
                ( { model
                    | playerOneHand =
                        model.playerOneHand ++ (List.take 3 cards)
                    , playerTwoHand =
                        model.playerTwoHand ++ (List.take 3 <| List.drop 3 cards)
                    , playerThreeHand =
                        model.playerThreeHand ++ (List.take 3 <| List.drop 6 cards)
                    , playerFourHand =
                        model.playerFourHand ++ (List.take 3 <| List.drop 9 cards)
                  }
                , List.drop 12 cards
                )


deal : Model -> Model
deal model =
    dealRecur
        ( { model
            | deck = List.drop (9 * 4) model.deck
            , kiddy = List.take 3 <| List.drop (9 * 4) model.deck
            , phase = Bidding
            , currentlyBiddingPlayer = PlayerOne
          }
        , List.take (9 * 4) model.deck
        )
        |> Tuple.first


submitBid : Model -> Model
submitBid model =
    if model.tempBid > model.currentBid then
        { model
            | currentlyBiddingPlayer = (nextBiddingPlayerId model.currentlyBiddingPlayer model.playersPassed)
            , currentBid = model.tempBid
        }
    else
        model


passBid : Model -> Model
passBid model =
    let
        playersPassed =
            Set.insert (playerIdToInt model.currentlyBiddingPlayer) model.playersPassed

        done =
            Set.size playersPassed == 3

        currentlyBiddingPlayer =
            (nextBiddingPlayerId model.currentlyBiddingPlayer playersPassed)
    in
        { model
            | currentlyBiddingPlayer = currentlyBiddingPlayer
            , playersPassed = playersPassed
            , phase =
                if done then
                    WaitingForTrump
                else
                    Bidding
        }
