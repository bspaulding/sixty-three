module SixtyThree.Update exposing (..)

import Random exposing (generate)
import SixtyThree.Actions exposing (..)
import SixtyThree.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck ->
            ( model, Random.generate DeckShuffled deckGenerator )

        DeckShuffled deck ->
            ( { model | deck = deck }, Cmd.none )

        Deal ->
            ( deal model, Cmd.none )

        Bid value ->
            ( { model | tempBid = value }, Cmd.none )

        SubmitBid ->
            ( submitBid model, Cmd.none )

        PassBid ->
            ( passBid model, Cmd.none )

        SelectTrump suit ->
            ( { model
                | trump = suit
                , phase = Redealing
              }
            , Cmd.none
            )
