module GameAction exposing (..)

import Card exposing (Card(..))
import Json.Decode as D
import Json.Encode as E
import Suit exposing (Suit)


type GameAction
    = Deal
    | BidPass
    | Bid Int
    | Play Card
    | PickTrump Suit
    | Discard (List Card)
    | PassCards (List Card)


encode : GameAction -> E.Value
encode action =
    let
        ( t, p ) =
            actionValues action
    in
    E.object [ ( "type", E.string t ), ( "payload", p ) ]


decode : D.Decoder GameAction
decode =
    D.field "type" D.string
        |> D.andThen
            (\t ->
                case t of
                    "Deal" ->
                        D.succeed Deal

                    "BidPass" ->
                        D.succeed BidPass

                    "Bid" ->
                        D.map Bid (D.field "payload" D.int)

                    "Play" ->
                        D.map Play (D.field "payload" Card.decode)

                    "PickTrump" ->
                        D.map PickTrump (D.field "payload" Suit.decode)

                    "Discard" ->
                        D.map Discard (D.field "payload" (D.list Card.decode))

                    "PassCards" ->
                        D.map PassCards (D.field "payload" (D.list Card.decode))

                    _ ->
                        D.fail ("Could not decode GameAction of type '" ++ t ++ "'")
            )


actionValues : GameAction -> ( String, E.Value )
actionValues action =
    case action of
        Deal ->
            ( "Deal", E.null )

        BidPass ->
            ( "BidPass", E.null )

        Bid bid ->
            ( "Bid", E.int bid )

        Play card ->
            ( "Play", Card.encode card )

        PickTrump suit ->
            ( "PickTrump", Suit.encode suit )

        Discard cards ->
            ( "Discard", E.list Card.encode cards )

        PassCards cards ->
            ( "PassCards", E.list Card.encode cards )
