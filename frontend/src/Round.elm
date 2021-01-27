module Round exposing (..)

import Dict exposing (Dict)
import GamePlayer exposing (GamePlayer)
import HaskellMapDecoder exposing (haskellMap)
import Json.Decode as D


type alias Round =
    ( ( GamePlayer, Int ), List ( String, Int ) )


decode : D.Decoder Round
decode =
    D.map2 Tuple.pair
        (D.index 0 decodeWinnerAndBid)
        (D.index 1 decodePlayersAndPoints)


decodeWinnerAndBid =
    D.map2 Tuple.pair (D.index 0 GamePlayer.decode) (D.index 1 D.int)


decodePlayersAndPoints =
    D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.int))
