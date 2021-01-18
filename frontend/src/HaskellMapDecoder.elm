module HaskellMapDecoder exposing (..)

import Dict
import Json.Decode as D


haskellMap : D.Decoder comparable -> D.Decoder v -> D.Decoder (Dict.Dict comparable v)
haskellMap keyDecoder valueDecoder =
    D.list (haskellMapEntry keyDecoder valueDecoder)
        |> D.andThen (\pairs -> D.succeed (Dict.fromList pairs))


haskellMapEntry : D.Decoder comparable -> D.Decoder v -> D.Decoder ( comparable, v )
haskellMapEntry keyDecoder valueDecoder =
    D.map2 Tuple.pair
        (D.index 0 keyDecoder)
        (D.index 1 valueDecoder)
