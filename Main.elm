module Main exposing (..)

import Html exposing (program)
import SixtyThree.Model exposing (Msg, Model, init)
import SixtyThree.Update exposing (update)
import SixtyThree.View exposing (view)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
