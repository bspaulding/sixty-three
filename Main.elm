module Main exposing (..)

import Html exposing (..)


type Suit
    = Spades
    | Clubs
    | Hearts
    | Diamonds


suits : List Suit
suits =
    [ Spades
    , Clubs
    , Hearts
    , Diamonds
    ]


type CardValue
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


cardValues : List CardValue
cardValues =
    [ Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    , Ace
    ]


type alias Card =
    ( Suit, CardValue )


type alias Model =
    { deck : List Card }


type Msg
    = None


startDeck : List Card
startDeck =
    List.map2 (,) suits cardValues


init : ( Model, Cmd Msg )
init =
    ( Model startDeck, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ text "hello" ]


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
