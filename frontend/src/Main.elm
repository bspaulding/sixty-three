port module Main exposing (..)

import Browser
import Card exposing (Card(..), cardDescription, unicard)
import Dict
import GameAction exposing (GameAction(..))
import GameState
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Maybe
import Suit exposing (Suit(..))
import WSMessage



---- PORTS ----


port receiveMessage : (String -> msg) -> Sub msg



---- MODEL ----


type alias Player =
    { id : WSMessage.ConnId
    , name : String
    }


type alias Model =
    { connId : Maybe String
    , playersById : Dict.Dict WSMessage.ConnId Player
    , roomId : Maybe WSMessage.RoomId
    , tempRoomId : String
    , tempName : String
    , gameState : Maybe GameState.GameState
    , lastErrorMsg : Maybe String
    , tempBid : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { connId = Nothing
      , playersById = Dict.empty
      , roomId = Nothing
      , tempRoomId = ""
      , tempName = ""
      , gameState = Nothing
      , lastErrorMsg = Nothing
      , tempBid = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | WebsocketSend String
    | WebsocketEventReceived String
    | CreateRoom
    | JoinRoom
    | TempRoomIdChanged String
    | TempNameChanged String
    | SubmitName
    | StartGame
    | BidChanged String
    | TakeGameAction GameAction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WebsocketSend socketMsg ->
            ( model, WSMessage.sendMessage socketMsg )

        WebsocketEventReceived eventDataStr ->
            case D.decodeString WSMessage.socketDecoder eventDataStr of
                Ok decodedMessage ->
                    Debug.log "Decoded Websocket event!" <|
                        handleWsMessage model decodedMessage

                Err err ->
                    Debug.log
                        ("Error decoding Websocket event: " ++ D.errorToString err)
                        ( model, Cmd.none )

        CreateRoom ->
            ( model, WSMessage.createRoom )

        JoinRoom ->
            ( model, WSMessage.joinRoom model.tempRoomId )

        TempRoomIdChanged roomId ->
            ( { model | tempRoomId = roomId }, Cmd.none )

        TempNameChanged name ->
            ( { model | tempName = name }, Cmd.none )

        SubmitName ->
            ( model, WSMessage.setPlayerName model.tempName )

        StartGame ->
            case model.roomId of
                Just roomId ->
                    ( model, WSMessage.initRoom roomId )

                Nothing ->
                    ( model, Cmd.none )

        BidChanged bid ->
            ( { model | tempBid = Maybe.withDefault model.tempBid (String.toInt bid) }, Cmd.none )

        TakeGameAction gameAction ->
            ( model, WSMessage.sendGameAction gameAction )


handleWsMessage : Model -> WSMessage.WSMessage -> ( Model, Cmd Msg )
handleWsMessage model wsMsg =
    case wsMsg of
        WSMessage.IdentifyConnection connId ->
            ( { model | connId = Just connId }, Cmd.none )

        WSMessage.PlayerNameChanged connId name ->
            ( { model | playersById = Dict.insert connId (Player connId name) model.playersById }, Cmd.none )

        WSMessage.PlayerJoinedRoom connId name ->
            ( { model | playersById = Dict.insert connId (Player connId name) model.playersById }, Cmd.none )

        WSMessage.JoinedRoomResponse roomId playerNamesById ->
            Debug.log "handled JoinedRoomResponse" ( { model | roomId = Just roomId, playersById = playerNamesById |> Dict.toList |> List.map (\( connId, name ) -> ( connId, Player connId name )) |> Dict.fromList }, Cmd.none )

        WSMessage.CreateRoomResponse roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        WSMessage.State gameState ->
            ( { model | gameState = Just gameState }, Cmd.none )

        WSMessage.ErrorResponse err ->
            Debug.log err ( { model | lastErrorMsg = Just err }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ case model.lastErrorMsg of
            Nothing ->
                div [] []

            Just err ->
                div [ class "error" ] [ text err ]
        , case model.roomId of
            Just roomId ->
                roomView roomId model

            Nothing ->
                case Dict.get (Maybe.withDefault "" model.connId) model.playersById of
                    Just player ->
                        createOrJoinRoomView model

                    Nothing ->
                        setNameView model
        ]


setNameView : Model -> Html Msg
setNameView model =
    div []
        [ text "Enter a name:"
        , input [ type_ "text", name "name", value model.tempName, onInput TempNameChanged ] []
        , button [ onClick SubmitName ] [ text "Submit" ]
        ]


createOrJoinRoomView : Model -> Html Msg
createOrJoinRoomView model =
    div []
        [ button [ id "create-room", onClick CreateRoom ] [ text "Create A Room" ]
        , input [ type_ "text", name "room-id", placeholder "Enter a Room ID", value model.tempRoomId, onInput TempRoomIdChanged ] []
        , button [ id "join-room", onClick JoinRoom ] [ text "Join Room" ]
        ]


gameView : Model -> String -> GameState.GameState -> Html Msg
gameView model connId state =
    div []
        [ div []
            [ span [] [ text "Enter Bid: " ]
            , input [ type_ "number", onInput BidChanged ] []
            , button [ onClick (TakeGameAction (GameAction.Bid model.tempBid)) ] [ text "SubmitBid" ]
            ]
        , case Dict.get connId state.playersByConnId of
            Nothing ->
                div [] []

            Just player ->
                case Dict.get (Debug.toString player) state.hands of
                    Nothing ->
                        div [] []

                    Just hand ->
                        div [] [ playerHandView ( Debug.toString player, hand ) ]
        ]


playerHandView : ( String, List Card ) -> Html Msg
playerHandView ( p, cards ) =
    div []
        [ text p
        , ul [] (List.map cardView cards)
        ]


cardView : Card -> Html Msg
cardView card =
    let
        suitClass =
            case card of
                Joker ->
                    "joker"

                FaceCard Spades _ ->
                    "spades"

                FaceCard Clubs _ ->
                    "clubs"

                FaceCard Hearts _ ->
                    "hearts"

                FaceCard Diamonds _ ->
                    "diamonds"
    in
    div [ class "card", class suitClass ] [ text (unicard card) ]


roomView : WSMessage.RoomId -> Model -> Html Msg
roomView roomId model =
    div []
        [ div [] [ roomDescription roomId model ]
        , case ( model.connId, model.gameState ) of
            ( Just connId, Just state ) ->
                gameView model connId state

            _ ->
                div [] [ button [ id "start-game", onClick StartGame ] [ text "Start Game" ] ]
        ]


roomDescription : String -> Model -> Html msg
roomDescription roomId model =
    let
        prefix =
            [ text "In room ", span [ id "room-id" ] [ text <| String.toUpper roomId ] ]

        namesL =
            Dict.remove (Maybe.withDefault "" model.connId) model.playersById
                |> Dict.values
                |> List.map .name
                |> List.sort

        lastName =
            List.reverse namesL |> List.head |> Maybe.withDefault "Oops"

        lenNames =
            List.length namesL

        names =
            if lenNames > 1 then
                String.join ", " (List.take (lenNames - 1) namesL)
                    ++ (if lenNames == 2 then
                            ""

                        else
                            ","
                       )
                    ++ " and "
                    ++ lastName

            else
                String.join ", " namesL

        suffix =
            if Dict.size model.playersById == 1 then
                ", all alone 😿."

            else
                " with " ++ names
    in
    span [] (prefix ++ [ text suffix ])



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMessage WebsocketEventReceived



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
