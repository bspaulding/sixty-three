port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Set
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
    }


init : ( Model, Cmd Msg )
init =
    ( { connId = Nothing
      , playersById = Dict.empty
      , roomId = Nothing
      , tempRoomId = ""
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

        WSMessage.ErrorResponse err ->
            Debug.log err ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.roomId of
        Just roomId ->
            roomView roomId model

        Nothing ->
            createOrJoinRoomView model


createOrJoinRoomView : Model -> Html Msg
createOrJoinRoomView model =
    div []
        [ button [ onClick CreateRoom ] [ text "Create A Room" ]
        , input [ type_ "text", placeholder "Enter a Room ID", value model.tempRoomId, onInput TempRoomIdChanged ] []
        , button [ onClick JoinRoom ] [ text "Join Room" ]
        ]


roomView : WSMessage.RoomId -> Model -> Html Msg
roomView roomId model =
    div []
        [ text "TODO: roomView"
        , text ("Room " ++ roomId)
        , div [] [ text (roomDescription model) ]
        ]


roomDescription : Model -> String
roomDescription model =
    let
        prefix =
            "In room " ++ String.toUpper (Maybe.withDefault "unknown" model.roomId)

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
    prefix ++ suffix



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
