port module WSMessage exposing (..)

import Dict
import Json.Decode as D
import Json.Encode as E


port sendMessage : String -> Cmd msg


type alias ConnId =
    String


type alias RoomId =
    String


type WSMessage
    = ErrorResponse String
    | IdentifyConnection ConnId
    | CreateRoomResponse RoomId
    | PlayerJoinedRoom ConnId String
    | JoinedRoomResponse RoomId (Dict.Dict ConnId String)
    | PlayerNameChanged ConnId String



-- | GameUpdated (List ConnId) (List ConnId) ConnId ConnId ConnId ConnId String (List String) (List String) Int Int Possession


socketDecoder : D.Decoder WSMessage
socketDecoder =
    D.field "type" D.string
        |> D.andThen wsMessageDecoder


wsMessageDecoder : String -> D.Decoder WSMessage
wsMessageDecoder type_ =
    case type_ of
        "IdentifyConnection" ->
            D.map IdentifyConnection (D.field "payload" D.string)

        "ErrorResponse" ->
            D.map ErrorResponse (D.field "payload" D.string)

        "CreateRoomResponse" ->
            D.map CreateRoomResponse (D.field "roomId" D.string)

        "JoinedRoom" ->
            D.map2 PlayerJoinedRoom (D.field "connId" D.string) (D.field "name" D.string)

        "JoinRoomResponse" ->
            D.map2 JoinedRoomResponse
                (D.field "roomId" D.string)
                (D.field "playerNamesById" (D.dict D.string))

        "PlayerNameChanged" ->
            D.map2 PlayerNameChanged (D.field "connId" D.string) (D.field "name" D.string)

        _ ->
            D.fail <| "Unknown message type '" ++ type_ ++ "'"


createRoom : Cmd msg
createRoom =
    E.object [ ( "type", E.string "CreateRoom" ) ]
        |> E.encode 0
        |> sendMessage


joinRoom : RoomId -> Cmd msg
joinRoom roomId =
    E.object [ ( "type", E.string "JoinRoom" ), ( "payload", E.string roomId ) ]
        |> E.encode 0
        |> sendMessage
