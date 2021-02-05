port module Main exposing (..)

import Browser
import Card exposing (Card(..), unicard)
import Dict
import Face exposing (Face(..))
import GameAction exposing (GameAction(..))
import GamePlayer exposing (GamePlayer(..))
import GameState
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Maybe
import Modal exposing (Modal(..))
import Set exposing (Set)
import String
import Suit exposing (Suit(..))
import Tuple
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
    , selectedCards : Set String -- CardId really just Card.toString
    , decodingErrors : List D.Error
    , debugMode : Bool
    , modals : Dict.Dict String Bool
    }


defaultInitState =
    { connId = Nothing
    , playersById = Dict.empty
    , roomId = Nothing
    , tempRoomId = ""
    , tempName = ""
    , gameState = Nothing
    , lastErrorMsg = Nothing
    , tempBid = 0
    , selectedCards = Set.empty
    , decodingErrors = []
    , debugMode = False
    , modals = Dict.empty
    }


midGameInitState : Model
midGameInitState =
    { connId = Just "player-three"
    , modals = Dict.empty
    , playersById =
        Dict.fromList
            [ ( "player-three", { id = "player-three", name = "player-three" } )
            , ( "player-one", { id = "player-one", name = "player-one" } )
            , ( "player-two", { id = "player-two", name = "player-two" } )
            , ( "player-four", { id = "player-four", name = "player-four" } )
            ]
    , roomId = Just "prtc"
    , tempRoomId = ""
    , tempName = ""
    , tempBid = 0
    , selectedCards = Set.empty
    , decodingErrors = []
    , lastErrorMsg = Nothing
    , gameState =
        Just
            { bidPassed = Dict.fromList [ ( "PlayerOne", True ), ( "PlayerThree", True ), ( "PlayerFour", True ) ]
            , cardsInPlay = Dict.fromList []
            , currentBid = Just ( PlayerFour, 25 )
            , hands =
                Dict.fromList
                    [ ( "PlayerFour", [ FaceCard Diamonds Three, FaceCard Diamonds Nine, FaceCard Diamonds Jack, FaceCard Diamonds King, FaceCard Spades Three ] )
                    , ( "PlayerOne", [ FaceCard Hearts Seven, FaceCard Diamonds Five, FaceCard Clubs Six, FaceCard Clubs Eight, FaceCard Spades Five ] )
                    , ( "PlayerThree", [ FaceCard Hearts Four, FaceCard Hearts Five, FaceCard Diamonds Six, FaceCard Diamonds Seven, FaceCard Spades Two ] )
                    , ( "PlayerTwo", [ FaceCard Hearts Two, FaceCard Diamonds Two, FaceCard Diamonds Four, FaceCard Diamonds Ten, FaceCard Clubs Two ] )
                    ]
            , playerInControl = PlayerFour
            , playersByConnId = Dict.fromList [ ( "player-four", PlayerFour ), ( "player-three", PlayerThree ), ( "player-one", PlayerOne ), ( "player-two", PlayerTwo ) ]
            , trump = Just Diamonds
            , previousRounds = []
            , tricks =
                [ Dict.fromList [ ( "PlayerFour", FaceCard Diamonds Ace ), ( "PlayerThree", FaceCard Diamonds Queen ), ( "PlayerTwo", Joker ), ( "PlayerOne", FaceCard Diamonds Eight ) ]
                ]
            }
    , debugMode = True
    }


init : ( Model, Cmd Msg )
init =
    ( midGameInitState
      --defaultInitState
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
    | CardSelected Card
    | DiscardSelected
    | PassSelected
    | ToggleModal Modal


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
                    handleWsMessage model decodedMessage

                Err e ->
                    ( { model | decodingErrors = [ e ] ++ model.decodingErrors }, Cmd.none )

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

        CardSelected card ->
            if weAreTricking model then
                ( model, WSMessage.sendGameAction (Play card) )

            else
                ( { model
                    | selectedCards =
                        let
                            cardKey =
                                Card.toString card
                        in
                        if Set.member cardKey model.selectedCards then
                            Set.remove cardKey model.selectedCards

                        else
                            Set.insert cardKey model.selectedCards
                  }
                , Cmd.none
                  -- cardAction model.gameState card
                )

        DiscardSelected ->
            ( { model | selectedCards = Set.empty }
            , WSMessage.sendGameAction (Discard (getSelectedCards model))
            )

        PassSelected ->
            ( { model | selectedCards = Set.empty }
            , WSMessage.sendGameAction (PassCards (getSelectedCards model))
            )

        ToggleModal m ->
            let
                modalId =
                    Modal.id m
            in
            ( { model
                | modals =
                    if Maybe.withDefault False (Dict.get modalId model.modals) then
                        Dict.remove modalId model.modals

                    else
                        Dict.insert modalId True model.modals
              }
            , Cmd.none
            )


weAreTricking : Model -> Bool
weAreTricking model =
    case model.gameState of
        Nothing ->
            False

        Just state ->
            GameState.trumpSelected state && GameState.biddingOver state && GameState.allPlayersDiscarded state


getSelectedCards : Model -> List Card
getSelectedCards model =
    let
        hand =
            case ( model.connId, model.gameState ) of
                ( Just connId, Just game ) ->
                    case Dict.get connId game.playersByConnId of
                        Just gamePlayer ->
                            Maybe.withDefault [] <| Dict.get (GamePlayer.toString gamePlayer) game.hands

                        Nothing ->
                            []

                _ ->
                    []
    in
    List.filter (\c -> Set.member (Card.toString c) model.selectedCards) hand


cardAction : Maybe GameState.GameState -> Card -> Cmd msg
cardAction gameState card =
    case gameState of
        Nothing ->
            Cmd.none

        Just game ->
            if GameState.allPlayersDiscarded game then
                WSMessage.sendGameAction (Play card)

            else
                WSMessage.sendGameAction (Discard [ card ])


handleWsMessage : Model -> WSMessage.WSMessage -> ( Model, Cmd Msg )
handleWsMessage model wsMsg =
    case wsMsg of
        WSMessage.IdentifyConnection connId ->
            if model.debugMode then
                ( model, Cmd.none )

            else
                ( { model | connId = Just connId }, Cmd.none )

        WSMessage.PlayerNameChanged connId name ->
            ( { model | playersById = Dict.insert connId (Player connId name) model.playersById }, Cmd.none )

        WSMessage.PlayerJoinedRoom connId name ->
            ( { model | playersById = Dict.insert connId (Player connId name) model.playersById }, Cmd.none )

        WSMessage.JoinedRoomResponse roomId playerNamesById ->
            ( { model | roomId = Just roomId, playersById = playerNamesById |> Dict.toList |> List.map (\( connId, name ) -> ( connId, Player connId name )) |> Dict.fromList }, Cmd.none )

        WSMessage.CreateRoomResponse roomId ->
            ( { model | roomId = Just roomId }, Cmd.none )

        WSMessage.State gameState ->
            ( { model | gameState = Just gameState }, Cmd.none )

        WSMessage.ErrorResponse err ->
            ( { model | lastErrorMsg = Just err }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ case model.lastErrorMsg of
            Nothing ->
                div [] []

            Just err ->
                div [ class "error" ] [ text err ]
        , div [] (List.map (\e -> pre [ class "error" ] [ text (D.errorToString e) ]) model.decodingErrors)
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


cardRanksView : Html Msg
cardRanksView =
    let
        trump =
            Hearts

        cards =
            [ Joker ]
                ++ List.map (FaceCard trump) Face.faces

        sortedCards =
            List.sortWith (Card.compare trump) cards |> List.reverse
    in
    div [ class "card-ranks-view" ]
        [ table []
            [ thead [] [ th [] [ text "Card" ], th [] [ text "Points" ] ]
            , tbody [] (List.map cardRanksViewItem sortedCards)
            ]
        ]


modal : Bool -> msg -> List (Html msg) -> Html msg
modal isVisible onCloseMsg contents =
    if isVisible then
        div [ class "modal" ]
            [ div [ class "modal-backdrop" ] []
            , div [ class "modal-inner" ]
                [ button [ class "reset", class "modal-close", onClick onCloseMsg ] [ text "x" ]
                , div [ class "modal-contents" ] contents
                ]
            ]

    else
        div [] []


cardRanksViewItem : Card -> Html Msg
cardRanksViewItem card =
    let
        faceStr =
            case card of
                FaceCard _ face ->
                    Face.toString face

                Joker ->
                    "Joker"
    in
    tr []
        [ td [] [ text faceStr ], td [] [ text <| String.fromInt (Card.scoreIfTrump card) ] ]


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


scoreView : GameState.GameState -> Html Msg
scoreView state =
    let
        ( teamOddScore, teamEvenScore ) =
            GameState.getTotalScore state
    in
    div [ class "scores" ]
        [ table []
            [ thead []
                [ tr [] [ th [ colspan 2 ] [ text "Score" ] ]
                , tr []
                    [ th [] [ text "Player 1/3" ]
                    , th [] [ text "Player 2/4" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text (String.fromInt teamOddScore) ]
                    , td [] [ text (String.fromInt teamEvenScore) ]
                    ]
                ]
            ]
        ]


gameView : Model -> GamePlayer -> GameState.GameState -> Html Msg
gameView model player state =
    div []
        [ scoreView state
        , case state.currentBid of
            Nothing ->
                div [] [ text "No one has bid yet." ]

            Just ( bidPlayer, bid ) ->
                div [] [ text <| "The bid belongs to " ++ playerName model bidPlayer ++ " at " ++ String.fromInt bid ]
        , if GameState.biddingOver state then
            case state.trump of
                Nothing ->
                    if state.playerInControl == player then
                        selectTrumpForm

                    else
                        div [] [ text "ok, bidding is over! waiting on trump to be selected" ]

                Just suit ->
                    div []
                        [ text <| "trump is " ++ Suit.toString suit
                        , if List.length (Maybe.withDefault [] (Dict.get (GamePlayer.toString player) state.hands)) <= 6 then
                            if GameState.allPlayersDiscarded state then
                                let
                                    cardsInPlay =
                                        if Dict.isEmpty state.cardsInPlay then
                                            Maybe.withDefault Dict.empty (List.head state.tricks)

                                        else
                                            state.cardsInPlay
                                in
                                div []
                                    [ if state.playerInControl == player then
                                        div [] [ text "click a card to play it" ]

                                      else
                                        div [] [ text ("waiting for " ++ playerName model state.playerInControl ++ " to play a card.") ]
                                    , div [ class "winning-player" ]
                                        [ if Dict.size cardsInPlay == 4 then
                                            case GameState.scoreTrick suit cardsInPlay of
                                                ( winningPlayer, score ) ->
                                                    text (playerName model winningPlayer ++ " takes the trick for " ++ String.fromInt score ++ " points.")

                                          else
                                            text ""
                                        ]
                                    , cardsInPlayView player cardsInPlay
                                    , if Maybe.withDefault False <| Dict.get (Modal.id CardRanksModal) model.modals then
                                        modal True (ToggleModal CardRanksModal) [ cardRanksView ]

                                      else
                                        div [] []
                                    , button [ onClick (ToggleModal CardRanksModal) ] [ text "Show Card Values" ]
                                    ]

                            else
                                div [] [ text "you have discarded! waiting for everyone else..." ]

                          else
                            div [] [ text "click on cards to select them" ]
                        ]

          else if state.playerInControl == player then
            bidFormView model.tempBid

          else
            div [] [ text <| "Waiting for " ++ playerName model state.playerInControl ]
        , case Dict.get (GamePlayer.toString player) state.hands of
            Nothing ->
                div [] []

            Just hand ->
                div []
                    [ if GameState.trumpSelected state && GameState.biddingOver state && not (GameState.allPlayersDiscarded state) then
                        div []
                            [ button [ onClick DiscardSelected ] [ text "Discard" ]
                            , button [ onClick PassSelected ] [ text "Pass" ]
                            ]

                      else
                        text ""
                    , playerHandView (playerName model player) hand (cardIsSelected model)
                    ]
        ]


cardIsSelected : Model -> Card -> Bool
cardIsSelected model card =
    Set.member (Card.toString card) model.selectedCards


sortCardsByPlayer : ( String, Card ) -> Int
sortCardsByPlayer ( playerStr, _ ) =
    case playerStr of
        "PlayerOne" ->
            1

        "PlayerTwo" ->
            2

        "PlayerThree" ->
            3

        "PlayerFour" ->
            4

        _ ->
            -1


gamePlayers =
    [ PlayerOne, PlayerTwo, PlayerThree, PlayerFour ]


cardsInPlayView : GamePlayer -> Dict.Dict String Card.Card -> Html Msg
cardsInPlayView povPlayer cardByPlayer =
    div [ class "cards-in-play", class (GamePlayer.toString povPlayer) ] <|
        List.indexedMap
            (\i p ->
                div []
                    [ case Dict.get (GamePlayer.toString p) cardByPlayer of
                        Just card ->
                            cardView (\_ -> False) card

                        Nothing ->
                            cardBackView
                    , div [] [ text (GamePlayer.toString p) ]
                    ]
            )
            gamePlayers


selectTrumpForm : Html Msg
selectTrumpForm =
    div []
        [ text "Congrats, you won the bid!"
        , div []
            [ text "Please select a trump suit:"
            , button [ onClick (TakeGameAction (PickTrump Hearts)) ] [ text "Hearts" ]
            , button [ onClick (TakeGameAction (PickTrump Diamonds)) ] [ text "Diamonds" ]
            , button [ onClick (TakeGameAction (PickTrump Clubs)) ] [ text "Clubs" ]
            , button [ onClick (TakeGameAction (PickTrump Spades)) ] [ text "Spades" ]
            ]
        ]



-- TODO: playerName should look up connId for player and name for connId


playerName : Model -> GamePlayer -> String
playerName model player =
    case model.gameState of
        Nothing ->
            "Unknown"

        Just state ->
            playerName_ model.playersById state.playersByConnId player


tupleFlip t =
    ( Tuple.second t, Tuple.first t )


dictFlip d =
    Dict.toList d
        |> List.map tupleFlip
        |> Dict.fromList


playerName_ playersByConnId gamePlayersByConnId player =
    let
        connIdsByGamePlayer =
            dictFlip <| Dict.map (\k v -> GamePlayer.toString v) gamePlayersByConnId

        connId =
            Maybe.withDefault "" <|
                Dict.get (GamePlayer.toString player) connIdsByGamePlayer
    in
    case Dict.get connId playersByConnId of
        Nothing ->
            "Unknown"

        Just p ->
            p.name


bidFormView : Int -> Html Msg
bidFormView tempBid =
    div []
        [ span [] [ text "Enter Bid: " ]
        , input [ type_ "number", onInput BidChanged ] []
        , button [ onClick (TakeGameAction (GameAction.Bid tempBid)) ] [ text "Submit Bid" ]
        , button [ onClick (TakeGameAction GameAction.BidPass) ] [ text "Pass" ]
        ]


playerHandView : String -> List Card -> (Card -> Bool) -> Html Msg
playerHandView p cards isSelected =
    div []
        [ text p
        , div [] (List.map (cardView isSelected) cards)
        ]


cardBackView =
    div [ class "card" ] [ text Card.unicardBack ]


cardView : (Card -> Bool) -> Card -> Html Msg
cardView isSelected card =
    let
        suitClass =
            case card of
                Joker ->
                    "joker"

                FaceCard suit _ ->
                    String.toLower <| Suit.toString suit

        cardId =
            Card.toString card
                |> String.toLower
                |> String.replace " " "-"
    in
    div
        [ id cardId
        , class "card"
        , class suitClass
        , class
            (if isSelected card then
                "selected"

             else
                "not-selected"
            )
        , onClick (CardSelected card)
        ]
        [ text (unicard card) ]


roomView : WSMessage.RoomId -> Model -> Html Msg
roomView roomId model =
    div []
        [ div [] [ roomDescription roomId model ]
        , case ( model.connId, model.gameState ) of
            ( Just connId, Just state ) ->
                case Dict.get connId state.playersByConnId of
                    Just player ->
                        gameView model player state

                    Nothing ->
                        div [] [ text "Oops, you are not a player in the game! TODO: Build an observer view?" ]

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
