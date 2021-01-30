module GameState exposing (..)

import Card
import Dict
import GamePlayer exposing (GamePlayer(..))
import HaskellMapDecoder exposing (haskellMap)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Round exposing (Round)
import Suit exposing (Suit)


type alias GameState =
    { playerInControl : GamePlayer
    , hands : Dict.Dict String (List Card.Card)
    , cardsInPlay : Dict.Dict String Card.Card
    , trump : Maybe Suit
    , currentBid : Maybe ( GamePlayer, Int )
    , playersByConnId : Dict.Dict String GamePlayer
    , bidPassed : Dict.Dict String Bool
    , tricks : List (Dict.Dict String Card.Card)
    , previousRounds : List Round
    }


decode : D.Decoder GameState
decode =
    D.succeed GameState
        |> DP.required "playerInControl" GamePlayer.decode
        |> DP.required "hands" (haskellMap D.string (D.list Card.cardDecoder))
        |> DP.required "cardsInPlay" (haskellMap D.string Card.cardDecoder)
        |> DP.required "trump" (D.maybe Suit.decode)
        |> DP.required "currentBid" (D.maybe (D.map2 Tuple.pair (D.index 0 GamePlayer.decode) (D.index 1 D.int)))
        |> DP.required "playersByConnId" (D.dict GamePlayer.decode)
        |> DP.required "bidPassed" (haskellMap D.string D.bool)
        |> DP.required "tricks" (D.list (haskellMap D.string Card.cardDecoder))
        |> DP.required "previousRounds" (D.list Round.decode)


biddingOver : GameState -> Bool
biddingOver game =
    Dict.filter (\_ v -> v) game.bidPassed
        |> Dict.size
        |> (\x -> x == 3)


allPlayersDiscarded : GameState -> Bool
allPlayersDiscarded game =
    game.hands
        |> Dict.values
        |> List.map List.length
        |> List.all (\x -> x <= 6)


trumpSelected : GameState -> Bool
trumpSelected game =
    game.trump /= Nothing


getGameOver : GameState -> Bool
getGameOver state =
    let
        ( teamOddScore, teamEvenScore ) =
            getTotalScore state
    in
    teamOddScore >= 200 || teamEvenScore >= 200


getTotalScore : GameState -> ( Int, Int )
getTotalScore state =
    calcTotalScore state.previousRounds


calcTotalScore : List Round -> ( Int, Int )
calcTotalScore rounds =
    let
        roundScores =
            List.map calcRoundScore rounds

        addTuples ( a1, b1 ) ( a2, b2 ) =
            ( a1 + a2, b1 + b2 )

        ( teamOdd, teamEven ) =
            List.foldl addTuples ( 0, 0 ) roundScores
    in
    ( teamOdd, teamEven )


calcRoundScore : Round -> ( Int, Int )
calcRoundScore ( ( bidder, bid ), scoresList ) =
    let
        scores =
            Dict.fromList scoresList

        playerOne =
            Maybe.withDefault 0 <| Dict.get "PlayerOne" scores

        playerTwo =
            Maybe.withDefault 0 <| Dict.get "PlayerTwo" scores

        playerThree =
            Maybe.withDefault 0 <| Dict.get "PlayerThree" scores

        playerFour =
            Maybe.withDefault 0 <| Dict.get "PlayerFour" scores

        teamOdd1 =
            playerOne + playerThree

        teamOdd =
            if (bidder == PlayerOne || bidder == PlayerThree) && teamOdd1 < bid then
                -1 * bid

            else
                teamOdd1

        teamEven1 =
            playerTwo + playerFour

        teamEven =
            if (bidder == PlayerTwo || bidder == PlayerFour) && teamEven1 < bid then
                -1 * bid

            else
                teamEven1
    in
    ( teamOdd, teamEven )
