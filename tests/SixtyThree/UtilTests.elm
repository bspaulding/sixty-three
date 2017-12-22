module SixtyThree.UtilTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import SixtyThree.Model exposing (..)
import SixtyThree.Util exposing (..)


suite : Test
suite =
    describe "UtilTests"
        [ describe "isOppositeFive"
            [ test "returns false for joker" <|
                \_ -> Expect.equal False (isOppositeFive Spades Joker)
            , test "returns true for club five with spade trump" <|
                \_ -> Expect.equal True (isOppositeFive Spades (SuitedCard Clubs Five))
            , test "returns false for spade five with heart trump" <|
                \_ -> Expect.equal False (isOppositeFive Hearts (SuitedCard Spades Five))
            ]
        , describe "pointsForCard"
            [ test "returns 5 for the club five with spade trump" <|
                \_ ->
                    Expect.equal 5 (pointsForCard Spades (SuitedCard Clubs Five))
            , test "returns 0 for the spade five with heart trump" <|
                \_ ->
                    Expect.equal 0 (pointsForCard Hearts (SuitedCard Spades Five))
            , test "returns 15 for joker" <|
                \_ ->
                    Expect.equal 15 (pointsForCard Hearts Joker)
            , test "returns 1 for trump ace" <|
                \_ -> Expect.equal 1 (pointsForCard Hearts (SuitedCard Hearts Ace))
            , test "returns 25 for trump king" <|
                \_ -> Expect.equal 25 (pointsForCard Hearts (SuitedCard Hearts King))
            , test "returns 1 for trump jack" <|
                \_ -> Expect.equal 1 (pointsForCard Hearts (SuitedCard Hearts Jack))
            , test "returns 1 for trump 10" <|
                \_ -> Expect.equal 1 (pointsForCard Hearts (SuitedCard Hearts Ten))
            , test "returns 9 for trump 9" <|
                \_ -> Expect.equal 9 (pointsForCard Hearts (SuitedCard Hearts Nine))
            , test "returns 5 for trump 5" <|
                \_ -> Expect.equal 5 (pointsForCard Hearts (SuitedCard Hearts Five))
            , test "returns 1 for trump 2" <|
                \_ -> Expect.equal 1 (pointsForCard Hearts (SuitedCard Hearts Two))
            ]
        ]
