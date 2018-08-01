module SixtyThree.Model exposing (..)

import Random exposing (andThen)
import Random.List exposing (shuffle)
import Set exposing (Set)


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


type Card
    = SuitedCard Suit CardValue
    | Joker


type PlayerId
    = PlayerOne
    | PlayerTwo
    | PlayerThree
    | PlayerFour


type Msg
    = ShuffleDeck
    | DeckShuffled (List Card)
    | Deal
    | Bid Int
    | SubmitBid
    | PassBid
    | SelectTrump Suit


type GamePhase
    = Dealing
    | Bidding
    | WaitingForTrump
    | Redealing
    | Tricking


type alias Model =
    { deck : List Card
    , kiddy : List Card
    , playerOneHand : List Card
    , playerTwoHand : List Card
    , playerThreeHand : List Card
    , playerFourHand : List Card
    , phase : GamePhase
    , currentlyBiddingPlayer : PlayerId
    , currentBid : Int
    , tempBid : Int
    , trump : Suit
    , playersPassed : Set Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model startDeck [] [] [] [] [] Dealing PlayerOne 0 0 Spades Set.empty, Cmd.none )


cardsOfSuit : Suit -> List Card
cardsOfSuit suit =
    List.map (\v -> SuitedCard suit v) cardValues


startDeck : List Card
startDeck =
    Joker
        :: (List.map cardsOfSuit suits
                |> List.concat
           )


deckGenerator : Random.Generator (List Card)
deckGenerator =
    shuffle startDeck
        |> andThen shuffle
        |> andThen shuffle
        |> andThen shuffle
