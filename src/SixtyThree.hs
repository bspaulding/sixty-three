{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SixtyThree where

import           Data.List (intersperse)
import qualified Data.Map  as Map
import qualified Data.Text as T
import           Import
import           Prelude   (enumFrom, succ, toEnum)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Enum, Eq, Show)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq, Show)

data Card = FaceCard Suit Face | Joker deriving (Eq, Show)

instance Display Card where
  textDisplay card =
    case card of
      Joker -> "Joker"
      FaceCard suit face ->
        let suit' = case suit of
              Hearts   -> "♥️"
              Diamonds -> "♦️"
              Clubs    -> "♣️"
              Spades   -> "♠️"
         in T.pack $ suit' ++ "  " ++ show face

newtype Cards = Cards [Card] deriving (Eq, Show)

instance Display Cards where
  display (Cards xs) = mconcat $ [start] ++ intersperse separator (map display xs) ++ [stop]
    where
      separator = displayBytesUtf8 ", "
      start = displayBytesUtf8 "["
      stop = displayBytesUtf8 "]"

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)

suits :: [Suit]
suits = generateEnumValues

faces :: [Face]
faces = generateEnumValues

deck :: Cards
deck =
  Cards $ Joker : [FaceCard suit face | suit <- suits, face <- faces]

deal :: Cards -> (Cards, Cards, Cards, Cards, Cards)
deal (Cards cards) =
  ( Cards $ take 12 cards,
    Cards $ take 12 $ drop 12 cards,
    Cards $ take 12 $ drop 24 cards,
    Cards $ take 12 $ drop 36 cards,
    Cards $ drop 48 cards
  )

data GameState = GameState
  { dealer          :: Player,
    currentBid      :: Maybe (Player, Integer),
    bidPassed       :: Map Player Bool,
    hands           :: Map Player [Card],
    kitty           :: [Card],
    tricks          :: [Trick],
    playerInControl :: Player,
    cardsInPlay     :: Map Player Card
  }
  deriving (Eq, Show)

data Player = PlayerOne | PlayerTwo | PlayerThree | PlayerFour deriving (Bounded, Enum, Eq, Ord, Show)

data Trick = Trick Player Cards deriving (Eq, Show)

initialGameState :: GameState
initialGameState =
  GameState
    { dealer = PlayerFour
    , currentBid = Nothing
    , bidPassed = Map.empty
    , hands = Map.empty
    , kitty = []
    , tricks = []
    , playerInControl = PlayerOne
    , cardsInPlay = Map.empty
    }

data GameAction
  = Deal
  | BidPass
  | Bid Integer
  | Play Card
  deriving (Eq, Show)

enumNext :: (Eq a, Bounded a, Enum a) => a -> a
enumNext a = if maxBound == a then minBound else succ a

reducer :: GameState -> (Player, GameAction) -> GameState
reducer state (player, action)
  | dealer state == player && action == Deal =
    let (Cards hand1, Cards hand2, Cards hand3, Cards hand4, Cards kitty) = deal deck
     in state
          { hands = Map.fromList [(PlayerOne, hand1), (PlayerTwo, hand2), (PlayerThree, hand3), (PlayerFour, hand4)]
          , kitty = kitty
          }
  | playerInControl state == player = case action of
    Bid amount ->
      if amount /= 126 && (amount > 63 || amount < 25)
        then state
        else case currentBid state of
          Just currentBid_ ->
            if amount > snd currentBid_
              then state {currentBid = Just (player, amount), playerInControl = enumNext player}
              else state
          Nothing -> state {currentBid = Just (player, amount), playerInControl = enumNext player}
    BidPass ->
      let newBidPassed = Map.insert player True (bidPassed state)
       in if currentBid state == Nothing && 3 == length (filter id $ Map.elems newBidPassed)
            then state {currentBid = Just (dealer state, 25), bidPassed = newBidPassed, playerInControl = enumNext player}
            else state {bidPassed = newBidPassed, playerInControl = enumNext player}
    _ -> state
  | otherwise = state

-- selectors
getDealer :: GameState -> Player
getDealer = dealer

getBid :: GameState -> Maybe (Player, Integer)
getBid = currentBid

getCurrentPlayer :: GameState -> Player
getCurrentPlayer = playerInControl

getBiddingComplete :: GameState -> Bool
getBiddingComplete state =
  Just 126 == fmap snd (getBid state)
    || 3 == length (filter id $ Map.elems (bidPassed state))

getKitty :: GameState -> [Card]
getKitty = kitty

getHand :: Player -> GameState -> [Card]
getHand player state =
  Map.findWithDefault [] player (hands state)
