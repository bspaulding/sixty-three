{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThree where

import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Text as T
import Import
import Prelude (enumFrom, succ, toEnum)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Enum, Eq, Show)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq, Show)

data Card = FaceCard Suit Face | Joker deriving (Eq, Show)

instance Display Card where
  textDisplay card =
    case card of
      Joker -> "Joker"
      FaceCard suit face ->
        let suit' = case suit of
              Hearts -> "♥️"
              Diamonds -> "♦️"
              Clubs -> "♣️"
              Spades -> "♠️"
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
  { currentBid :: (Player, Int),
    bidPassed :: Map Player Bool,
    hands :: [Cards],
    kitty :: Cards,
    tricks :: [Trick],
    playerInControl :: Player
  }
  deriving (Eq, Show)

data Player = PlayerOne | PlayerTwo | PlayerThree | PlayerFour deriving (Bounded, Enum, Eq, Ord, Show)

data Trick = Trick Player Cards deriving (Eq, Show)

initialGameState :: GameState
initialGameState =
  GameState
    { currentBid = (PlayerFour, 25),
      bidPassed = Map.empty,
      hands = [],
      kitty = Cards [],
      tricks = [],
      playerInControl = PlayerOne
    }

data GameAction
  = BidPass
  | Bid Int
  | Play Card

enumNext :: (Eq a, Bounded a, Enum a) => a -> a
enumNext a = if maxBound == a then minBound else succ a

reducer :: GameState -> (Player, GameAction) -> GameState
reducer state (player, action)
  | playerInControl state == player = case action of
    Bid amount -> state {currentBid = (player, amount), playerInControl = enumNext player}
    BidPass -> state {bidPassed = Map.insert player True (bidPassed state), playerInControl = enumNext player}
    _ -> state
  | otherwise = state

-- selectors
getBid :: GameState -> (Player, Int)
getBid = currentBid

getCurrentPlayer :: GameState -> Player
getCurrentPlayer = playerInControl