{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThree where

import Data.List (intersperse)
import qualified Data.Text as T
import Import
import Prelude (enumFrom, toEnum)

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

newtype Cards = Cards [Card]

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
