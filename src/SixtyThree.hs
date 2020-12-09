{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThree where

import Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Text as T
import Import
import Shuffle
import System.Random
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

oppositeTrump :: Suit -> Suit
oppositeTrump s =
  case s of
    Hearts -> Diamonds
    Diamonds -> Hearts
    Spades -> Clubs
    Clubs -> Spades

cardScore :: Suit -> Card -> Integer
cardScore trumpSuit card =
  case card of
    FaceCard suit face ->
      case (suit == trumpSuit, suit == oppositeTrump trumpSuit, face) of
        (True, False, Ace) -> 1
        (True, False, King) -> 25
        (True, False, Jack) -> 1
        (True, False, Ten) -> 1
        (True, False, Nine) -> 9
        (True, False, Five) -> 5
        (False, True, Five) -> 5
        (True, False, Two) -> 1
        _ -> 0
    Joker -> 15

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)

suits :: [Suit]
suits = generateEnumValues

faces :: [Face]
faces = generateEnumValues

deck :: [Card]
deck =
  Joker : [FaceCard suit face | suit <- suits, face <- faces]

deal :: RandomGen g => [Card] -> g -> (([Card], [Card], [Card], [Card], [Card]), g)
deal unshuffled gen =
  if all hasAceOrFace [hand1, hand2, hand3, hand4]
    then (dealt, gen')
    else deal cards gen'
  where
    (cards, gen') = shuffle unshuffled gen
    hand1 = take 12 cards
    hand2 = take 12 $ drop 12 cards
    hand3 = take 12 $ drop 24 cards
    hand4 = take 12 $ drop 36 cards
    kitty_ = drop 48 cards
    dealt = (hand1, hand2, hand3, hand4, kitty_)

hasAceOrFace :: [Card] -> Bool
hasAceOrFace cards = not $ null acesAndFaces
  where
    acesAndFaces = filter isAceOrFace cards
    isAceOrFace :: Card -> Bool
    isAceOrFace (FaceCard _ Ace) = True
    isAceOrFace (FaceCard _ King) = True
    isAceOrFace (FaceCard _ Queen) = True
    isAceOrFace (FaceCard _ Jack) = True
    isAceOrFace _ = False

data GameState = GameState
  { dealer :: Player,
    currentBid :: Maybe (Player, Integer),
    bidPassed :: Map Player Bool,
    hands :: Map Player [Card],
    kitty :: [Card],
    tricks :: [Map Player Card],
    playerInControl :: Player,
    cardsInPlay :: Map Player Card,
    trump :: Maybe Suit,
    g :: StdGen
  }
  deriving (Eq, Show)

data Player = PlayerOne | PlayerTwo | PlayerThree | PlayerFour deriving (Bounded, Enum, Eq, Ord, Show)

initialGameState :: GameState
initialGameState =
  GameState
    { dealer = PlayerFour,
      currentBid = Nothing,
      bidPassed = Map.empty,
      hands = Map.empty,
      kitty = [],
      tricks = [],
      playerInControl = PlayerOne,
      cardsInPlay = Map.empty,
      trump = Nothing,
      g = mkStdGen 0
    }

data GameAction
  = Deal
  | BidPass
  | Bid Integer
  | Play Card
  | PickTrump Suit
  deriving (Eq, Show)

enumNext :: (Eq a, Bounded a, Enum a) => a -> a
enumNext a = if maxBound == a then minBound else succ a

reducer :: GameState -> (Player, GameAction) -> GameState
reducer state (player, action)
  | dealer state == player && action == Deal =
    let ((hand1, hand2, hand3, hand4, kitty'), g') = deal deck (g state)
     in state
          { hands = Map.fromList [(PlayerOne, hand1), (PlayerTwo, hand2), (PlayerThree, hand3), (PlayerFour, hand4)],
            kitty = kitty',
            g = g'
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
    Play card ->
      if trump state /= Nothing && any (card ==) (Map.findWithDefault [] player (hands state))
        then
          let newHand = filter (card /=) $ Map.findWithDefault [] player (hands state)
              newCardsInPlay = Map.insert player card (cardsInPlay state)
              roundIsOver = Map.size newCardsInPlay == 4
           in state
                { hands = Map.insert player newHand (hands state),
                  cardsInPlay =
                    if roundIsOver
                      then Map.empty
                      else newCardsInPlay,
                  tricks =
                    if roundIsOver
                      then newCardsInPlay : tricks state
                      else tricks state,
                  playerInControl = enumNext player
                }
        else state
    PickTrump suit -> state {trump = Just suit}
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

getCardInPlay :: Player -> GameState -> Maybe Card
getCardInPlay player state = Map.lookup player (cardsInPlay state)

getTricks :: GameState -> [Map Player Card]
getTricks = tricks

getTrump :: GameState -> Maybe Suit
getTrump = trump