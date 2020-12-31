{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThree where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Import
import Shuffle
import System.Random
import Util
import Prelude (enumFrom, foldl, head, succ, toEnum)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Enum, Eq, Ord, Show)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq, Show)

rank :: Face -> Integer
rank f = case f of
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 11
  Queen -> 12
  King -> 13
  Ace -> 14

instance Ord Face where
  compare a b = compare (rank a) (rank b)

data Card = FaceCard Suit Face | Joker deriving (Eq, Ord, Show)

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
  display (Cards xs) = mconcat $ [start] ++ List.intersperse separator (map display xs) ++ [stop]
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

isTrump :: Suit -> Card -> Bool
isTrump trump (FaceCard suit Five) = suit == trump || suit == oppositeTrump trump
isTrump trump (FaceCard suit face) = suit == trump
isTrump trump Joker = True

compareCards :: Suit -> Card -> Card -> Ordering
compareCards trump a b =
  case (a, b) of
    (FaceCard asuit aface, FaceCard bsuit bface) ->
      case (isTrump trump a, isTrump trump b) of
        (True, True) -> case (asuit == trump, bsuit == trump) of
          (True, False) -> GT
          (False, True) -> LT
          _ -> compare aface bface
        (True, False) -> GT
        (False, True) -> LT
        (False, False) -> compare aface bface
    (Joker, _) -> if isTrump trump b then LT else GT
    (_, Joker) -> if isTrump trump a then GT else LT
    _ -> LT

scoreTrick :: Suit -> Map Player Card -> (Player, Integer)
scoreTrick trump trick = (winner, totalScore)
  where
    scores = Map.toList $ Map.map (cardScore trump) trick
    totalScore = foldl (+) 0 (map snd scores)
    sortedCards = List.sortBy (\(_, a) (_, b) -> compareCards trump b a) (Map.toList trick)
    winner = fst . head $ sortedCards

scoreTricks :: Suit -> [Map Player Card] -> Map Player Integer
scoreTricks trump tricks = foldl foldScores Map.empty scores
  where
    scores :: [(Player, Integer)]
    scores = List.map (scoreTrick trump) tricks
    foldScores :: Map Player Integer -> (Player, Integer) -> Map Player Integer
    foldScores acc (winner, score) =
      Map.insert winner (score + Map.findWithDefault 0 winner acc) acc

type Round = ((Player, Integer), [Map Player Card])

-- lists of cards should probably be sets of cards
data GameState = GameState
  { dealer :: Player,
    currentBid :: Maybe (Player, Integer),
    bidPassed :: Map Player Bool,
    hands :: Map Player [Card],
    kitty :: [Card],
    tricks :: [Map Player Card],
    playerInControl :: Player,
    cardsInPlay :: Map Player Card,
    discarded :: [Card],
    trump :: Maybe Suit,
    previousRounds :: [Round],
    g :: StdGen
  }
  deriving (Eq, Show)

data Player = PlayerOne | PlayerTwo | PlayerThree | PlayerFour deriving (Bounded, Enum, Eq, Ord, Show)

players :: [Player]
players = generateEnumValues

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
      discarded = [],
      trump = Nothing,
      previousRounds = [],
      g = mkStdGen 0
    }

data GameAction
  = Deal
  | BidPass
  | Bid Integer
  | Play Card
  | PickTrump Suit
  | Discard [Card]
  | PassCards [Card]
  deriving (Eq, Show)

enumNext :: (Eq a, Bounded a, Enum a) => a -> a
enumNext a = if maxBound == a then minBound else succ a

partner :: Player -> Player
partner p =
  case p of
    PlayerOne -> PlayerThree
    PlayerTwo -> PlayerFour
    PlayerThree -> PlayerOne
    PlayerFour -> PlayerTwo

maybeFinishRound :: GameState -> GameState
maybeFinishRound state =
  if null allHands
    then -- TODO: move bid and tricks to previousRounds, reset game for next round
    case currentBid state of
      Just bid ->
        initialGameState
          { previousRounds = (bid, tricks state) : previousRounds state,
            -- TODO: maybe a resetRound function here
            dealer = enumNext (dealer state),
            playerInControl = enumNext (enumNext (dealer state)),
            g = g state
          }
      -- TODO this should really be an error or something
      Nothing -> state
    else state
  where
    allHands = concatMap (`getHand` state) players

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
      if getAllPlayersDiscarded state && trump state /= Nothing && any (card ==) (Map.findWithDefault [] player (hands state))
        then
          let newHand = filter (card /=) $ Map.findWithDefault [] player (hands state)
              newCardsInPlay = Map.insert player card (cardsInPlay state)
              roundIsOver = Map.size newCardsInPlay == 4
           in maybeFinishRound $
                state
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
    PickTrump suit ->
      let newHand = (kitty state) ++ Map.findWithDefault [] player (hands state)
       in state
            { trump = Just suit,
              kitty = [],
              hands = Map.insert player newHand (hands state)
            }
    PassCards cards ->
      let partner' = partner player
          newHand = (Map.findWithDefault [] partner' (hands state)) ++ cards
          newPlayerHand = Set.toList $ Set.difference (Set.fromList (Map.findWithDefault [] player (hands state))) (Set.fromList cards)
          newHands = Map.insert player newPlayerHand $ Map.insert partner' newHand (hands state)
       in state {hands = newHands}
    Discard cards ->
      case (trump state) of
        Nothing -> state -- cannot discard if trump not selected!
        Just t ->
          if any (isTrump t) cards
            then state -- cannot discard trump!
            else
              let hand = Set.fromList $ getHand player state
                  newHand = Set.toList $ Set.difference hand (Set.fromList cards)
               in if length newHand == 6
                    then
                      state
                        { hands = Map.insert player newHand (hands state),
                          playerInControl = enumNext player,
                          discarded = discarded state ++ cards
                        }
                    else state
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

getAllPlayersDiscarded :: GameState -> Bool
getAllPlayersDiscarded state =
  all (<= 6) $ Map.elems $ Map.map length (hands state)

getLastRound :: GameState -> Maybe Round
getLastRound = safeHead . previousRounds
