{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SixtyThree where

import Card
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GameAction
import GameState
import Import
import Player
import Shuffle
import System.Random
import Util
import Prelude (enumFrom, foldl, head, succ, toEnum)

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
isTrump t (FaceCard suit Five) = suit == t || suit == oppositeTrump t
isTrump t (FaceCard suit _) = suit == t
isTrump _ Joker = True

compareCards :: Suit -> Card -> Card -> Ordering
compareCards trumpSuit a b =
  case (a, b) of
    (FaceCard asuit aface, FaceCard bsuit bface) ->
      case (isTrump trumpSuit a, isTrump trumpSuit b) of
        (True, True) -> case (asuit == trumpSuit, bsuit == trumpSuit) of
          (True, False) -> GT
          (False, True) -> LT
          _ -> compare aface bface
        (True, False) -> GT
        (False, True) -> LT
        (False, False) -> compare aface bface
    (Joker, _) -> if isTrump trumpSuit b then LT else GT
    (_, Joker) -> if isTrump trumpSuit a then GT else LT

scoreTrick :: Suit -> Map Player Card -> (Player, Integer)
scoreTrick trumpSuit trick = (winner, totalScore)
  where
    scores = Map.toList $ Map.map (cardScore trumpSuit) trick
    totalScore = foldl (+) 0 (map snd scores)
    sortedCards = List.sortBy (\(_, a) (_, b) -> compareCards trumpSuit b a) (Map.toList trick)
    winner = fst . head $ sortedCards

scoreTricks :: Suit -> [Map Player Card] -> Map Player Integer
scoreTricks trumpSuit ts = foldl foldScores Map.empty scores
  where
    scores :: [(Player, Integer)]
    scores = List.map (scoreTrick trumpSuit) ts
    foldScores :: Map Player Integer -> (Player, Integer) -> Map Player Integer
    foldScores acc (winner, score) =
      Map.insert winner (score + Map.findWithDefault 0 winner acc) acc

players :: [Player]
players = generateEnumValues

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
    case (currentBid state, trump state) of
      (Just bid, Just t) ->
        let 
          resetState = initialGameState
            { previousRounds = (bid, scoreTricks t (tricks state)) : previousRounds state,
              -- TODO: maybe a resetRound function here
              dealer = enumNext (dealer state),
              playerInControl = enumNext (enumNext (dealer state)),
              playersByConnId = playersByConnId state,
              g = g state
            }
        in reducer resetState (getDealer resetState, Deal)
      -- TODO this should really be an error or something
      _ -> state
    else state
  where
    allHands = concatMap (`getHand` state) players

initializer :: StdGen -> [String] -> Either String GameState
initializer gen connIds
  | length connIds < 4 = Left "Need at least four people to start a game!"
  | otherwise = Right $ reducer (initialGameState { g = gen, playersByConnId = Map.fromList (zip connIds players)}) (dealer initialGameState, Deal)

reducer :: GameState -> (Player, GameAction) -> GameState
reducer state action = case reducerSafe state action of
  Left err -> state -- TODO: later maybe throw: error err
  Right newState -> newState

reducerSafeConns :: GameState -> String -> GameAction -> Either String GameState
reducerSafeConns state connId action = 
  case getPlayer state connId of
    Just player -> reducerSafe state (player, action)
    Nothing -> Left "You are not a player in the game!"

reducerSafe :: GameState -> (Player, GameAction) -> Either String GameState
reducerSafe state (player, action)
  | getGameOver state = Right state
  | dealer state == player && action == Deal =
    let ((hand1, hand2, hand3, hand4, kitty'), g') = deal deck (g state)
     in Right
          state
            { hands = Map.fromList [(PlayerOne, hand1), (PlayerTwo, hand2), (PlayerThree, hand3), (PlayerFour, hand4)],
              kitty = kitty',
              g = g'
            }
  | isDiscard action = case action of
    Discard cards ->
      case trump state of
        Nothing -> Left "cannot discard if trump not selected!"
        Just t ->
          if foldl (+) 0 (map (cardScore t) cards) > 0
            then Left "cannot discard trump worth points!"
            else
              let hand = Set.fromList $ getHand player state
                  newHand = Set.toList $ Set.difference hand (Set.fromList cards)
               in if length newHand >= 6
                    then
                      Right
                        state
                          { hands = Map.insert player newHand (hands state),
                            playerInControl = enumNext player,
                            discarded = discarded state ++ cards
                          }
                    else Left "You must keep at least six cards in your hand."
  | playerInControl state == player = case action of
    Bid amount ->
      if amount /= 126 && (amount > 63 || amount < 25)
        then Left "Bid must be between 25 and 63, or double 63 (126)."
        else case currentBid state of
          Just currentBid_ ->
            if amount > snd currentBid_
              then Right state {currentBid = Just (player, amount), playerInControl = enumNext player}
              else Left $ "You cannot bid less than the current bid of " ++ show (snd currentBid_)
          Nothing -> Right state {currentBid = Just (player, amount), playerInControl = enumNext player}
    BidPass ->
      let newBidPassed = Map.insert player True (bidPassed state)
       in if currentBid state == Nothing && 3 == length (filter id $ Map.elems newBidPassed)
            then Right state {currentBid = Just (dealer state, 25), bidPassed = newBidPassed, playerInControl = enumNext player}
            else Right state {bidPassed = newBidPassed, playerInControl = enumNext player}
    Play card ->
      if getAllPlayersDiscarded state && trump state /= Nothing && any (card ==) (Map.findWithDefault [] player (hands state))
        then
          let newHand = filter (card /=) $ Map.findWithDefault [] player (hands state)
              newCardsInPlay = Map.insert player card (cardsInPlay state)
              roundIsOver = Map.size newCardsInPlay == 4
           in Right $
                maybeFinishRound $
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
        else Left "You cannot play a card right now!"
    PickTrump suit ->
      let newHand = kitty state ++ Map.findWithDefault [] player (hands state)
       in Right
            state
              { trump = Just suit,
                kitty = [],
                hands = Map.insert player newHand (hands state)
              }
    PassCards cards ->
      case trump state of
        Just t ->
          let partner' = partner player
              newHand = Map.findWithDefault [] partner' (hands state) ++ cards
              newPlayerHand = Set.toList $ Set.difference (Set.fromList (Map.findWithDefault [] player (hands state))) (Set.fromList cards)
              newHands = Map.insert player newPlayerHand $ Map.insert partner' newHand (hands state)
              playerHasAce = Set.member (FaceCard t Ace) (Set.fromList (getHand player state))
              passingTheJoker = Set.member Joker (Set.fromList cards)
           in if not playerHasAce && passingTheJoker then Left "You cannot pass the joker if you do not have the ace." else Right state {hands = newHands}
        Nothing -> Left "You cannot pass cards until trump has been selected."
    _ -> Right state
  | otherwise = Right state
