module PokerHoldEm where

import PokerHands as PH
import Data.Maybe(isJust, fromMaybe, catMaybes)
import Data.List(sortBy, any, find)
import GHC.Exts(groupWith)
import Debug.Trace(trace)
import Combinatorics(variate)
import Data.Set(toList, fromList)


highestCard :: [PH.Card] -> PH.Card
highestCard cards = head $ sortBy compare cards

overCardCount :: [PH.Card] -> [PH.Card] -> Integer
overCardCount flop hand =
  let flops = map PH.face flop
      hands = map PH.face hand
      sorted = sortBy compare $ hands ++ flops
  in foldl (\count card -> if elem card hands then count + 1 else count) 0
     $ take 2 sorted

fourCardsFlush :: [Card] -> Maybe Suit
fourCardsFlush cards =
  let suits = sortBy compare $ map PH.suit cards
      groups = groupWith id suits
      found = find (\cards -> length cards == 4) groups
  in head <$> found

isOpenStraight :: [Card] -> [Face]
isOpenStraight cards =
  let sorted = sortBy (\x y -> compare (PH.face x) (PH.face y)) cards
      first4 = isStraight $ take 4 sorted
      second4 = isStraight $ take 4 $ drop 1 sorted
  in concatMap (\(Straight face) -> [face, pred face]) $ catMaybes [first4, second4]

isInsideStraight :: [Card] -> Bool
isInsideStraight cards =
  let sorted = sortBy compare $ map (fromEnum . PH.face) cards
      first4 = take 4 sorted
      second4 = take 4 $ drop 1 sorted
  in any (\cs@[c1,c2,c3,c4] ->
            if (c1 + 2 == c2 && enumFromTo c2 c4 == [c2,c3,c4]) then
              True
            else if ((enumFromTo c1 c2) == [c1,c2] && c2+2 == c3 && (enumFromTo c3 c4) == [c3,c4]) then
              True
            else if (enumFromTo c1 c3 == [c1,c2,c3] && c3+2 == c4) then
              True
            else False
         )
     [first4, second4]

outCount :: [PH.Card] -> [PH.Card] -> [(Integer, AHand)]
outCount flop hand@[c1, c2] =
  let testHand =  hand ++ flop
      sortedHandCards =  sortBy compare hand
      overCardCounts = overCardCount flop hand
      pair = (isTwoOfAKind testHand)
      flush = (fourCardsFlush testHand)
      openStraight = isOpenStraight testHand
  in if isJust (isTwoPair testHand) then [(4, HighCard Two)]
  else if isJust flush  && isInsideStraight testHand then
    let (Just suit) = flush
    in [(4, Straight Two), (8, Flush suit)]
  else if isInsideStraight testHand then
    [(4, Straight Two)]
  else if isJust flush && length openStraight > 0 then
    let (Just suit) = flush
        [face1, face2] = trace ("open straight" ++ show openStraight) openStraight
    in [(9, Flush suit), (3, Straight face1), (3, Straight face2)]
  else if isJust flush then
    let (Just suit) = flush
    in [(9, Flush suit)]
  else if length openStraight > 0 then
    [(4, Straight Two), (4, Straight Two)]
  else if isJust (isThreeOfAKind testHand) then
    [(7, HighCard Two)]
  else if isJust pair then
    let TwoOfAKind face = fromMaybe (HighCard Two) pair
    in [(2, ThreeOfAKind face)]
  else if 1 == overCardCounts  then
    [(3, TwoOfAKind $ face $ head sortedHandCards)]
  else if 2 == overCardCounts  then
    [(3, TwoOfAKind $ face $ head sortedHandCards),
     (3, TwoOfAKind $ face $ head $ drop 1 sortedHandCards)]
  else [(0,HighCard Two)]

percentage :: [PH.Card] -> [PH.Card] -> Integer
percentage flop hand = 100

possibleHands cards =
  toList $ fromList $ map (\x -> sortBy compare x) $ variate 5 cards

bestHand :: [PH.Card] -> PH.AHand
bestHand cards =
  let handsPermutations = possibleHands cards
      hands = map (\hand -> PH.detectHand hand Nothing) handsPermutations
  in
    head $ sortBy compare $ catMaybes hands

winner :: [PH.Card] -> [PH.Card] -> [PH.Card] -> Either PH.AHand PH.AHand
winner p1 p2 table =
  let left = bestHand (p1 ++ table)
      right = bestHand (p2 ++ table)
  in if left < right then Left left else Right right

rounder :: (Fractional a2, RealFrac a1, Integral b) => a1 -> b -> a2
rounder f n =
  (fromInteger $ round $ f * (10^n)) / (10.0^^n)

oddsFlopToTurn :: Integer -> Double
oddsFlopToTurn outs =
  rounder (100*  ((fromIntegral outs)/46.0) ) 1
