module PokerHoldEm where

import PokerHands as PH
import Data.Maybe(isJust, fromMaybe, catMaybes)
import Data.List(sortBy, any)
import GHC.Exts(groupWith)
import Debug.Trace(trace)
import Combinatorics(variate)
import Data.Set(toList, fromList)

overCardCount :: [PH.Card] -> [PH.Card] -> Integer
overCardCount flop hand =
  let flops = map PH.face flop
      hands = map PH.face hand
      sorted = sortBy compare $ hands ++ flops
  in foldl (\count card -> if elem card hands then count + 1 else count) 0
     $ take 2 sorted

fourCardsFlush :: [Card] -> Bool
fourCardsFlush cards =
  let suits = sortBy compare $ map PH.suit cards
  in any (\cards -> length cards == 4) $ groupWith id suits

isOpenStraight :: [Card] -> Bool
isOpenStraight cards =
  let sorted = sortBy (\x y -> compare (PH.face x) (PH.face y)) cards
      first4 = isJust $ isStraight $ take 4 sorted
      second4 = isJust $ isStraight $ take 4 $ drop 1 sorted
  in first4 || second4

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

outCount :: [PH.Card] -> [PH.Card] -> Integer
outCount flop hand@[c1, c2] =
  let testHand =  hand ++ flop
      overCardCounts = overCardCount flop hand
  in
    if isJust (isTwoPair testHand) then 4
    else if fourCardsFlush testHand && isInsideStraight testHand then 12
    else if isInsideStraight testHand then 4
    else if fourCardsFlush testHand && isOpenStraight testHand then 15
    else if fourCardsFlush testHand then 9
    else if isOpenStraight testHand then 8
    else if isJust (isThreeOfAKind testHand) then 7
    else if isJust (isTwoOfAKind testHand) then 2
    else if 1 == overCardCounts  then 3
    else if 2 == overCardCounts  then 6
    else 0

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
