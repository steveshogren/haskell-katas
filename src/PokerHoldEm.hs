module PokerHoldEm where

import PokerHands as PH
import Data.Maybe(isJust)
import Data.List(sortBy, any)
import GHC.Exts(groupWith)

overCardCount :: [PH.Card] -> [PH.Card] -> Integer
overCardCount flop hand =
  let flops = map PH.face flop
      hands = map PH.face hand
      sorted = sortBy (compare) $ hands ++ flops
  in foldl (\count card -> if elem card hands then count + 1 else count) 0
     $ take 2 sorted

fourCardsFlush :: [Card] -> Bool
fourCardsFlush cards =
  let suits = sortBy (compare) $ map PH.suit cards
  in any (\cards -> length cards == 4) $ groupWith id suits

isOpenStraight :: [Card] -> Bool
isOpenStraight cards =
  let sorted = sortBy (\x y -> compare (PH.face x) (PH.face y)) cards
      first4 = isJust $ isStraight $ take 4 sorted
      second4 = isJust $ isStraight $ take 4 $ drop 1 sorted
  in first4 || second4

outCount :: [PH.Card] -> [PH.Card] -> Integer
outCount flop hand@[c1, c2] =
  let testHand =  hand ++ flop
      overCardCounts = overCardCount flop hand
  in
    if isJust (isTwoPair testHand) then 4
    else if fourCardsFlush testHand then 9
    else if fourCardsFlush testHand then 9
    else if isOpenStraight testHand then 8
    else if isJust (isThreeOfAKind testHand) then 7
    else if isJust (isTwoOfAKind testHand) then 2
    else if 1 == overCardCounts  then 3
    else if 2 == overCardCounts  then 6
    else 0

percentage :: [PH.Card] -> [PH.Card] -> Integer
percentage flop hand = 100
