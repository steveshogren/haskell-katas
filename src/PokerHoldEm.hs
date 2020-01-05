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

fourCardsFlush cards =
  let suits = sortBy (compare) $ map PH.suit cards
  in any (\cards -> length cards == 4) $ groupWith id suits

outCount :: [PH.Card] -> [PH.Card] -> Integer
outCount flop hand@[c1, c2] =
  let testHand =  hand ++ flop
      overCardCounts = overCardCount flop hand
      pocketSameValue = PH.face c1 == PH.face c2
      pocketSameSuit = PH.suit c1 == PH.suit c2
  in
    if isJust (isTwoPair testHand) then 4
    else if pocketSameSuit && isJust (isFlush testHand) then 7
    else if pocketSameValue && isJust (isThreeOfAKind testHand) then 7
    else if pocketSameValue && isJust (isTwoOfAKind testHand) then 2
    else if 1 == overCardCounts  then 3
    else if 2 == overCardCounts  then 6
    else 0

percentage :: [PH.Card] -> [PH.Card] -> Integer
percentage flop hand = 100
