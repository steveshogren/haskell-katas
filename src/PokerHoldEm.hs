module PokerHoldEm where

import PokerHands as PH
import Data.Maybe(isJust)
import Data.List(sortBy)

overCardCount :: [PH.Card] -> [PH.Card] -> Integer
overCardCount flop hand =
  let flops = map PH.face flop
      hands = map PH.face hand
      sorted = sortBy (compare) $ hands ++ flops
  in foldl (\count card -> if elem card hands then count + 1 else count) 0
     $ take 2 sorted

outCount :: [PH.Card] -> [PH.Card] -> Integer
outCount board hand =
  let testHand =  hand ++ board
  in
    if isJust (isTwoPair testHand) then 4
    else if isJust (isTwoOfAKind testHand) then 2
    else 0

percentage :: [PH.Card] -> [PH.Card] -> Integer
percentage board hand = 100
