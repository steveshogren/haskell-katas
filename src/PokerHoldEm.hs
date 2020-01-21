module PokerHoldEm where

import PokerHands as PH
import Data.Maybe(isJust, fromMaybe, catMaybes)
import Data.Either(isLeft)
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

isInsideStraight :: [Card] -> [Face]
isInsideStraight cards =
  let sorted = sortBy compare $ map (\c ->  ((fromEnum . PH.face) c, PH.face c)) cards
      first4 = take 4 sorted
      second4 = take 4 $ drop 1 sorted
  in catMaybes $ map (\cs@[(c1,highCard),(c2,_),(c3,_),(c4,_)] ->
            if (c1 + 2 == c2 && enumFromTo c2 c4 == [c2,c3,c4]) then
              Just highCard
            else if ((enumFromTo c1 c2) == [c1,c2] && c2+2 == c3 && (enumFromTo c3 c4) == [c3,c4]) then
              Just highCard
            else if (enumFromTo c1 c3 == [c1,c2,c3] && c3+2 == c4) then
              Just highCard
            else Nothing
         )
     [first4, second4]

outCount :: [PH.Card] -> [PH.Card] -> [(Integer, AHand)]
outCount flop hand@[c1, c2] =
  let testHand =  hand ++ flop
      sortedHandCards =  sortBy compare hand
      overCardCounts = overCardCount flop hand
      pair = isTwoOfAKind testHand
      flush = fourCardsFlush testHand
      openStraight = isOpenStraight testHand
      insideStraight = isInsideStraight testHand
      twoPair = isTwoPair testHand
      threeKind = isThreeOfAKind testHand
  in if isJust twoPair then
    let Just (TwoPair (a, b)) = twoPair
    in [(2, FullHouse (a,b)), (2, FullHouse (b,a))]
  else if isJust flush  && length insideStraight > 0 then
    let (Just suit) = flush
    in [(4, Straight (head insideStraight)), (8, Flush suit)]
  else if length insideStraight > 0 then
    [(4, Straight (head insideStraight))]
  else if isJust flush && length openStraight > 0 then
    let (Just suit) = flush
        [face1, face2] = openStraight
    in [(9, Flush suit), (3, Straight face1), (3, Straight face2)]
  else if isJust flush then
    let (Just suit) = flush
    in [(9, Flush suit)]
  else if length openStraight > 0 then
    let [face1, face2] = openStraight
    in [(4, Straight face1), (4, Straight face2)]
  else if isJust threeKind then
    let Just (ThreeOfAKind face) = threeKind
        [a,b] = sortBy compare $ filter (\c -> c /= face) $ map PH.face testHand
    in [(1, FourOfAKind face),
        (3, FullHouse (face, a)),
        (3, FullHouse (face, b))]
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

runner =
  let flop = fromMaybe [] (parseHand "QD 2H 9S")
      --p1 = fromMaybe [] (parseHand "4D 4H")
      --p2 = fromMaybe [] (parseHand "2D 2H")
      p1 = fromMaybe [] (parseHand "QD 4H")
      p2 = fromMaybe [] (parseHand "QD QH")
  in winPercentage flop p1 p2

leftWins :: PH.AHand -> PH.AHand -> Bool
leftWins h1 h2 = h1 < h2

winPercentage flop p1 p2 =
  let h1 = outCount flop p1 ++ [(0, bestHand (flop ++ p1))]
      h2 = outCount flop p2 ++ [(0, bestHand (flop ++ p2))]
      leftWinning = isLeft $ winner p1 p2 flop
      (leftOuts, rightOuts) =
        foldl (\(l,r) (outs1,ahand1) ->
                 let (ln, rn) =
                       foldl (\(l, r) (outs2, ahand2) ->
                                 if leftWins ahand1 ahand2
                                 then (l + outs1, r)
                                 else (l, r + outs2))
                       (0,0) h2
                 in (l+ln, r+rn))
        (0,0) h1
      leftPercent = oddsFlopToTurn leftOuts
      rightPercent = oddsFlopToTurn rightOuts
   in if leftWinning then
        (100-rightPercent,rightPercent)
      else (leftPercent,100-leftPercent)
