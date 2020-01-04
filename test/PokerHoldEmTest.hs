module PokerHoldEmTest where

import Test.Tasty
import Test.Tasty.HUnit

import PokerHands
import PokerHoldEm as PHE
import Data.Maybe(fromMaybe)

pHand2 = [Card Two Spades,Card Two Diamonds]
board1 = []

testOverCardCount :: Assertion
testOverCardCount =
   let flop = fromMaybe [] (parseHand "5D 8H 9S")
       hand = fromMaybe [] (parseHand "QH 2H")
   in (assertEqual "one overcard" 1 (PHE.overCardCount flop hand))

testTwoOverCardCount :: Assertion
testTwoOverCardCount =
   let flop = fromMaybe [] (parseHand "5D 8H 2S")
       hand = fromMaybe [] (parseHand "QH 9H")
   in (assertEqual "Two overcards" 2 (PHE.overCardCount flop hand))

testOutCounterPocketPair :: Assertion
testOutCounterPocketPair =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       pocketHand = fromMaybe [] (parseHand "4D 4H")
       nonPocketHand = fromMaybe [] (parseHand "3D 2H")
   in (assertEqual "pocket pair" 2 (PHE.outCount flop pocketHand))
      >> (assertEqual "no pocket pair" 0 (PHE.outCount flop nonPocketHand))

testOutCounterOneOvercard :: Assertion
testOutCounterOneOvercard =
   let flop = fromMaybe [] (parseHand "5D 8H 9S")
       hand = fromMaybe [] (parseHand "QH 2H")
   in (assertEqual "one overcard" 3 (PHE.outCount flop hand))

testOutCounterTwoOvercard :: Assertion
testOutCounterTwoOvercard =
   let flop = fromMaybe [] (parseHand "5D 8H 2S")
       hand = fromMaybe [] (parseHand "QH 9H")
   in (assertEqual "two overcard" 6 (PHE.outCount flop hand))

testOutCounterTwoPairToFullHouse :: Assertion
testOutCounterTwoPairToFullHouse =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       hand = fromMaybe [] (parseHand "QH 9H")
   in (assertEqual "two pair to full" 4 (PHE.outCount flop hand))


testSetToFullHouseFourKind :: Assertion
testSetToFullHouseFourKind =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       hand = fromMaybe [] (parseHand "QH QS")
   in (assertEqual "set to full house / four kind" 7 (PHE.outCount flop hand))

testFlushDraw =
   let flop = fromMaybe [] (parseHand "QD 2D 9S")
       hand = fromMaybe [] (parseHand "4D 8D")
   in (assertEqual "flush draw" 9 (PHE.outCount flop hand))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
      testCase "pocket pair" testOutCounterPocketPair
      , testCase "one overcard" testOutCounterOneOvercard
      , testCase "two pair to full house" testOutCounterTwoPairToFullHouse
      , testCase "two overcards" testOutCounterTwoOvercard
      , testCase "1 overcard count" testOverCardCount
      , testCase "2 overcard count" testTwoOverCardCount
      , testCase "set to full house / four kind" testSetToFullHouseFourKind
      , testCase "flush draw" testFlushDraw
  ]

runner = defaultMain tests2
