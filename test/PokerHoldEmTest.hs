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
      >> (assertEqual "no pocket pair still counts" 2 (PHE.outCount flop nonPocketHand))

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

testOpenEndedStraightDraw :: Assertion
testOpenEndedStraightDraw =
   let flop = fromMaybe [] (parseHand "2S 5D 6S")
       hand = fromMaybe [] (parseHand "7D 8D")
       flop1 = fromMaybe [] (parseHand "4S 5D 6S")
       hand1 = fromMaybe [] (parseHand "QD 7D")
   in (assertEqual "is open straight " 8 (PHE.outCount flop hand))
      >> (assertEqual "doesn't have to use both hand cards" 8 (PHE.outCount flop1 hand1))

testFlushDraw :: Assertion
testFlushDraw =
   let flop = fromMaybe [] (parseHand "QD 2D 9S")
       hand = fromMaybe [] (parseHand "4D 8D")
   in (assertEqual "flush draw" 9 (PHE.outCount flop hand))

testOpenEndedStraightFlushDraw :: Assertion
testOpenEndedStraightFlushDraw =
   let flop = fromMaybe [] (parseHand "10H 3H JS")
       hand = fromMaybe [] (parseHand "QD KD")
   in (assertEqual "is open ended straight flush draw" 15 (PHE.outCount flop hand))

testFourCardsFlush :: Assertion
testFourCardsFlush =
   let cards = fromMaybe [] (parseHand "QD 2D 9S 5D 7D")
   in (assertEqual "four cards flush" True (PHE.fourCardsFlush cards))

testInsideStraightDraw :: Assertion
testInsideStraightDraw =
   let flop = fromMaybe [] (parseHand "QC 8D 4S")
       hand = fromMaybe [] (parseHand "JS 10D")
   in (assertEqual "inside straight draw" 4 (PHE.outCount flop hand))

testInsideStraightFlushDraw :: Assertion
testInsideStraightFlushDraw =
   let flop = fromMaybe [] (parseHand "QD 8D 4S")
       hand = fromMaybe [] (parseHand "JD 10D")
   in (assertEqual "inside straight flush draw" 12 (PHE.outCount flop hand))

testIsInsideStraight :: Assertion
testIsInsideStraight =
   let cards1 = fromMaybe [] (parseHand "4S 8D 10D JS QC")
       cards2 = fromMaybe [] (parseHand "4S 8D 9D JS QC")
       cards3 = fromMaybe [] (parseHand "4S 8D 9D 10S QC")
       cards4 = fromMaybe [] (parseHand "4S 4D 9D 10S QC")
   in (assertEqual "card 2 missing" True (PHE.isInsideStraight cards1))
      >> (assertEqual "card 3 missing" True (PHE.isInsideStraight cards2))
      >> (assertEqual "card 4 missing" True (PHE.isInsideStraight cards3))
      >> (assertEqual "isnt inside straight" False (PHE.isInsideStraight cards4))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
      testCase "pocket pair to set" testOutCounterPocketPair
      , testCase "one overcard" testOutCounterOneOvercard
      , testCase "two pair to full house" testOutCounterTwoPairToFullHouse
      , testCase "two overcards to overpair" testOutCounterTwoOvercard
      , testCase "set to full house / four kind" testSetToFullHouseFourKind
      , testCase "open ended straight draw" testOpenEndedStraightDraw
      , testCase "flush draw" testFlushDraw
      , testCase "inside straight draw" testInsideStraightDraw
      , testCase "inside straight and flush draw" testInsideStraightFlushDraw

      -- helpers
      , testCase "is inside straigh" testIsInsideStraight
      , testCase "1 overcard count" testOverCardCount
      , testCase "2 overcard count" testTwoOverCardCount
      , testCase "four cards flush" testFourCardsFlush
  ]

runner = defaultMain tests2
