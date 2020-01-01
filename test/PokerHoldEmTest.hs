module PokerHoldEmTest where

import Test.Tasty
import Test.Tasty.HUnit

import PokerHands
import PokerHoldEm as PHE
import Data.Maybe(fromMaybe)

pHand2 = (Card Two Spades,Card Two Diamonds)
board1 = []

testOutCounter :: Assertion
testOutCounter =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       pHand1 = (Card Four Spades,Card Four Diamonds)
   in (assertEqual "detects two outs"
       2
       (PHE.outCount flop 47 pHand1))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
      testCase "percentages" testOutCounter
  ]

runner = defaultMain tests2
