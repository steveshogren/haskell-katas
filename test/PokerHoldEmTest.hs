module PokerHoldEmTest where

import Test.Tasty
import Test.Tasty.HUnit

import PokerHands
import PokerHoldEm as PHE


p1Hand = (Card Ace Spades,Card Ace Diamonds)
p2Hand = (Card Two Spades,Card Two Diamonds)
board1 = []

testPercentage :: Assertion
testPercentage =
   (assertEqual "should correctly determine winner percentage"
    (100,0)
    (PHE.percentage [] p1Hand p2Hand ))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
      testCase "percentages" testPercentage
  ]

runner = defaultMain tests2
