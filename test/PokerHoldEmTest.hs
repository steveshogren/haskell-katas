module PokerHoldEmTest where

import Test.Tasty
import Test.Tasty.HUnit

import PokerHands
import PokerHoldEm as PHE


pHand1 = (Card Ace Spades,Card Ace Diamonds)
pHand2 = (Card Two Spades,Card Two Diamonds)
board1 = []

testPercentage :: Assertion
testPercentage =
   (assertEqual "should correctly determine winner percentage"
    100
    (PHE.percentage [] 47 pHand1))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
      testCase "percentages" testPercentage
  ]

runner = defaultMain tests2
