module PokerHandsTest where

import PokerHands
import Test.Tasty
import Test.Tasty.HUnit

testIsTwoOfAKind :: Assertion
testIsTwoOfAKind =
  (assertEqual "" True isTwoOfAKind)

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
    testCase "isTwoOfAKind" testIsTwoOfAKind
  ]

runner = defaultMain tests2
