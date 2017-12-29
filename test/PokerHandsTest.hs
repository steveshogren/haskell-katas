module PokerHandsTest where

import PokerHands
import Test.Tasty
import Test.Tasty.HUnit

h2 = Card Two Hearts
h10 = Card Ten Hearts
hj = Card Jack Hearts
hq = Card Queen Hearts
hk = Card King Hearts
ha = Card Ace Hearts
ca = Card Ace Clubs
sa = Card Ace Spades
da = Card Ace Diamonds

testParse :: Assertion
testParse =
  (assertEqual "" (Just h2) (parseCard "2H"))
  >> (assertEqual "" (Just h10) (parseCard "10H"))

testIsTwoOfAKind :: Assertion
testIsTwoOfAKind =
  (assertEqual "" True isTwoOfAKind)

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
    testCase "isTwoOfAKind" testIsTwoOfAKind
    , testCase "parse" testParse
  ]

runner = defaultMain tests2
