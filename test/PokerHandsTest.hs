module PokerHandsTest where

import PokerHands
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe(fromMaybe)

h2 = Card Two Hearts
h10 = Card Ten Hearts
hj = Card Jack Hearts
hq = Card Queen Hearts
hk = Card King Hearts
ha = Card Ace Hearts
ca = Card Ace Clubs
sa = Card Ace Spades
da = Card Ace Diamonds

hand1 = [h2,h10,hj,hq,hk]

testParse :: Assertion
testParse =
  (assertEqual "" Nothing (parseCard "Y3"))
  >> (assertEqual "" (Just h2) (parseCard "2H"))
  >> (assertEqual "" (Just hq) (parseCard "QH"))
  >> (assertEqual "" (Just h10) (parseCard "10H"))
  >> (assertEqual "" Nothing (parseCard "H"))
  >> (assertEqual "" (Just hand1) (parseHand "2H 10H JH QH KH"))

testIsTwoOfAKind :: Assertion
testIsTwoOfAKind =
  let h1 = fromMaybe [] (parseHand "2H 2S JH QH KH")
  in (assertEqual "" True (isTwoOfAKind h1))
     >> (assertEqual "" False (isTwoOfAKind []))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
    testCase "isTwoOfAKind" testIsTwoOfAKind
    , testCase "parse" testParse
  ]

runner = defaultMain tests2
