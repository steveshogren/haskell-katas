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
      h2 = fromMaybe [] (parseHand "2H 2S JH JH KH")
  in (assertEqual "" (Just Two) (isTwoOfAKind h1))
     >> (assertEqual "" Nothing (isTwoOfAKind []))
     >> (assertEqual "" (Just Jack) (isTwoOfAKind h2))

testIsThreeOfAKind :: Assertion
testIsThreeOfAKind =
  let h1 = fromMaybe [] (parseHand "2H 2S 2H QH KH")
      h2 = fromMaybe [] (parseHand "2H 2S JH JH JH")
  in (assertEqual "" (Just Two) (isThreeOfAKind h1))
     >> (assertEqual "" Nothing (isThreeOfAKind []))
     >> (assertEqual "" (Just Jack) (isThreeOfAKind h2))

testIsFourOfAKind :: Assertion
testIsFourOfAKind =
  let h1 = fromMaybe [] (parseHand "2H 2S 2H 2H KH")
  in (assertEqual "" (Just Two) (isFourOfAKind h1))
     >> (assertEqual "" Nothing (isFourOfAKind []))

testIsFullHouse :: Assertion
testIsFullHouse =
  let h1 = fromMaybe [] (parseHand "2H 2S 2H QH QH")
      h2 = fromMaybe [] (parseHand "2H 2S JH JH JH")
  in (assertEqual "" (Just (Two, Queen)) (isFullHouse h1))
     >> (assertEqual "" Nothing (isFullHouse []))
     >> (assertEqual "" (Just (Jack, Two)) (isFullHouse h2))

testIsTwoPair :: Assertion
testIsTwoPair =
  let h1 = fromMaybe [] (parseHand "2H 2S 3H QH QH")
      h2 = fromMaybe [] (parseHand "2H 2S JH JH JH")
      h3 = fromMaybe [] (parseHand "2H 2S 3H JH JH")
  in (assertEqual "" (Just (Queen, Two)) (isTwoPair h1))
     >> (assertEqual "" Nothing (isTwoPair h2))
     >> (assertEqual "" (Just (Jack, Two)) (isTwoPair h3))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
    testCase "isTwoOfAKind" testIsTwoOfAKind
    , testCase "isThreeOfAKind" testIsThreeOfAKind
    , testCase "isFullHouse" testIsFullHouse
    , testCase "parse" testParse
  ]

runner = defaultMain tests2
