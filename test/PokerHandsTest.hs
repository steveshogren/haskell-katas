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
  in (assertEqual "" (Just $ TwoOfAKind Two) (isTwoOfAKind h1))
     >> (assertEqual "" Nothing (isTwoOfAKind []))
     >> (assertEqual "" (Just $ TwoOfAKind Jack) (isTwoOfAKind h2))
     >> (assertEqual "" (Just $ TwoOfAKind Jack) (isTwoOfAKind h2))

testIsThreeOfAKind :: Assertion
testIsThreeOfAKind =
  let h1 = fromMaybe [] (parseHand "2H 2S 2H QH KH")
      h2 = fromMaybe [] (parseHand "2H 2S JH JH JH")
  in (assertEqual "" (Just $ ThreeOfAKind Two) (isThreeOfAKind h1))
     >> (assertEqual "" Nothing (isThreeOfAKind []))
     >> (assertEqual "" (Just $ ThreeOfAKind Jack) (isThreeOfAKind h2))

testIsFourOfAKind :: Assertion
testIsFourOfAKind =
  let h1 = fromMaybe [] (parseHand "2H 2S 2H 2H KH")
  in (assertEqual "" (Just $ FourOfAKind Two) (isFourOfAKind h1))
     >> (assertEqual "" Nothing (isFourOfAKind []))

testIsFullHouse :: Assertion
testIsFullHouse =
  let h1 = fromMaybe [] (parseHand "2H 2S 2H QH QH")
      h2 = fromMaybe [] (parseHand "2H 2S JH JH JH")
  in (assertEqual "" (Just $ FullHouse (Two, Queen)) (isFullHouse h1))
     >> (assertEqual "" Nothing (isFullHouse []))
     >> (assertEqual "" (Just $ FullHouse (Jack, Two)) (isFullHouse h2))

testIsTwoPair :: Assertion
testIsTwoPair =
  let h1 = fromMaybe [] (parseHand "2H 2S 3H QH QH")
      h2 = fromMaybe [] (parseHand "2H 2S JH JH JH")
      h3 = fromMaybe [] (parseHand "2H 2S 3H JH JH")
  in (assertEqual "" (Just $ TwoPair (Queen, Two)) (isTwoPair h1))
     >> (assertEqual "" Nothing (isTwoPair h2))
     >> (assertEqual "" (Just $ TwoPair (Jack, Two)) (isTwoPair h3))

testIsStraight :: Assertion
testIsStraight =
  let h1 = fromMaybe [] (parseHand "7H 3S 4H 5H 6H")
      h2 = fromMaybe [] (parseHand "2H 2S 3H JH JH")
  in (assertEqual "" (Just $ Straight Seven) (isStraight h1))
     >> (assertEqual "" Nothing (isStraight h2))

testIsFlush :: Assertion
testIsFlush =
  let h1 = fromMaybe [] (parseHand "7H 3H 4H 5H 6H")
      h2 = fromMaybe [] (parseHand "2H 2S 3H JH JH")
  in (assertEqual "" (Just $ Flush Hearts) (isFlush h1))
     >> (assertEqual "" Nothing (isFlush h2))

hand s = fromMaybe [] (parseHand s)

testIsRoyalFlush :: Assertion
testIsRoyalFlush =
  (assertEqual "" (Just $ RoyalFlush Hearts) (isRoyalFlush $ hand "JH KH QH AH 10H"))
     >> (assertEqual "" Nothing (isRoyalFlush $ hand  "2H 2H 3H JH JH"))

testDetectHand :: Assertion
testDetectHand =
  (assertEqual "" (Just $ RoyalFlush Hearts) (convertHand "JH KH QH AH 10H"))
  >> (assertEqual "" (Just $ Flush Hearts) (convertHand  "7H 3H 4H 5H 6H"))
  >> (assertEqual "" (Just $ Straight Seven) (convertHand  "7H 3S 4H 5H 6H"))
  >> (assertEqual "" (Just $ TwoPair (Queen, Two)) (convertHand  "2H 2S 3H QH QH"))
  >> (assertEqual "" (Just $ TwoOfAKind Two) (convertHand "2H 2S JH QH KH"))
  >> (assertEqual "" (Just $ ThreeOfAKind Two) (convertHand "2H 2S 2H QH KH"))
  >> (assertEqual "" (Just $ FourOfAKind Two) (convertHand  "2H 2S 2H 2H KH"))
  >> (assertEqual "" (Just $ FullHouse (Two, Queen)) (convertHand  "2H 2S 2H QH QH"))
  >> (assertEqual "" (Nothing) (convertHand  "uH 2S 2H QH QH"))
  >> (assertEqual "" (Nothing) (convertHand  "5Y2S2HQHQH"))


tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
    testCase "isTwoOfAKind" testIsTwoOfAKind
    , testCase "isThreeOfAKind" testIsThreeOfAKind
    , testCase "isFullHouse" testIsFullHouse
    , testCase "isTwoPair" testIsTwoPair
    , testCase "isFourOfaKind" testIsFourOfAKind
    , testCase "isStraight" testIsStraight
    , testCase "isFlush" testIsFlush
    , testCase "isRoyalFlush" testIsRoyalFlush
    , testCase "detectHand" testDetectHand
    , testCase "parse" testParse
  ]

runner = defaultMain tests2
