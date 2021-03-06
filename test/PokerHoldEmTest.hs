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

testWinPercentage :: Assertion
testWinPercentage =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       p1 = fromMaybe [] (parseHand "QD 4H")
       p2 = fromMaybe [] (parseHand "3D 9H")

       p3 = fromMaybe [] (parseHand "QD 4H")
       p4 = fromMaybe [] (parseHand "6D 5H")

       flop2 = fromMaybe [] (parseHand "3H 4S 5H")
       p5 = fromMaybe [] (parseHand "6D 4H")
       p6 = fromMaybe [] (parseHand "3D 9H")

   in (assertEqual "win %" (95.7, 4.3) (PHE.winPercentage flop p1 p2))
      >> (assertEqual "win %" (100,0) (PHE.winPercentage flop p3 p4))
      -- >> (assertEqual "win %" (100,0) (PHE.winPercentage flop2 p5 p6))

testOutCounterStraightWithTwo :: Assertion
testOutCounterStraightWithTwo =
   let flop = fromMaybe [] (parseHand "3D 4H 5S")
       h1 = fromMaybe [] (parseHand "2D 9H")
       flop2 = fromMaybe [] (parseHand "JD QH KS")
       h2 = fromMaybe [] (parseHand "AD 3H")
   in (assertEqual "straigt two" [(4, Straight Six)] (PHE.outCount flop h1))
      >> (assertEqual "straigt ace" [(4, Straight Ace)] (PHE.outCount flop2 h2))

testOutCounterPocketPair :: Assertion
testOutCounterPocketPair =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       pocketHand = fromMaybe [] (parseHand "4D 4H")
       nonPocketHand = fromMaybe [] (parseHand "3D 2H")
   in (assertEqual "pocket pair" [(2, ThreeOfAKind Four)] (PHE.outCount flop pocketHand))
      >> (assertEqual "no pocket pair still counts" [(2, ThreeOfAKind Two)] (PHE.outCount flop nonPocketHand))

testOutCounterOneOvercard :: Assertion
testOutCounterOneOvercard =
   let flop = fromMaybe [] (parseHand "5D 8H 9S")
       hand = fromMaybe [] (parseHand "QH 2H")
   in (assertEqual "one overcard" [(3, TwoOfAKind Queen)] (PHE.outCount flop hand))

testOutCounterTwoOvercard :: Assertion
testOutCounterTwoOvercard =
   let flop = fromMaybe [] (parseHand "5D 8H 2S")
       hand = fromMaybe [] (parseHand "QH 9H")
   in (assertEqual "two overcard"
       [(3,TwoOfAKind Queen), (3,TwoOfAKind Nine)]
       (PHE.outCount flop hand))

testOutCounterTwoPairToFullHouse :: Assertion
testOutCounterTwoPairToFullHouse =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       hand = fromMaybe [] (parseHand "QH 9H")
   in (assertEqual "two pair to full"
       [(2, FullHouse (Queen, Nine)), (2, FullHouse (Nine, Queen))]
       (PHE.outCount flop hand))

testSetToFullHouseFourKind :: Assertion
testSetToFullHouseFourKind =
   let flop = fromMaybe [] (parseHand "QD 2H 9S")
       hand = fromMaybe [] (parseHand "QH QS")
   in (assertEqual "set to full house / four kind"
       [(1, FourOfAKind Queen),
        (3, FullHouse (Queen, Nine)),
        (3, FullHouse (Queen, Two ))]
       (PHE.outCount flop hand))

testOpenEndedStraightDraw :: Assertion
testOpenEndedStraightDraw =
   let flop = fromMaybe [] (parseHand "2S 5D 6S")
       hand = fromMaybe [] (parseHand "7D 8D")
   in (assertEqual "is open straight "
      [(4, Straight Eight),
       (4, Straight Nine)]
       (PHE.outCount flop hand))

testFlushDraw :: Assertion
testFlushDraw =
   let flop = fromMaybe [] (parseHand "QD 2D 9S")
       hand = fromMaybe [] (parseHand "4D 8D")
   in (assertEqual "flush draw"
       [(9, Flush Diamonds)]
       (PHE.outCount flop hand))

testOpenEndedStraightFlushDraw :: Assertion
testOpenEndedStraightFlushDraw =
   let flop = fromMaybe [] (parseHand "10H 3H JS")
       hand = fromMaybe [] (parseHand "QH 9H")
   in (assertEqual "is open ended straight flush draw"
       [(9, Flush Hearts),
        -- already counting the outs for the QH and KH in the 9 hearts
        (3, Straight Queen),
        (3, Straight King)]
       (PHE.outCount flop hand))

testFourCardsFlush :: Assertion
testFourCardsFlush =
   let cards = fromMaybe [] (parseHand "QD 2D 9S 5D 7D")
   in (assertEqual "four cards flush" (Just Diamonds) (PHE.fourCardsFlush cards))

testInsideStraightDraw :: Assertion
testInsideStraightDraw =
   let flop = fromMaybe [] (parseHand "QC 8D 4S")
       hand = fromMaybe [] (parseHand "JS 10D")
   in (assertEqual "inside straight draw"
       [(4, Straight Queen)]
       (PHE.outCount flop hand))

testInsideStraightFlushDraw :: Assertion
testInsideStraightFlushDraw =
   let flop = fromMaybe [] (parseHand "QD 8D 4S")
       hand = fromMaybe [] (parseHand "JD 10D")
   in (assertEqual "inside straight flush draw"
       [(4, Straight Queen),
        (8, Flush Diamonds)]
       (PHE.outCount flop hand))

testIsInsideStraight :: Assertion
testIsInsideStraight =
   let cards1 = fromMaybe [] (parseHand "4S 8D 10D JS QC")
       cards2 = fromMaybe [] (parseHand "4S 8D 9D JS QC")
       cards3 = fromMaybe [] (parseHand "4S 8D 9D 10S QC")
       cards4 = fromMaybe [] (parseHand "4S 4D 9D 10S QC")
   in (assertEqual "card 2 missing" [Queen] (PHE.isInsideStraight cards1))
      >> (assertEqual "card 3 missing" [Queen] (PHE.isInsideStraight cards2))
      >> (assertEqual "card 4 missing" [Queen] (PHE.isInsideStraight cards3))
      >> (assertEqual "isnt inside straight" [] (PHE.isInsideStraight cards4))

testHandPermutations :: Assertion
testHandPermutations =
   let cards = fromMaybe [] (parseHand "2D 3D 4D 5D 6D 7D 8D")
   in (assertEqual "all permutations"
       [[6,7,8,9,10],[6,7,8,9,11],[6,7,8,9,12],[6,7,8,10,11],[6,7,8,10,12],[6,7,8,11,12],[6,7,9,10,11],[6,7,9,10,12],[6,7,9,11,12],[6,7,10,11,12],[6,8,9,10,11],[6,8,9,10,12],[6,8,9,11,12],[6,8,10,11,12],[6,9,10,11,12],[7,8,9,10,11],[7,8,9,10,12],[7,8,9,11,12],[7,8,10,11,12],[7,9,10,11,12],[8,9,10,11,12]]
       (map (\hand -> map (fromEnum . face) hand) $ PHE.possibleHands cards))

testCurrentHand :: Assertion
testCurrentHand =
   let cards = fromMaybe [] (parseHand "2D 2S QD QC 6D 7S 8D")
   in (assertEqual "detect current hand"
      (TwoPair (Queen, Two))
      (PHE.bestHand cards))

testWinner :: Assertion
testWinner =
   let cards = fromMaybe [] (parseHand "QD 2C 6D 7S 8D")
       p1 = fromMaybe [] (parseHand "QD 2S")
       p2 = fromMaybe [] (parseHand "2D 3S")
   in (assertEqual "winner"
      (Left $ TwoPair (Queen, Two))
      (PHE.winner p1 p2 cards))

testOddsFlopToTurn :: Assertion
testOddsFlopToTurn =
   (assertEqual "1" 2.2 (PHE.oddsFlopToTurn 1))
   >> (assertEqual "20" 43.5 (PHE.oddsFlopToTurn 20))

testHighestCard :: Assertion
testHighestCard =
   let cards = fromMaybe [] (parseHand "2C QS 2C 6D 7S 8D")
   in (assertEqual "highest card" (Card Queen Spades) (PHE.highestCard cards))

tests2 :: TestTree
tests2 = testGroup "PokerHandsTests"
  [
      testCase "pocket pair to set" testOutCounterPocketPair
      , testCase "straight with two" testOutCounterStraightWithTwo
      , testCase "one overcard" testOutCounterOneOvercard
      , testCase "two pair to full house" testOutCounterTwoPairToFullHouse
      , testCase "two overcards to overpair" testOutCounterTwoOvercard
      , testCase "set to full house / four kind" testSetToFullHouseFourKind
      , testCase "open ended straight draw" testOpenEndedStraightDraw
      , testCase "flush draw" testFlushDraw
      , testCase "inside straight draw" testInsideStraightDraw
      , testCase "inside straight and flush draw" testInsideStraightFlushDraw
      , testCase "open straight and flush draw" testOpenEndedStraightFlushDraw

        -- helpers
      , testCase "is inside straigh" testIsInsideStraight
      , testCase "1 overcard count" testOverCardCount
      , testCase "2 overcard count" testTwoOverCardCount
      , testCase "four cards flush" testFourCardsFlush

      , testCase "permutations" testHandPermutations

      , testCase "current hand" testCurrentHand
      , testCase "winner" testWinner

      , testCase "oddsToTurn" testOddsFlopToTurn

      , testCase "highestCards" testHighestCard

      , testCase "win %" testWinPercentage 
  ]

runner = defaultMain tests2
