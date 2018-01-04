module PokerHands where

import Data.List.Split(splitOn)
import Data.List(any, groupBy, sortBy)
import Control.Monad(mapM)

data Suit = Hearts | Clubs | Diamonds | Spades
  deriving (Show, Eq)

data Face = Ace| King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
  deriving (Show, Eq, Ord)

data Card = Card Face Suit
  deriving (Show, Eq)

type Hand = [Card]

parseSuit :: Char -> Maybe Suit
parseSuit 'S' = Just Spades
parseSuit 'H' = Just Hearts
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit _ = Nothing

parseFace :: Char -> Maybe Face
parseFace '2' = Just Two
parseFace '3' = Just Three
parseFace '4' = Just Four
parseFace '5' = Just Five
parseFace '6' = Just Six
parseFace '7' = Just Seven
parseFace '8' = Just Eight
parseFace '9' = Just Nine
parseFace '1' = Just Ten
parseFace 'J' = Just Jack
parseFace 'Q' = Just Queen
parseFace 'K' = Just King
parseFace 'A' = Just Ace
parseFace _ = Nothing

parseCard :: String -> Maybe Card
parseCard [first,second] = do
  suit <- parseSuit second
  face <- parseFace first
  return $ Card face suit
parseCard [first,_,third] = do
  suit <- parseSuit third
  face <- parseFace first
  return $ Card face suit
parseCard _ = Nothing

parseHand :: String -> Maybe Hand
parseHand str =
  let cardStrs = splitOn " " str
  in mapM parseCard cardStrs

sameFace :: Card -> Card -> Bool
sameFace (Card f1 _) (Card f2 _) = f1 == f2

face :: Card -> Face
face (Card f1 _) = f1


isTwoOfAKind :: Hand -> Maybe Face
isTwoOfAKind hand =
  let faceGrouped = groupBy sameFace hand
      moreThanTwo = filter (\a -> length a == 2) faceGrouped
  in if length moreThanTwo > 0 then
      let sorted = sortBy (compare) $ map (face . head) moreThanTwo
      in Just (head sorted)
     else Nothing

isThreeOfAKind :: Hand -> Maybe Face
isThreeOfAKind hand =
  let faceGrouped = groupBy sameFace hand
      moreThanThree = filter (\a -> length a == 3) faceGrouped
  in if length moreThanThree > 0 then
      let sorted = sortBy (compare) $ map (face . head) moreThanThree
      in Just (head sorted)
     else Nothing

isFullHouse :: Hand -> Maybe (Face,Face)
isFullHouse hand = do
  twoKind <- isTwoOfAKind hand
  threeKind <- isThreeOfAKind hand
  return (threeKind, twoKind)
