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

isOfAKind :: Bool -> Int -> Hand -> Maybe Face
isOfAKind reverse size hand =
  let faceGrouped = groupBy sameFace hand
      moreThanN = filter (\a -> length a == size) faceGrouped
  in if length moreThanN > 0 then
      let sorted = sortBy (compare) $ map (face . head) moreThanN
      in Just (head sorted)
     else Nothing

isTwoOfAKind :: Hand -> Maybe Face
isTwoOfAKind hand = isOfAKind False 2 hand

isThreeOfAKind :: Hand -> Maybe Face
isThreeOfAKind hand = isOfAKind False 3 hand

isFourOfAKind :: Hand -> Maybe Face
isFourOfAKind hand = isOfAKind False 4 hand

isFullHouse :: Hand -> Maybe (Face,Face)
isFullHouse hand = do
  twoKind <- isTwoOfAKind hand
  threeKind <- isThreeOfAKind hand
  return (threeKind, twoKind)

isTwoPair :: Hand -> Maybe (Face,Face)
isTwoPair hand = do
  twoKindA <- isOfAKind False 2 hand
  twoKindB <- isOfAKind True 2 hand
  if (twoKindA /= twoKindB) then
    return (twoKindA, twoKindB)
  else Nothing
