module PokerHands where

data Suit = Hearts | Clubs | Diamonds | Spades
  deriving (Show, Eq)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq)

data Card = Card Face Suit
  deriving (Show, Eq)

type Hand = [Card]

isTwoOfAKind :: Bool
isTwoOfAKind = True

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
parseHand str = Nothing
