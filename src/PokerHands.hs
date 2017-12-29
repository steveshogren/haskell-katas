module PokerHands where

data Suit = Hearts | Clubs | Diamonds | Spades
  deriving (Show, Eq)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq)

data Card = Card Face Suit
  deriving (Show, Eq)

isTwoOfAKind :: Bool
isTwoOfAKind = True

parseCard a = Nothing

