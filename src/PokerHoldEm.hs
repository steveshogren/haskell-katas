module PokerHoldEm where

import PokerHands as PH

type HoldEmHand = (PH.Card,PH.Card)

type Board = [PH.Card]

outCount :: Board -> Integer -> HoldEmHand -> Integer
outCount board cardsRemaning hand = 0

percentage :: Board -> Integer -> HoldEmHand -> Integer
percentage board cardsRemaining hand = 100
