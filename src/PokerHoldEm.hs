module PokerHoldEm where

import PokerHands as PH

type HoldEmHand = (PH.Card,PH.Card)

type Board = [PH.Card]

percentage :: Board -> Integer -> HoldEmHand -> Integer
percentage board cardsRemaining hand = 100
