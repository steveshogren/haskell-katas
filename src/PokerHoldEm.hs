module PokerHoldEm where

import PokerHands as PH

type HoldEmHand = (PH.Card,PH.Card)

type Board = [PH.Card]

percentage :: Board -> HoldEmHand -> HoldEmHand -> (Integer, Integer)
percentage board hand1 hand2 =
  (100, 0)
