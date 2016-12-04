module Hearthstone where

import qualified Data.Map as M
import qualified System.Random.Shuffle as Rand
import Control.Monad.Random
import Control.Monad

type Health = Int
type Mana = Int
data Player = Player Health Mana

type Card = Int
type Deck = [Card]

fullDeck = [0,0,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,7,8]

makeDeck :: (MonadRandom m) => m Deck
makeDeck = Rand.shuffleM fullDeck

main = do
  a <- makeDeck
  b <- makeDeck
  putStrLn $ show a
  putStrLn $ show b
