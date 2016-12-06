module Hearthstone where

import qualified Data.Map as M
import qualified System.Random.Shuffle as Rand
import Control.Monad.Random
import Control.Monad

type Card = Int
type Deck = [Card]
type Hand = [Card]

type Health = Int
type Mana = Int
data Player = Player Health Mana

fullDeck :: [Card]
fullDeck = [0,0,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,7,8]

makeDeck :: (MonadRandom m) => m Deck
makeDeck = Rand.shuffleM fullDeck

playHand :: t -> t1 -> t2 -> (t, t1, t2)
playHand p1 p2 h1 = (p1, p2, h1)

main :: IO ()
main = do
  a <- makeDeck
  b <- makeDeck
  putStrLn $ show a
  putStrLn $ show b
