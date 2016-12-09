module Hearthstone where

import qualified System.Random.Shuffle as Rand
import Data.List(sort)
import Control.Monad.Random

type Card = Int
type Deck = [Card]
type Hand = [Card]

type Health = Int
type Mana = Int
data Player = Player Health Mana Hand Deck

fullDeck :: [Card]
fullDeck = [0,0,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,7,8]

makeDeck :: (MonadRandom m) => m Deck
makeDeck = Rand.shuffleM fullDeck

playHand :: (Player, Player) -> (Player, Player)
playHand ((Player health1 mana1 cards1 d1), (Player health2 p2m p2c d2)) =
  let newCard = head d1
      newDeck = drop 1 d1
      (totalDamage, handLeft, manaLeft) =
        foldr
        (\card (dam, hand, mana) ->
           if (card <= mana) then
             (dam+card, hand, mana-card)
           else
             (dam, card:hand,mana))
        (0, [], mana1)
        (sort (newCard:cards1))
      p2 = Player (health2 - totalDamage) p2m p2c d2
      p1 = Player health1 manaLeft handLeft newDeck
  in (p2, p1)

playRound :: (Player, Player) -> (Player, Player)
playRound a =
  let (p2, p1) = playHand a
      (p1a, p2a) = playHand (p2,p1)
  in (p1a, p2a)

main :: IO ()
main = do
  a <- makeDeck
  b <- makeDeck
  putStrLn $ show a
  putStrLn $ show b
