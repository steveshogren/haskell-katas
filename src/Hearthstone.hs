module Hearthstone where

import qualified System.Random.Shuffle as Rand
import Control.Monad.Random

type Card = Int
type Deck = [Card]
type Hand = [Card]

type Health = Int
type Mana = Int
data Player = Player Health Mana Hand

fullDeck :: [Card]
fullDeck = [0,0,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,7,8]

makeDeck :: (MonadRandom m) => m Deck
makeDeck = Rand.shuffleM fullDeck

playHand :: Player -> Player -> (Player, Player)
playHand (Player health1 mana1 cards1) (Player health2 p2m p2c) =
  let (totalDamage, handLeft, manaLeft) =
        foldr
        (\card (dam, hand, mana) ->
           if (card <= mana) then
             (dam+card, hand, mana-card)
           else
             (dam, [card]++hand,mana))
        (0, [], mana1)
        cards1
  in (Player health1 manaLeft handLeft, Player (health2 - totalDamage) p2m p2c)

main :: IO ()
main = do
  a <- makeDeck
  b <- makeDeck
  putStrLn $ show a
  putStrLn $ show b
