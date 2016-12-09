module HearthstoneTest where

import Hearthstone
import Test.Tasty
import Test.Tasty.HUnit

testPlayingAHand :: IO ()
testPlayingAHand =
  let p1 = Player 30 6 [5,0,0,1,1]
      p2 = Player 30 1 []
      (Player p1h _ p1hand, Player p2h _ p2hand) = playHand p1 p2
  in (assertEqual "health" 30 p1h)
     >> (assertEqual "p2 health" 24 p2h)
     >> (assertEqual "hand" [1] p1hand )
     >> (assertEqual "p2 hand" [] p2hand)

tests :: TestTree
tests = testGroup "HeartstoneTests"
  [
    testCase "playing just one card" testPlayingAHand
  ]
