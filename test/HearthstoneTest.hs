module HearthstoneTest where

import Hearthstone
import Test.Tasty
import Test.Tasty.HUnit

testPlayingAHand =
  let p1 = Player 30 1 [0,0,1,1]
      p2 = Player 30 1 []
      (Player p1h _ p1hand, Player p2h _ _) = playHand p1 p2
  in (assertEqual "health" 30 p1h)
     >> (assertEqual "hand" [0,0,1] p1hand )
     >> (assertEqual "p2 health" 29 p2h)


tests :: TestTree
tests = testGroup "HeartstoneTests"
  [
    testCase "undo game one move" testPlayingAHand
  ]
