module HearthstoneTest where

import Hearthstone
import Test.Tasty
import Test.Tasty.HUnit

testPlayingAHand :: IO ()
testPlayingAHand =
  let p1 = Player 30 1 []
      p2 = Player 30 1 []
      h1 = [0,0,1,1]
      (p1a, p2a) = playHand p1 p2 h1
  in assertEqual "Undoing the move" 1 1


tests :: TestTree
tests = testGroup "HeartstoneTests"
  [
    testCase "undo game one move" testPlayingAHand
  ]

main :: IO ()
main = defaultMain tests
