module HearthstoneTest where

import Hearthstone
import Test.Tasty
import Test.Tasty.HUnit

testUndoOnlyOneMove :: IO ()
testUndoOnlyOneMove =
  assertEqual "Undoing the move" 1 1


tests :: TestTree
tests = testGroup "HeartstoneTests"
  [
    testCase "undo game one move" testUndoOnlyOneMove
  ]

main :: IO ()
main = defaultMain tests
