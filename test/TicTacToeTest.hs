module TicTacToeTest where

import qualified Data.Map as M
import TicTacToe
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit


testUndo :: IO ()
testUndo =
  let f = NoMoves
      partWay = (f >>== midCen >>== topRight)
  in case partWay of
      Unfinished g ->
        let undone = undoMove g
            expectedUndo = XMove (X ((1,1),ONone))
            expectedBefore = OMove (O ((0,2),X ((1,1),ONone)))
        in (assertEqual "Undoing the move" expectedUndo undone >>
            assertEqual "Before undo" expectedBefore g)
      _ -> assertFailure "Game should be Unfinished"

testWinning :: Assertion
testWinning =
  let f = NoMoves
      game = stringifyGame (f >>== midCen >>== topLeft >>== bottomCen >>== topCen >>== bottomRight >>== topRight)
      expectedString = ["Player: O Won!",
                        "OOO",
                        " X ",
                        " XX"]
  in (assertEqual "Player 0 Winning" expectedString game)

testUnfinished :: Assertion
testUnfinished =
  let f = NoMoves
      game = stringifyGame (f >>== midCen >>== topLeft >>== bottomCen >>== topCen >>== bottomRight)
      expectedString = ["OO ",
                        " X ",
                        " XX"]
  in (assertEqual "Game unfinished" expectedString game)

tests :: TestTree
tests = testGroup "TicTacToeTests"
  [
    testCase "undo game" testUndo,
    testCase "game unfinished" testUnfinished,
    testCase "winning game" testWinning
  ]

runTests :: IO ()
runTests = defaultMain tests
