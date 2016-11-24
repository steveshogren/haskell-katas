module TicTacToeTest where

import TicTacToe
import Test.Tasty
import Test.Tasty.HUnit

testUndoOnlyOneMove :: IO ()
testUndoOnlyOneMove =
  let f = NoMoves
      partWay = (f >>== midCen)
  in case partWay of
      Unfinished g ->
        let undone = undoMove g
            expectedBefore = XMove (X ((1,1),ONone))
            expectedAfter = NoMoves
        in (assertEqual "Undoing the move" expectedAfter undone >>
            assertEqual "Before undo" expectedBefore g)
      _ -> assertFailure "Game should be Unfinished"

testUndo :: IO ()
testUndo =
  let f = NoMoves
      partWay = (f >>== midCen >>== topRight)
  in case partWay of
      Unfinished g ->
        let undone = undoMove g
            expectedUndo = Unfinished $ XMove (X ((1,1),ONone))
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
    testCase "undo game one move" testUndoOnlyOneMove,
    testCase "undo game" testUndo,
    testCase "game unfinished" testUnfinished,
    testCase "winning game" testWinning
  ]

main :: IO ()
main = defaultMain tests
