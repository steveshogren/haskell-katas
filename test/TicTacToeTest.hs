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

testDidWin :: Game -> Bool
testDidWin (Unfinished a) = didWin a
testDidWin (Finished a) = didWin a
testDidWin _ = False

testWinConditions :: Assertion
testWinConditions =
  let f = NoMoves
      topRowWin = testDidWin $ f >>== topLeft >>== bottomCen >>== topCen >>== bottomRight >>== topRight
      midRowWin = testDidWin $ f >>== midLeft >>== bottomCen >>== midCen >>== bottomRight >>== midRight
      bottomRowWin = testDidWin $ f >>== bottomLeft >>== topCen >>== bottomCen >>== midRight >>== bottomRight
      rightWin = testDidWin $ f >>== bottomRight >>== topCen >>== midRight >>== bottomLeft >>== topRight
      leftWin = testDidWin $ f >>== bottomLeft >>== topCen >>== midLeft >>== bottomRight >>== topLeft
      cenWin = testDidWin $ f >>== bottomCen >>== topRight >>== midCen >>== bottomRight >>== topCen
      noWin = testDidWin $ f >>== bottomCen >>== topRight >>== midLeft >>== bottomRight >>== topCen
      diagLeft = testDidWin $ f >>== bottomLeft >>== topCen >>== midCen >>== bottomRight >>== topRight
      diagRight = testDidWin $ f >>== bottomRight >>== topRight >>== midCen >>== bottomRight >>== topLeft
  in (assertEqual "Top row win" True topRowWin)
     >> (assertEqual "Mid row win" True midRowWin)
     >> (assertEqual "Bottom row win" True bottomRowWin)
     >> (assertEqual "Right win" True rightWin)
     >> (assertEqual "left win" True leftWin)
     >> (assertEqual "cen win" True cenWin)
     >> (assertEqual "no win" False noWin)
     >> (assertEqual "diag right" True diagRight)
     >> (assertEqual "diag left" True diagLeft)

testUnfinished :: Assertion
testUnfinished =
  let f = NoMoves
      game = stringifyGame (f >>== midCen >>== topLeft >>== bottomCen >>== topCen >>== bottomRight)
      expectedString = ["OO ",
                        " X ",
                        " XX"]
  in (assertEqual "Game unfinished" expectedString game)

tests2 :: TestTree
tests2 = testGroup "TicTacToeTests"
  [
    testCase "undo game one move" testUndoOnlyOneMove,
    testCase "undo game" testUndo,
    testCase "game unfinished" testUnfinished,
    testCase "winning game" testWinning,
    testCase "Win conditions" testWinConditions
  ]

runner = defaultMain tests2
