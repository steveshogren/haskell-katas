module GameOfLifeTest where

import GameOfLife
import Test.Tasty
import Test.Tasty.HUnit

aBoard = [Cell Alive 0 0, Cell Alive 1 1]

testNeighbors :: Assertion
testNeighbors =
   (assertEqual "T1" 2 (liveNeighborCount aBoard $ Cell Alive 0 1))
   >> (assertEqual "T2" 2 (liveNeighborCount aBoard $ Cell Alive 1 0))
   >> (assertEqual "T3" 1 (liveNeighborCount aBoard $ Cell Alive 2 2))

tests2 :: TestTree
tests2 = testGroup "GameOfLifeTests"
  [
    testCase "" testNeighbors
  ]

runner = defaultMain tests2
