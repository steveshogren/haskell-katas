module GameOfLifeTest where

import GameOfLife
import Test.Tasty
import Test.Tasty.HUnit

aBoard = [Cell True 0 0, Cell True 1 1, Cell False 2 1, Cell False 1 2, Cell False 2 3, Cell False 3 2]

testNeighbors :: Assertion
testNeighbors =
   (assertEqual "T1" 2 (liveNeighborCount aBoard $ Cell True 0 1))
   >> (assertEqual "T2" 2 (liveNeighborCount aBoard $ Cell True 1 0))
   >> (assertEqual "T3" 1 (liveNeighborCount aBoard $ Cell True 2 2))

tests2 :: TestTree
tests2 = testGroup "GameOfLifeTests"
  [
    testCase "" testNeighbors
  ]

runner = defaultMain tests2
