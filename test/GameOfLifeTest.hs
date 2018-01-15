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



testStepBoard :: Assertion
testStepBoard =
   let dieUnderpop = [Cell False 0 0, Cell True 0 1]
       dieOverpop = [Cell True 0 0, Cell True 0 1, Cell True 1 1, Cell True 2 0, Cell True 1 0]
       growExactPop = [Cell True 0 0, Cell True 0 1, Cell True 1 1, Cell False 1 0]
       twoLives = [Cell True 0 0, Cell True 0 1, Cell False 1 1, Cell True 1 0]
       threeLives = [Cell True 0 0, Cell True 0 1, Cell True 1 1, Cell True 1 0]
   in (assertEqual "S1" (Cell False 0 1) (stepCell dieUnderpop (Cell True 0 1)))
      >> (assertEqual "S2" (Cell False 1 0) (stepCell dieOverpop (Cell True 1 0)))
      >> (assertEqual "S3" (Cell True 1 0) (stepCell growExactPop (Cell False 1 0)))
      >> (assertEqual "S4" (Cell True 1 0) (stepCell twoLives (Cell True 1 0)))
      >> (assertEqual "S5" (Cell True 1 0) (stepCell threeLives (Cell True 1 0)))

tests2 :: TestTree
tests2 = testGroup "GameOfLifeTests"
  [

    testCase "testNeighbors" $ testNeighbors
    , testCase "testStepBoard" $ testStepBoard
  ]

runner = defaultMain tests2
