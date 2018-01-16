module GameOfLife where

import Control.Concurrent

-- import qualified Data.Map as M

data Cell = Cell Bool Int Int
  deriving(Show,Eq,Ord)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

liveNeighborCount :: [Cell] -> Cell -> Int
liveNeighborCount board (Cell _ x y) =
  length $ filter (\(Cell isLive bx by) ->
                     let xClose = (abs (bx-x)) < 2
                         yClose = (abs (by-y)) < 2
                         self = (bx== x) && (by== y)
                      in xClose && yClose && (not self) && isLive) board

stepCell :: [Cell] -> Cell -> Cell
stepCell board cell@(Cell s x y) =
   let liveCount = liveNeighborCount board cell
   in if liveCount < 2 || (liveCount > 3 && s)
   then Cell False x y
   else if liveCount == 3 && (not s)
        then Cell True x y
        else cell

stepBoard :: [Cell] -> [Cell]
stepBoard board =
  map (stepCell board) board

status :: Foldable t => t Cell -> [Int] -> Bool
status board [x,y] =
  foldl (\ret (Cell s cx cy) -> if (cx == x) && (cy==y) then s else ret) False board

printBoard :: Foldable t => t Cell -> Int -> IO ()
printBoard board size =
  let empty = [[x,y] | x <- [0..size], y <- [0..size]]
  in putStr $ "----\n" ++ concatMap (\cell@[x,y] ->
                  let live = status board cell
                      cellString = if live then "|X" else"| "
                  in if y == size then cellString ++ "\n" else cellString) empty

playGame :: [Cell] -> Int -> IO b
playGame board size = do
  printBoard board size >> threadDelay 1000000
  let newBoard = stepBoard board
  playGame newBoard size


bBoard :: [Cell]
bBoard = [Cell True 2 1, Cell True 1 1, Cell True 0 1]
