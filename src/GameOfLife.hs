module GameOfLife where

import qualified Data.Map as M

data Cell = Cell Bool Int Int
  deriving(Show,Eq,Ord)

xor True False = True
xor False True = True
xor _ _ = False

liveNeighborCount board (Cell _ x y) =
  length $ filter (\(Cell isLive bx by) ->
                     let xClose = (abs (bx-x)) < 2
                         yClose = (abs (by-y)) < 2
                         self = (bx== x) && (by== y)
                      in xClose && yClose && (not self) && isLive) board

stepCell board (Cell s x y) =
   let liveCount = liveNeighborCount board (Cell s x y)
   in if liveCount < 2 || (liveCount > 3 && s)
   then Cell False x y
   else if liveCount == 3 && (not s)
        then Cell True x y
        else Cell s x y

stepBoard board =
  map stepCell board
