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
                         isntSelf = not ((bx/= x) && (by/= y))
                      in xClose && yClose && isntSelf && isLive) board
