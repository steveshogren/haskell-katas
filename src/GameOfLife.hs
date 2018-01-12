module GameOfLife where

import qualified Data.Map as M

data Cell = Cell State Int Int
  deriving(Show,Eq,Ord)

data State = Alive | Dead
  deriving(Show,Eq,Ord)

liveNeighborCount board (Cell _ x y) = 1
