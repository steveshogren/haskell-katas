module TicTacToe where

import qualified Data.Map as M

type AsMap = M.Map Cell State

data State = One | Two | Empty
type Cell = (Integer,Integer)
data TwoMove = TwoMove (Cell, OneMove) | TNone
data OneMove = OneMove (Cell, TwoMove) | ONone
data Move = OMove OneMove
            | TMove TwoMove

makeMove :: Cell -> Move -> Move
makeMove c (OMove oneMove) =
  TMove $ TwoMove (c, oneMove)
makeMove c (TMove twoMove) =
  OMove $ OneMove (c, twoMove)

topRight m = makeMove (0,2) m
midRight m = makeMove (1,2) m
bottomRight m = makeMove (2,2) m
topCen m = makeMove (0,1) m
midCen m = makeMove (1,1) m
bottomCen m = makeMove (2,1) m
topLeft m = makeMove (0,0) m
midLeft m = makeMove (1,0) m
bottomLeft m = makeMove (2,0) m

emptyMap :: AsMap
emptyMap = M.fromList [((0,0),Empty), ((0,1),Empty), ((0,2),Empty), ((1,0),Empty), ((1,1),Empty), ((1,2),Empty), ((2,0),Empty), ((2,1),Empty), ((2,2),Empty)]

oneToMap :: AsMap -> OneMove -> AsMap
oneToMap  m (OneMove (cell, o)) = twoToMap (M.insert cell One m) o
oneToMap m ONone = m

twoToMap :: AsMap -> TwoMove -> AsMap
twoToMap m (TwoMove (cell, o)) = oneToMap (M.insert cell Two m) o
twoToMap m TNone = m

toMap :: Move -> AsMap
toMap (OMove o) = oneToMap M.empty o
toMap (TMove o) = twoToMap M.empty o

cellAsString (Just One) = "X"
cellAsString (Just Two) = "O"
cellAsString (Just Empty) = " "
cellAsString Nothing = " "

getCell c m = cellAsString $ M.lookup c m

toString :: AsMap -> [String]
toString m =
  let top = getCell (0,0) m ++ getCell (0,1) m ++ getCell (0,2) m
      mid = getCell (1,0) m ++ getCell (1,1) m ++ getCell (1,2) m
      bot = getCell (2,0) m ++ getCell (2,1) m ++ getCell (2,2) m
  in [top, mid, bot]

main = do
  let f = TMove $ TNone
  (mapM (print)) $ toString $ toMap $ topRight $ topLeft $ topCen $ f
