module TicTacToe where

import qualified Data.Map as M
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

type AsMap = M.Map Cell State

data State = Exx | Oh | Empty
  deriving (Eq, Show, Read)
type Cell = (Integer,Integer)
data O = O (Cell, X) | ONone
  deriving (Eq, Show, Read)
data X = X (Cell, O) | XNone
  deriving (Eq, Show, Read)
data Move = XMove X
            | OMove O
  deriving (Eq, Show, Read)
data Game = Finished Move
            | Unfinished Move
            | NoMoves
  deriving (Eq, Show, Read)

areSameAndSet :: Cell -> Cell -> Cell -> AsMap -> Bool
areSameAndSet c1 c2 c3 m =
  let o = getCell c1 m
      t = getCell c2 m
      th = getCell c3  m
  in o /= " " && (o == t) && (o == th)

diagsSameRight :: AsMap -> Bool
diagsSameRight m = areSameAndSet (0,2) (1,1) (2,0) m

diagsSameLeft :: AsMap -> Bool
diagsSameLeft m = areSameAndSet (0,0) (1,1) (2,2) m

vertSame :: Integer -> AsMap -> Bool
vertSame y m = areSameAndSet (0, y) (1, y) (2, y) m

horizontalsSame :: Integer -> AsMap -> Bool
horizontalsSame x m = areSameAndSet (x, 0) (x, 1) (x, 2) m

didWin :: Move -> Bool
didWin move =
  let m = toMap move
  in horizontalsSame 0 m ||
     horizontalsSame 1 m ||
     horizontalsSame 2 m ||
     vertSame 0 m ||
     vertSame 1 m ||
     vertSame 2 m ||
     diagsSameLeft m ||
     diagsSameRight m

makeGame :: Bool -> Move -> Game
makeGame True move = Finished move
makeGame False move = Unfinished move

makeMove :: Cell -> Move -> Game
makeMove c (XMove oneMove) =
  let m = OMove $ O (c, oneMove)
  in makeGame (didWin m) m
makeMove c (OMove twoMove) =
  let m = XMove $ X (c, twoMove)
  in makeGame (didWin m) m

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

xToMap :: AsMap -> X -> AsMap
xToMap  m (X (cell, o)) = oToMap (M.insert cell Exx m) o
xToMap m XNone = m

oToMap :: AsMap -> O -> AsMap
oToMap m (O (cell, o)) = xToMap (M.insert cell Oh m) o
oToMap m ONone = m

toMap :: Move -> AsMap
toMap (XMove o) = xToMap M.empty o
toMap (OMove o) = oToMap M.empty o

cellAsString :: Maybe State -> [Char]
cellAsString (Just Exx) = "X"
cellAsString (Just Oh) = "O"
cellAsString (Just Empty) = " "
cellAsString Nothing = " "

getCell :: Ord k => k -> M.Map k State -> [Char]
getCell c m = cellAsString $ M.lookup c m

toString :: AsMap -> [String]
toString m =
  let top = getCell (0,0) m ++ getCell (0,1) m ++ getCell (0,2) m
      mid = getCell (1,0) m ++ getCell (1,1) m ++ getCell (1,2) m
      bot = getCell (2,0) m ++ getCell (2,1) m ++ getCell (2,2) m
  in [top, mid, bot]

undoMove :: Move -> Move
undoMove (XMove XNone) = OMove ONone
undoMove (OMove ONone) = XMove XNone
undoMove (XMove (X (_, o))) = OMove o
undoMove (OMove (O (_, x))) = XMove x

(>>==) :: Game -> (Move -> Game) -> Game
(Unfinished g) >>== m = m g
NoMoves >>== m = m $ OMove ONone
a >>== _ = a

playerName :: Move -> [Char]
playerName (XMove _) = "X"
playerName (OMove _) = "O"

playerWonMessage :: Move -> [Char]
playerWonMessage m =
  "Player: " ++ (playerName m) ++ " Won!"

printGame :: Game -> IO [()]
printGame (Finished m) = do
  putStrLn (playerWonMessage m)
  (mapM (print)) $ toString $ toMap $ m
printGame (Unfinished m) = (mapM (print)) $ toString $ toMap $ m
printGame NoMoves = (mapM (print)) $ toString $  emptyMap

main :: IO [()]
main = do
  let f = NoMoves
      partWay = (f >>== midCen >>== topLeft >>== bottomLeft)
  case partWay of
    Unfinished g ->
      let undone = Unfinished $ undoMove g
      in printGame $ (undone >>== bottomCen >>== topCen >>== bottomRight >>== topRight)
    Finished g ->
      let undone = Unfinished $ undoMove g
      in printGame $ (undone >>== bottomCen >>== topCen >>== bottomRight >>== topRight)
    NoMoves ->
      mapM putStrLn ["invalid move"]


test1 =
  let f = NoMoves
      partWay = (f >>== midCen >>== topRight)
  in case partWay of
      Unfinished g ->
        let undone = undoMove g
            expectedUndo = XMove (X ((1,1),ONone))
            expectedBefore = OMove (O ((0,2),X ((1,1),ONone)))
        in (assertEqual "Undoing the move" expectedUndo undone >>
            assertEqual "Before undo" expectedBefore g)
      _ -> assertFailure "Got wrong Game type"


tests :: TestTree
tests = testGroup "TicTacToeTests"
  [
    testCase "winning game" test1
  ]

runTests = defaultMain tests
