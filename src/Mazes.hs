{-# LANGUAGE OverloadedStrings #-}
module Mazes where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import  Control.Monad
import Data.Functor
import qualified System.Random as Rand

type Cell = (Integer,Integer)
type Board = Map.Map Cell Bool

maxX :: Integer
maxX = 20
maxY :: Integer
maxY = 7
getX :: (a, b) -> a
getX = fst
getY :: (a, b) -> b
getY = snd

inc :: Integer -> Integer
inc = (+1)
dec :: Num a => a -> a
dec x = x - 1

deltaX :: (b, b1) -> (b -> t) -> (t, b1)
deltaX cell d =
  let newX = d . getX $ cell
  in (newX, getY cell)
deltaY :: (a, b) -> (b -> t) -> (a, t)
deltaY cell d =
  let newY = d . getY $ cell
  in (getX cell, newY)

emptyBoard :: Board
emptyBoard =
  let m = Map.empty
      coords = [(x,y) | x <- [0..maxX], y <- [0..maxY]]
  in foldl (\m k -> Map.insert k False m) m coords

toChar :: Bool -> String
toChar x = if x then "a" else " "

toBool :: Maybe Bool -> Bool
toBool = Maybe.fromMaybe False

toStringRow :: Board -> Integer -> String
toStringRow m y =
  let ks = Map.keys m
      topRow = filter ((==y).getY) ks
      topString = foldl (\ret k -> ret++((toChar . toBool . Map.lookup k) m)) "" topRow
  in topString

toStringMap :: Board -> [String]
toStringMap m =
  map (\y -> toStringRow m y) [0..7]

printBoard board =
  sequence . (map putStrLn) $ toStringMap board

nextAlternatives :: Cell -> [Cell]
nextAlternatives cell =
  let right = if (getX cell == maxX) then [] else [deltaX cell inc]
      left = if (getX cell == 0) then [] else [deltaX cell dec]
      up = if (getY cell == 0) then [] else [deltaY cell dec]
      down = if (getY cell == maxY) then [] else [deltaY cell inc]
  in right ++ left ++ up ++ down

growStep (board, currentCell) _ =
  let alts = nextAlternatives currentCell
      random = Rand.randomRIO (0, length alts - 1)
      next =  ((!!) alts) <$> random
  in (\n -> (Map.insert n True board, n)) <$> next

grower =
  let empty = emptyBoard
      m = foldM growStep (empty, (0,0)) [0..10]
  in m >>= (\(board, _) -> printBoard board)


generate :: IO ()
generate = do
  putStrLn "abcdef                           "
  putStrLn "     g                           "
  putStrLn "     h                           "
  putStrLn "     ijklmnopqrstuvwxyzabcdefghij"
