{-# LANGUAGE OverloadedStrings #-}
module Mazes where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import  Control.Monad
import Data.Functor
import qualified System.Random as Rand

type Cell = (Integer,Integer)
type Board = Map.Map Cell Char

maxX :: Integer
maxX = 60
maxY :: Integer
maxY = 16
getX :: (a, b) -> a
getX = fst
getY :: (a, b) -> b
getY = snd

inc :: Integer -> Integer
inc = (+1)
dec :: Num a => a -> a
dec x = x - 1

pick :: [a] -> IO a
pick x = return.(x!!)=<<Rand.randomRIO(0,length x-1)

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
  in foldl (\m k -> Map.insert k ' ' m) m coords

toChar :: Maybe Char -> Char
toChar = Maybe.fromMaybe ' '

toBool :: Maybe Char -> Bool
toBool c =
  let char = Maybe.fromMaybe ' ' c
  in if char == ' ' then False else True

occupied :: Board -> Cell -> Bool
occupied b c = toBool . Map.lookup c $ b

toStringRow :: Board -> Integer -> String
toStringRow m y =
  let ks = Map.keys m
      topRow = filter ((==y).getY) ks
      topString = foldl (\ret k -> ret++[(toChar . Map.lookup k $ m)]) "" topRow
  in topString

toStringMap :: Board -> [String]
toStringMap m =
  map (\y -> toStringRow m y) [0..maxY]

printBoard :: Board -> IO [()]
printBoard b =
  sequence . (map putStrLn) $ toStringMap b

neighborCells :: Cell -> [Cell]
neighborCells cell =
  let right = if (getX cell == maxX) then [] else [deltaX cell inc]
      left = if (getX cell == 0) then [] else [deltaX cell dec]
      up = if (getY cell == 0) then [] else [deltaY cell dec]
      down = if (getY cell == maxY) then [] else [deltaY cell inc]
  in right ++ left ++ up ++ down

hasOnlyOneNeighbor :: Board -> Cell -> Bool
hasOnlyOneNeighbor b c =
  let alts = neighborCells c
      neighbors = filter (occupied b) alts
  in length neighbors < 2

goodNeighborCells :: Board -> Cell -> [Cell]
goodNeighborCells b c = filter (hasOnlyOneNeighbor b) $ neighborCells c

growStep :: (Board,Cell, Int) -> Char -> IO (Board, Cell, Int)
growStep (b, c, count) l =
  let alts = goodNeighborCells b c
  in if (length alts == 0) || (c == (19,6)) then return (b,c, count)
  else
    let next = pick alts
    in (\n -> (Map.insert n l b, n, count+1)) <$> next

growerBoard :: IO Board
growerBoard = do
  let empty = emptyBoard
      letters = zipWith (\_ c -> c) [0..200] $ concat ([['a'..'z'] | _ <- [0..]])
  (b,_,count) <- foldM growStep (empty, (1,1), 0) letters
  if count > 70 then return b else growerBoard

grower = do
  b <- growerBoard
  printBoard b

someBoards =
  sequence $ map (\x -> grower >> putStrLn "-----------------------------") [1..2]
