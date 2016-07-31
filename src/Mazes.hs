{-# LANGUAGE OverloadedStrings #-}
module Mazes where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Cell = (Integer,Integer)
type Board = Map.Map Cell Bool

emptyBoard =
  let m = Map.empty
      coords = [(x,y) | x <- [0..20], y <- [0..7]]
  in foldl (\m k -> Map.insert k False m) m coords

sampleBoard =
  let m = emptyBoard
  in foldl (\m k -> Map.insert k True m) m [(0,0), (1,0), (2,0), (4,0), (1,2) ]

toChar :: Bool -> String
toChar x = if x then "a" else " "

toBool = Maybe.fromMaybe False

toStringRow :: Board -> Integer -> String
toStringRow m y =
  let ks = Map.keys m
      topRow = filter ((==y).snd) ks
      topString = foldl (\ret k -> ret++((toChar . toBool . Map.lookup k) m)) "" topRow
  in topString

toStringMap :: Board -> [String]
toStringMap m =
  map (\y -> toStringRow m y) [0..7]

printMap =
  sequence . (map putStrLn) $ toStringMap sampleBoard

generate = do
  putStrLn "abcdef                           "
  putStrLn "     g                           "
  putStrLn "     h                           "
  putStrLn "     ijklmnopqrstuvwxyzabcdefghij"
