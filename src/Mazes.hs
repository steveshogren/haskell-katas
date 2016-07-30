{-# LANGUAGE OverloadedStrings #-}
module Mazes where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Cell = (Int,Int)
type Board = Map.Map Cell Bool

toChar x = if x then "a" else " "
toBool = Maybe.fromMaybe False

toStringMap :: Board -> String
toStringMap m l w =
  let ks = Map.keys m
      topRow = filter ((==0).snd) ks
      topString = foldl (\ret k -> ret++((toChar . toBool . Map.lookup m) k)) "" topRow
  in topString

generate = do
  putStrLn "abcdef                           "
  putStrLn "     g                           "
  putStrLn "     h                           "
  putStrLn "     ijklmnopqrstuvwxyzabcdefghij"
