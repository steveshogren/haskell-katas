module Main where

import TicTacToe

main :: IO [()]
main = do
  let f = NoMoves
      partWay = (f >>== midCen >>== topLeft >>== bottomLeft)
  case partWay of
    Unfinished g ->
      let undone = Unfinished $ undoMove g
      in mapM print $ stringifyGame $ (undone >>== bottomCen >>== topCen >>== bottomRight >>== topRight)
    Finished g ->
      let undone = Unfinished $ undoMove g
      in mapM print $ stringifyGame $ (undone >>== bottomCen >>== topCen >>== bottomRight >>== topRight)
    NoMoves ->
      mapM putStrLn ["invalid move"]
