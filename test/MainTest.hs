module Main where

import qualified TicTacToeTest as TTTT
import Test.Tasty
import Test.Tasty.HUnit
import qualified HearthstoneTest as HT

main :: IO ()
main = defaultMain (testGroup "new group" [TTTT.tests2, HT.tests])
