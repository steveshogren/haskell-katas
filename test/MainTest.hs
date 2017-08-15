module Main where

import qualified TicTacToeTest as TTTT
import Test.Tasty
import Test.Tasty.HUnit
import qualified HearthstoneTest as HT
import qualified RomanNumeralTest as RN
import qualified PokerHandsTest as PH

main :: IO ()
main = defaultMain (testGroup "new group" [TTTT.tests2, HT.tests, RN.tests2, PH.tests])
