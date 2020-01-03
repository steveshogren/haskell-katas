module Main where

import Test.Tasty
import Test.Tasty.HUnit
-- import qualified TicTacToeTest as TTTT
-- import qualified GameOfLifeTest as GOL
-- import qualified HearthstoneTest as HT
-- import qualified RomanNumeralTest as RN
import qualified PokerHandsTest as PH
import qualified PokerHoldEmTest as PHT

main :: IO ()
main = defaultMain (testGroup "new group" [
                     -- TTTT.tests2
                     --, GOL.tests2
                     --, HT.tests
                     --, RN.tests2
                 --    PH.tests2
                      PHT.tests2
                       ])
