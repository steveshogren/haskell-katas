module RomanNumeralTest where

import RomanNumeral
import Test.Tasty
import Test.Tasty.HUnit

testUnfinished :: Assertion
testUnfinished =
  let f = add 1 2
  in (assertEqual "Add 1 2" f 3)

tests2 :: TestTree
tests2 = testGroup "RomanNumeralTests"
  [
    testCase "simple addition" testUnfinished
  ]

runner = defaultMain tests2
