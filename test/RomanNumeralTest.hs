module RomanNumeralTest where

import RomanNumeral
import Test.Tasty
import Test.Tasty.HUnit

testUnfinished :: Assertion
testUnfinished =
  let f = add 1 2
  in (assertEqual "Add 1 2" f 3)

testToRoman :: Assertion
testToRoman =
  (assertEqual "" (toRoman 1) "I")
  >> (assertEqual "" (toRoman 5) "V")
  >> (assertEqual "" (toRoman 10) "X")

tests2 :: TestTree
tests2 = testGroup "RomanNumeralTests"
  [
    testCase "simple addition" testUnfinished,
    testCase "toRoman" testToRoman
  ]

runner = defaultMain tests2
