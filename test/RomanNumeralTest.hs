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
  (assertEqual "" "I" (toRoman 1))
  >> (assertEqual "" "V" (toRoman 5))
  >> (assertEqual "" "X" (toRoman 10))
  >> (assertEqual "" "II" (toRoman 2))
  >> (assertEqual "" "IV" (toRoman 4))
  >> (assertEqual "" "VI" (toRoman 6))
  >> (assertEqual "" "VIII" (toRoman 8))
  >> (assertEqual "" "IX" (toRoman 9))
  >> (assertEqual "" "XI" (toRoman 11))
  >> (assertEqual "" "XXXVIII" (toRoman 38))
  >> (assertEqual "" "XXXIX" (toRoman 39))
  >> (assertEqual "" "L" (toRoman 50))
  >> (assertEqual "" "XLIX" (toRoman 49))
  >> (assertEqual "" "LIX" (toRoman 59))
  >> (assertEqual "" "LXXXIX" (toRoman 89))

tests2 :: TestTree
tests2 = testGroup "RomanNumeralTests"
  [
    testCase "simple addition" testUnfinished,
    testCase "toRoman" testToRoman
  ]

runner = defaultMain tests2
