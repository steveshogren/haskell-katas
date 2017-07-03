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
  (assertEqual ""    "I" (toRoman 1)  )
  >> (assertEqual "" "V" (toRoman 5)  )
  >> (assertEqual "" "X" (toRoman 10) )
  >> (assertEqual "" "II"(toRoman 2)  )
  >> (assertEqual "" "IV"(toRoman 4)  )
  >> (assertEqual "" "VI"(toRoman 6)  )
  >> (assertEqual "" "VIII"(toRoman 8)  )

tests2 :: TestTree
tests2 = testGroup "RomanNumeralTests"
  [
    testCase "simple addition" testUnfinished,
    testCase "toRoman" testToRoman
  ]

runner = defaultMain tests2
