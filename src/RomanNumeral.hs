module RomanNumeral where

tensThroughFours :: Int -> Int -> Int -> Int -> String
tensThroughFours tens fives ones arabic =
  -- tens case
  if (arabic >= tens) then
    (toRoman 0) ++ (toRoman tens) ++ (toRoman (arabic - tens))
  -- nines case
  else if (arabic >= (tens - ones)) then
    (toRoman ones) ++ (toRoman tens) ++ (toRoman (arabic - (tens - ones)))
  -- fives case
  else if (arabic >= fives) then
    (toRoman 0) ++ (toRoman fives) ++ (toRoman (arabic - fives))
  -- fours case
  else if (arabic >= (fives - ones)) then
    (toRoman ones) ++ (toRoman fives) ++ (toRoman (arabic - (fives - ones)))
  else ""

toRoman :: Int -> String
toRoman 1 = "I"
toRoman 5 = "V"
toRoman 10 = "X"
toRoman 50 = "L"
toRoman 100 = "C"
toRoman 500 = "D"
toRoman 1000 = "M"
toRoman arabic =
  if (arabic > 399) then
    (tensThroughFours 1000 500 100 arabic)
  else if (arabic > 39) then
    (tensThroughFours 100 50 10 arabic)
  else if (arabic > 3) then
    (tensThroughFours 10 5 1 arabic)
  else if (arabic < 4 && arabic >= 1) then
    (toRoman 1) ++ (toRoman (arabic - 1))
  else ""
