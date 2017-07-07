module RomanNumeral where

add :: Num a => a -> a -> a
add x y = x + y


toRoman :: Int -> String
toRoman 1 = "I"
toRoman 5 = "V"
toRoman 10 = "X"
toRoman 50 = "L"
toRoman 100 = "C"
toRoman 500 = "D"
toRoman arabic =
  if (arabic > 500) then
    (toRoman 500) ++ (toRoman (arabic - 500))
  else if (arabic >= 400) then
    (toRoman 100) ++ (toRoman 500) ++ (toRoman (arabic - 400))
  else if (arabic >= 100) then
    (toRoman 100) ++ (toRoman (arabic - 100))
  else if (arabic >= 90) then
    (toRoman 10) ++ (toRoman 100) ++ (toRoman (arabic - 90))
  else if (arabic > 50) then
    (toRoman 50) ++ (toRoman (arabic - 50))
  else if (arabic >= 40) then
    (toRoman 10) ++ (toRoman 50) ++ (toRoman (arabic - 40))
  else if (arabic > 10) then
    (toRoman 10) ++ (toRoman (arabic - 10))
  else if (arabic >= 9) then
    (toRoman 1) ++ (toRoman 10)
  else if (arabic > 5) then
    (toRoman 5) ++ (toRoman (arabic - 5))
  else if (arabic >= 4) then
    (toRoman 1) ++ (toRoman 5)
  else if (arabic > 1) then
    (toRoman 1) ++ (toRoman (arabic - 1))
  else "I"
