module RomanNumeral where

add x y = x + y


toRoman 1 = "I"
toRoman 5 = "V"
toRoman 10 = "X"
toRoman arabic =
  if (arabic > 5) then
    (toRoman 5) ++ (toRoman (arabic - 5))
  else if (arabic > 3) then
    (toRoman 1) ++ (toRoman 5)
  else if (arabic > 1) then
    (toRoman 1) ++ (toRoman (arabic - 1))
  else "I"
