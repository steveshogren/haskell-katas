module BookCalendar where

import Data.Time

currentWordCount :: Integer
currentWordCount = 9048

wordsPerDay :: Integer
wordsPerDay = 333

isSat :: FormatTime t => t -> Bool
isSat d = formatTime defaultTimeLocale "%a" d == "Sat"

addToDay :: UTCTime -> Integer -> Day
addToDay today days =
  let d = addDays days . utctDay $ today
  in if isSat d then addToDay today (days+ 2)
     else d

printDay :: FormatTime t => t -> String
printDay d = formatTime defaultTimeLocale "   %a - %b %e %Y" d

buildDate :: Integer -> UTCTime -> Integer -> [Char]
buildDate goal today daysFuture =
  let dayNumber = addToDay today daysFuture
  in (show ((goal * daysFuture) + currentWordCount)) ++ printDay dayNumber

dailyCounts :: Integer -> UTCTime -> [[Char]]
dailyCounts goal today =
 fmap (buildDate goal today) [1..35]

main :: IO [()]
main = do
  today <- getCurrentTime
  sequence $ map (putStrLn . show) $ dailyCounts wordsPerDay today
