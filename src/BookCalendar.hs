module BookCalendar where

import Data.Time

currentWordCount :: Integer
currentWordCount = 9048

wordsPerDay :: Integer
wordsPerDay = 333

isWeekend :: FormatTime t => t -> Bool
isWeekend d =
  let da = formatTime defaultTimeLocale "%a" d
  in da == "Sat" || da == "Sun"

addToDay :: UTCTime -> Integer -> Day
addToDay today days = addDays days . utctDay $ today

printDay :: FormatTime t => t -> String
printDay d = formatTime defaultTimeLocale "   %a - %b %e %Y" d

buildDate :: Integer -> UTCTime -> Integer -> [Char]
buildDate goal today daysFuture =
  let dayNumber = addToDay today daysFuture
  in (show ((goal * daysFuture) + currentWordCount)) ++ printDay dayNumber

dailyCounts :: Integer -> UTCTime -> [[Char]]
dailyCounts goal today =
 fmap (buildDate goal today) $ filter (\n -> not $ isWeekend $ addToDay today n ) [1..35]

main :: IO [()]
main = do
  today <- getCurrentTime
  sequence $ map (putStrLn . show) $ dailyCounts wordsPerDay today
