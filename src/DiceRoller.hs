module DiceRoller where

import Control.Monad.Random
import Control.Monad

die :: RandomGen g => Rand g Int
die = getRandomR (1,6)

roll = do
  roll1 <- evalRandIO $ die
  roll2 <- evalRandIO $ die
  if (roll2 > 4) then
    return 6
  else return roll1

roller :: Int -> IO [Int]
roller n = sequence (replicate n roll)

data RollCounts = RollCounts { one :: Int, two :: Int, three::Int, four::Int,five::Int,six::Int}
  deriving Show
newHash = RollCounts { one = 0, two = 0, three = 0, four = 0, five = 0, six = 0}

main = do
  rolls <- roller 100
  let counts = foldr (\next result ->
                        if (next == 1) then
                          result { one = (one result) + 1 }
                        else if (next == 2) then
                          result { two = (two result) + 1 }
                        else if (next == 3) then
                          result { three = (three result) + 1 }
                        else if (next == 4) then
                          result { four = (four result) + 1 }
                        else if (next == 5) then
                          result { five = (five result) + 1 }
                        else
                          result { six = (six result) + 1 }
                        ) newHash rolls
  let total = (one counts) + (two counts) + (three counts) + (four counts) + (five counts) + (six counts) 
  let expectedAverage = (fromIntegral total) / (fromIntegral 6)
  let sixWeight = ((fromIntegral (six counts)) / expectedAverage) * 100
  print counts
  print sixWeight
