module DiceRoller where

import Control.Monad.Random
import qualified Data.Map as M

die :: RandomGen g => Rand g Int
die = getRandomR (1,6)

roll :: IO Int
roll = do
  roll1 <- evalRandIO $ die
  roll2 <- evalRandIO $ die
  if (roll2 == 6) then
    return 6
  else return roll1

roller :: Int -> IO [Int]
roller n = sequence (replicate n roll)

emptyMap :: M.Map Int Int
emptyMap = M.empty

main :: IO ()
main = do
  let rollCount = 10000
  rolls <- roller rollCount
  let counts = foldr (\next result ->
                          let current = M.findWithDefault 0 next result
                          in M.insert next (current+1) result
                        ) emptyMap rolls
  let expectedAverage = (fromIntegral rollCount) / 6
  let sixWeight = ((fromIntegral (M.findWithDefault 0 6 counts)) / expectedAverage) * 100
  if sixWeight > 105 || sixWeight < 98 then
    print "Its UNFAIR"
  else print "Seems legit"
  print counts
  print sixWeight
