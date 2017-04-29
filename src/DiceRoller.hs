module DiceRoller where

import Control.Monad.Random
import qualified Data.Map as M
import Control.Lens
import Data.Map.Lens
import Control.Lens.Iso

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


doer :: M.Map Int Int -> M.Map Int Int
doer = at 0 . non 0 +~ 1

main :: IO ()
main = do
  let rollCount = 10000
  rolls <- roller rollCount
  let counts = foldr (\next result -> at next . non 0 +~ 1 $ result) emptyMap rolls
  let expectedAverage = (fromIntegral rollCount) / 6
  let sixWeight = ((fromIntegral (counts ^.at 6 . non 0)) / expectedAverage) * 100
  if sixWeight > 105 || sixWeight < 98 then
    print $ "Its UNFAIR" ++ " six showed up " ++ show sixWeight ++ "% of the time!"
  else print "Seems legit"
  print counts
  print sixWeight
