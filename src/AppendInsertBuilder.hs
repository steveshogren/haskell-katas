{-# LANGUAGE OverloadedStrings #-}
module AppendInsertBuilder where

import Data.List.Split
import System.Directory
import Data.Maybe
import qualified System.IO as IO
import Data.Ord
import Data.List
import qualified Data.Map as M
import qualified System.Random.Shuffle as Rand
import Control.Monad.Random
import Control.Monad

outDir :: String
outDir = "/home/jack/programming/vimtutor/files/"

flipCoin :: IO Int
flipCoin =  getStdRandom (randomR (1,2))

shouldKeep :: IO Bool
shouldKeep = do
  roll <- flipCoin
  return $ if (roll == 1) then True else False

filterOutAOrB :: Int -> Int -> (Int -> IO Bool)
filterOutAOrB a b n = do
   keep <- shouldKeep
   return ((n /= a && n /= b) || (keep && ((n == b ) || (n == a))))

buildRow :: Int -> Int -> IO [Int]
buildRow a b = filterM (filterOutAOrB a b) [1..9]

buildNumberSection :: Int -> IO (String, [[Int]])
buildNumberSection num = (liftM (\row -> (show num, row))) $ mapM (\_ -> buildRow (num-1) (num+1)) [1..9]

printSection :: [[Int]] -> String
printSection numLists =
  concatMap (\l -> (concatMap show l) ++ "\n") numLists

writeAppendInsertFile :: String -> IO ()
writeAppendInsertFile = IO.writeFile (outDir ++ "append-insert-file.txt")

main :: IO ()
main = do
  nums <- mapM buildNumberSection [2..8]
  writeAppendInsertFile $ concatMap (\(num, section) -> "\n-------\nSearch For " ++ num ++ "\n\n" ++ printSection section) $ nums
