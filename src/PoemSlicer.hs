{-# LANGUAGE OverloadedStrings #-}
module PoemSlicer where

import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Map as M
import qualified System.Random.Shuffle as Rand


lineSize = 2

grouped = chunksOf lineSize

poemLines fileName = do
  source <- readFile fileName
  return $ lines source

makeDict n = do
  rvs <- Rand.shuffleM [2..n+2]
  let kvs = zip [2..n+2] rvs
      randed = foldl (\m (k,v) -> M.insert k v m) M.empty kvs
  return randed

updateLine d (str, current, next) =
  let c = fromMaybe 0 $ M.lookup current d
      n = fromMaybe 0 $ M.lookup next d
  in (str, c, n)

main = do
  lines <- poemLines "poem.txt"
  let output = zip3 (grouped lines) [2..] [3..]
      outMissingFirst = drop 1 output
      size = length outMissingFirst
  d <- makeDict size
  let o = map (updateLine d) outMissingFirst
      (s,c,n) = head output
      (_,secondLineNum,_) = head o
      firstLine = (s,1, secondLineNum)
      sorted = sortBy (comparing (\(_,k,_) -> k))  ([firstLine] ++ o)
  mapM (putStrLn . show) sorted
