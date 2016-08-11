{-# LANGUAGE OverloadedStrings #-}
module PoemSlicer where

import Data.List.Split
import Data.Maybe
import qualified System.IO as IO
import Data.Ord
import Data.List
import qualified Data.Map as M
import qualified System.Random.Shuffle as Rand

--lineSize = 1
totalLineSize = (2 +)

poemLines :: FilePath -> IO [String]
poemLines fileName = do
  source <- readFile fileName
  return $ lines source

makeDict n = do
  rvs <- Rand.shuffleM [2..n+1]
  let kvs = zip [2..n+1] rvs
      randed = foldl (\m (k,v) -> M.insert k v m) M.empty kvs
  return randed

updateLine
  :: (Num t, Num t2, Ord t) =>
     M.Map t t2 -> (t1, t, t) -> (t1, t2, t2)
updateLine d (str, current, next) =
  let c = fromMaybe 0 $ M.lookup (current-1) d
      n = fromMaybe 0 $ M.lookup (next-1) d
  in (str, c, n)

groupLine :: Int -> ([String], t, Int) -> [String]
groupLine lineSize (s, _, goto) =
  let totalSize = (totalLineSize lineSize)
  in s ++ ["   -> "  ++ (show (goto*totalSize)), " "]

printOneLine :: (Show a, Show a1) => (a, t, a1) -> IO ()
printOneLine (s, _, goto) =
  putStrLn ((show s) ++ " -> " ++ (show goto))

writePoem :: [String] -> IO ()
writePoem = IO.writeFile "/home/jack/programming/vimtutor/files/linenumbers/because_i_could_not_stop_for_death.txt" . unlines

groupSize :: String -> Int
groupSize headLine = read headLine

main :: IO ()
main = do
  lines <- poemLines "/home/jack/programming/vimtutor/poems/because_i_could_not_stop_for_death.txt"
  let poemLines = tail lines
      grpSize = groupSize . head $ lines
      output = zip3 (chunksOf grpSize poemLines) [2..] [3..]
      outMissingFirst = drop 1 output
      size = length outMissingFirst
  d <- makeDict size
  let o = map (updateLine d) outMissingFirst
      (s,c,n) = head output
      (_,secondLineNum,_) = head o
      firstLine = (s,1, secondLineNum)
      allLines = ([firstLine] ++ o)
      sorted = sortBy (comparing (\(_,k,_) -> k))  allLines
      vals = map (\(a,b,c) -> (b, c)) sorted
  writePoem $ concatMap (groupLine grpSize) sorted
