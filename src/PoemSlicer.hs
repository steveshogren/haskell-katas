{-# LANGUAGE OverloadedStrings #-}
module PoemSlicer where

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

randomWords :: FilePath -> IO [String]
randomWords fileName = do
  source <- readFile fileName
  return $ lines source

totalLineSize :: Int -> Int
totalLineSize = (2 +)

poemLines :: FilePath -> IO [String]
poemLines fileName = do
  source <- readFile fileName
  return $ lines source

makeDict :: (Enum a, Num a, Ord a, MonadRandom m) => a -> m (M.Map a a)
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

padLines :: Int -> [String] -> [String]
padLines lineSize lines =
  if length lines /= lineSize then
    lines ++ (map (\s -> " ") [1..(lineSize-length lines)])
  else lines

displayLinNum :: Int -> ([String], t, Int) -> [String]
displayLinNum lineSize (s, _, goto) =
  let totalSize = (totalLineSize lineSize)
      lines = padLines lineSize s
  in lines ++ ["   -> "  ++ (show (goto*totalSize)), " "]

displaySearch :: [String] -> Int -> ([String], Int, Int) -> [String]
displaySearch words lineSize (s, currentLine, nextLine) =
  let lines = padLines lineSize s
      nextString = (filter (/='"') (words !! nextLine))
      currentString = (filter (/='"') (words !! currentLine))
  in lines ++ [currentString ++ (show currentLine) ++  "   -> "  ++ (nextString  ++ (show nextLine)), " "]

printOneLine :: (Show a, Show a1) => (a, t, a1) -> IO ()
printOneLine (s, _, goto) =
  putStrLn ((show s) ++ " -> " ++ (show goto))

outDir :: String
outDir = "/home/jack/programming/vimtutor/files/"

writeLinNumPoem :: String -> [String] -> IO ()
writeLinNumPoem f = IO.writeFile (outDir ++ "linenumbers/" ++ f) . unlines

writeSearchPoem :: String -> [String] -> IO ()
writeSearchPoem f = IO.writeFile (outDir ++ "search/" ++ f) . unlines

groupSize :: String -> Int
groupSize headLine = read headLine

allFiles :: IO [FilePath]
allFiles = do
  fs <- getDirectoryContents $ "/home/jack/programming/vimtutor/poems/"
  return $ (filter (/="..")) . (filter (/=".")) $ fs

doAFile :: String -> IO ()
doAFile f = do
  words <- randomWords "words.txt"
  lines <- poemLines ("/home/jack/programming/vimtutor/poems/" ++ f)
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
  (writeLinNumPoem f $ concatMap (displayLinNum grpSize) sorted)
    >> (writeSearchPoem f $ concatMap (displaySearch words grpSize) sorted)

main :: IO [()]
main = do
  files <- allFiles
  mapM doAFile files

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

appendInsertFile :: IO ()
appendInsertFile = do
  nums <- mapM buildNumberSection [2..8]
  writeAppendInsertFile $ concatMap (\(num, section) -> "\n-------\nSearch For " ++ num ++ "\n\n" ++ printSection section) $ nums

