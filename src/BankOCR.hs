module BankOCR where

import Control.Monad
import Data.List.Split(chunksOf)
import System.Random
import Control.Applicative

getFile :: FilePath -> IO [String]
getFile name = liftM (take 3 . lines) $ readFile name

breakIntoThrees :: [String] -> [[String]]
breakIntoThrees = map (chunksOf 3)

makeDigitTable [[], [], []] = []
makeDigitTable [a, b, c] =
  [[head a, head b, head c]] ++ makeDigitTable [tail a, tail b, tail c]

matchWith :: Num a => [[Char]] -> a
matchWith [" _ ",
           "| |",
           "|_|"] = 0
matchWith ["   ",
           "  |",
           "  |"] = 1
matchWith [" _ ",
           " _|",
           "|_ "] = 2
matchWith [" _ ",
           " _|",
           " _|"] = 3
matchWith ["   ",
           "|_|",
           "  |"] = 4
matchWith [" _ ",
           "|_ ",
           " _|"] = 5
matchWith [" _ ",
           "|_ ",
           "|_|"] = 6
matchWith [" _ ",
           "  |",
           "  |"] = 7
matchWith [" _ ",
           "|_|",
           "|_|"] = 8
matchWith [" _ ",
           "|_|",
           "  |"] = 9

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

fib2 =
  let fibi a b n = if (n > 0) then fibi b (a+b) (n-1) else a
  in fibi 0 1

checkSum :: Integral a => [a] -> Bool
checkSum [d9,d8,d7,d6,d5,d4,d3,d2,d1] = 
   let x = d1 + (2 * d2) + (3 * d3) + (4 * d4) + (5 * d5) + (6 * d6) + (7 * d7) + (8* d8) + (9 * d9)
   in mod x 11 == 0
checkSum _ = False

makeOutput x =
    let msg = if checkSum x then "    " else " ERR"
    in (foldr (++) " " $ map show x) ++ msg

doer = do
  x <- getFile "input.dt"
  return . makeOutput . map matchWith . makeDigitTable . breakIntoThrees $ x

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuffle rest
    where
    getIx = getStdRandom $ randomR (1, length lst)
    pickElem n = case splitAt n lst of
        ([], s) -> error $ "failed at index " ++ show n -- should never match
        (r, s)  -> (last r, init r ++ s)

shuffleThem = do
      chunksOf 2 <$> shuffle ["nils", "keith", "boguste", "patrick", "justin", "steve", "max", "becky", "dave"]
