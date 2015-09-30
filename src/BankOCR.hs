module BankOCR where

import Control.Monad
import Data.List.Split(chunksOf)
import System.Random
import Control.Applicative
import Data.Either

-- data Number = Number String String String

getFile :: FilePath -> IO [[String]]
getFile name = liftM (chunksOf 4 . lines) $ readFile name

breakIntoThrees :: [String] -> [[String]]
breakIntoThrees = map (chunksOf 3)

makeDigitTable [[], [], [], []] = []
makeDigitTable [a, b, c, _] =
  [[head a, head b, head c]] ++ makeDigitTable [tail a, tail b, tail c, []]

matchWith :: Num a => [[Char]] -> Either String a
matchWith [" _ ",
           "| |",
           "|_|"] = Right 0
matchWith ["   ",
           "  |",
           "  |"] = Right 1
matchWith [" _ ",
           " _|",
           "|_ "] = Right 2
matchWith [" _ ",
           " _|",
           " _|"] = Right 3
matchWith ["   ",
           "|_|",
           "  |"] = Right 4
matchWith [" _ ",
           "|_ ",
           " _|"] = Right 5
matchWith [" _ ",
           "|_ ",
           "|_|"] = Right 6
matchWith [" _ ",
           "  |",
           "  |"] = Right 7
matchWith [" _ ",
           "|_|",
           "|_|"] = Right 8
matchWith [" _ ",
           "|_|",
           "  |"] = Right 9
matchWith _ = Left "?"

checkSum :: Integral a => [a] -> Bool
checkSum [d9,d8,d7,d6,d5,d4,d3,d2,d1] =
   let x = d1 + (2 * d2) + (3 * d3) + (4 * d4) + (5 * d5) + (6 * d6) + (7 * d7) + (8* d8) + (9 * d9)
   in mod x 11 == 0
checkSum _ = False

getMessage x = if (checkSum . rights) x then "    " else " ERR"

getCharacter :: Show a => Either String a -> String
getCharacter (Left a) = a
getCharacter (Right a) = show a

makeOutput :: (Show a, Integral a) => [Either String a] -> String
makeOutput x = (foldr (++) " " $ map getCharacter x) ++ (getMessage x)

parse = makeOutput . map matchWith . makeDigitTable . breakIntoThrees

doer = do
  x <- getFile "input.dt"
  return $ map parse x






-- Random kata stuff

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

fib2 =
  let fibi a b n = if (n > 0) then fibi b (a+b) (n-1) else a
  in fibi 0 1


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
