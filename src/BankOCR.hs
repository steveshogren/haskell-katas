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

type Letter = Either (String, [String]) Int

tops = [" _ ", "   "]
middles = ["| |","|_|", " _|",  "|_ ", "  |"]
bottoms = ["|_|","  |", " _|",  "|_ "]

alternativesTop " _ " = ["   "]
alternativesTop "   " = [" _ "]

alternativesMid "| |" = ["|_|", "  |"]
alternativesMid "|_|" = ["| |", " _|",  "|_ "]
alternativesMid " _|" = ["|_|", "  |"]
alternativesMid "|_ " = ["|_|"]
alternativesMid "  |" = ["| |", " _|"]

alternativesBottom "|_|" = [" _|",  "|_ "]
alternativesBottom "  |" = [" _|"]
alternativesBottom " _|" = ["|_|", "  |"]
alternativesBottom "|_ " = ["|_|"]

matchWith :: [String] -> Letter
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
matchWith a = Left ("?", a)

data CheckSumResult = Valid | Invalid | Missing

type UnparsedLetter = ([String], [[String]])

checkSum :: [Int] -> CheckSumResult
checkSum [d9,d8,d7,d6,d5,d4,d3,d2,d1] =
   let x = d1 + (2 * d2) + (3 * d3) + (4 * d4) + (5 * d5) + (6 * d6) + (7 * d7) + (8* d8) + (9 * d9)
   in if mod x 11 == 0 then Valid else Invalid
checkSum _ = Missing

printCheckSum :: CheckSumResult -> String
printCheckSum Valid = "    "
printCheckSum Invalid = " ERR"
printCheckSum Missing = " ILL"

getMessage :: [Letter] -> String
getMessage = printCheckSum . checkSum . rights

getCharacter :: Letter -> String
getCharacter (Left (str, real)) = str
getCharacter (Right a) = show a

makeOutput :: [Letter] -> String
makeOutput x = (foldr (++) " " $ map getCharacter x) ++ (getMessage x)

parse = makeOutput . map matchWith . makeDigitTable . breakIntoThrees

testChecksum = checkSum . rights . map matchWith

includeAlts :: [String] -> UnparsedLetter
includeAlts [top, mid, bottom] =
  ([top, mid, bottom], [alternativesTop top, alternativesMid mid, alternativesBottom bottom])

smartparse a =
  let unparsed = includeAlts . makeDigitTable . breakIntoThrees a
  in 

doer = do
  x <- getFile "input.dt"
  return $ map parse x

tests = do
  output <- doer
  return $ ["711111111     ",
            "123456789     ",
            "1?3?56789  ILL",
            "123456781  ERR",
            "000000051     "] == output




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
