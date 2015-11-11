{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module BankOCR where

import Control.Monad
import Data.List.Split(chunksOf)
import Data.List(groupBy, sort, foldl)
import System.Random
import Control.Applicative
import Data.Either
import Control.Lens.Tuple
import Data.List.Lens
import Control.Lens

getFile :: FilePath -> IO [[String]]
getFile name = liftM (chunksOf 4 . lines) $ readFile name

type LetterAlts = ([String], [String], [String])
type LetterStrings = (String, String, String)
type UnparsedLetter = (LetterStrings, LetterAlts)
type Letter = Either (String, LetterStrings) Int

breakIntoThrees :: [String] -> [[String]]
breakIntoThrees = map (chunksOf 3)

makeDigitTable :: [[String]] -> [LetterStrings]
makeDigitTable [[], [], [], []] = []
makeDigitTable [a, b, c, _] =
  [(head a, head b, head c)] ++ makeDigitTable [tail a, tail b, tail c, []]

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
alternativesMid "   " = ["  |"]
alternativesMid " _ " = [" _|"]
alternativesMid a = fail a

alternativesBottom "|_|" = [" _|",  "|_ "]
alternativesBottom "  |" = [" _|"]
alternativesBottom " _|" = ["|_|", "  |"]
alternativesBottom "|_ " = ["|_|"]

matchWith :: (String,String,String) -> Letter
matchWith (" _ ",
           "| |",
           "|_|") = Right 0
matchWith ("   ",
           "  |",
           "  |") = Right 1
matchWith (" _ ",
           " _|",
           "|_ ") = Right 2
matchWith (" _ ",
           " _|",
           " _|") = Right 3
matchWith ("   ",
           "|_|",
           "  |") = Right 4
matchWith (" _ ",
           "|_ ",
           " _|") = Right 5
matchWith (" _ ",
           "|_ ",
           "|_|") = Right 6
matchWith (" _ ",
           "  |",
           "  |") = Right 7
matchWith (" _ ",
           "|_|",
           "|_|") = Right 8
matchWith (" _ ",
           "|_|",
           "  |") = Right 9
matchWith a = Left ("?", a)

data CheckSumResult = Valid | Invalid | Missing
  deriving (Show, Eq)

type DigitRow = (Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter)

checkSum :: [Int] -> CheckSumResult

checkSum a =
   if 9 == (length a) then
     let x = foldl (+) 0 $ zipWith (*) a (reverse [1..9])
     in if mod x 11 == 0 then Valid else Invalid
   else Missing

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

includeAlts :: LetterStrings -> UnparsedLetter
includeAlts (top, mid, bottom) =
  ((top, mid, bottom), (alternativesTop top, alternativesMid mid, alternativesBottom bottom))

type AGetter = Lens (String, String, String) (String, String, String) String String

tryGen :: AGetter -> (String, String, String) -> [String] -> [Letter]
tryGen _ _ [] = []
tryGen getter a ts = [matchWith $ set getter (head ts) a] ++ (tryGen getter a $ tail ts)

tryTops _ [] = []
tryTops a ts = tryGen _1 a ts

tryMids _ [] = []
tryMids a ms = tryGen _2 a ms

tryBottoms _ [] = []
tryBottoms a bs = tryGen _3 a bs

tryAll (l, (ts, ms, bs)) = map Right $ rights $ tryTops l ts ++ tryMids l ms ++ tryBottoms l bs

letterGood (Left _) = False
letterGood (Right l) = True

allLetterCombos :: [UnparsedLetter] -> [[Letter]]
allLetterCombos a = map (\b ->
                           let rawLetter = matchWith $ fst b
                           in [rawLetter] ++ tryAll b) a

findChecksumCombos :: [Letter] -> (CheckSumResult, [Letter])
findChecksumCombos numbers = (checkSum . rights $ numbers, numbers)

tryCombosFirst :: [[a]] -> [[a]]
tryCombosFirst all =
  let base = map head all
  in concat $ zipWith (\idx letters -> map (\each -> base & ix idx .~ each) letters) [0.. ] all

filterGood (Valid, nums) = Right nums
filterGood (Invalid, n) = Left n
filterGood (Missing, n) = Left n

onlyGood = rights . (map filterGood)

removeEmpty :: [[a]] -> [[a]]
removeEmpty = filter (not.null)

smartparse a =
  let altsToo = map includeAlts . makeDigitTable . breakIntoThrees $ a
      combinations = allLetterCombos altsToo
      goodCombos = onlyGood $ map findChecksumCombos $ tryCombosFirst $ combinations
  in (map makeOutput) $ goodCombos -- makeOutput . head . goodCombos

doer = do
  x <- getFile "input.dt"
  return $ (map head) . removeEmpty $ map smartparse x

tests = do
  output <- doer
  return $ ["711111111     ",
            "123456789     ",
            "123456789     ",
            "123956781     ",
            "000000051     "] == output
    && checkSum [7,1,1,1,1,1,1,1,1] == Valid
    && checkSum [1,1,1,1,1,1,1,1,1] == Invalid


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

diceRoller :: IO Int
diceRoller =
  getStdRandom $ randomR (1, 6)

rollDice :: IO Int
rollDice = getStdRandom $ randomR (1, 6)

badDiceRoller :: IO Int
badDiceRoller = do
  firstRoll <- rollDice
  if firstRoll == 6
  then return 6
  else rollDice

rolls = do
  rs <- sequence $ map (\x -> badDiceRoller) [1..10000]
  let groups =  map (\x -> (head x,length x)) $ groupBy (==) $ sort rs
      counts = map snd groups
      total = foldl (+) 0 counts
      expectedMaxVar = 20
      variance = map (\x -> (expectedMaxVar >) $ (100 *) $ (fromIntegral x) / (fromIntegral total)) counts
      allInVariance = foldl (&&) True variance
    in return (allInVariance,counts)
