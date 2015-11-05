module BankOCR where

import Control.Monad
import Data.List.Split(chunksOf)
import Data.List(groupBy, sort, foldl)
import System.Random
import Control.Applicative
import Data.Either

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

type DigitRow = (Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter, Letter)

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

includeAlts :: LetterStrings -> UnparsedLetter
includeAlts (top, mid, bottom) =
  ((top, mid, bottom), (alternativesTop top, alternativesMid mid, alternativesBottom bottom))

tryTops :: UnparsedLetter -> [Letter]
tryTops ((t,m,b), ([], _, _)) = []
tryTops ((t,m,b), (ts, _, _)) = map Right $ rights $ [matchWith (head ts, m, b)] ++ (tryTops ((t,m,b), (tail ts, [], [])))

tryMids :: UnparsedLetter -> [Letter]
tryMids ((t,m,b), (_, [], _)) = []
tryMids ((t,m,b), (_, ms, _)) = map Right $ rights $ [matchWith (t, head ms, b)] ++ (tryMids ((t,m,b), ([], tail ms, [])))

tryBottoms :: UnparsedLetter -> [Letter]
tryBottoms ((t,m,b), (_, _, [])) = []
tryBottoms ((t,m,b), (_, _, bs)) = map Right $ rights $ [matchWith (t, m, head bs)] ++ (tryBottoms ((t,m,b), ([], [], tail bs)))

letterGood (Left _) = False
letterGood (Right _) = True

allLetterCombos :: [UnparsedLetter] -> [[Letter]]
allLetterCombos a = map (\b ->
                           let rawLetter = matchWith $ fst b
                           in if letterGood rawLetter then [rawLetter] else tryTops b ++ tryMids b ++ tryBottoms b) a

smartparse a =
  let altsToo = map includeAlts . makeDigitTable . breakIntoThrees $ a
      combinations = allLetterCombos altsToo
  in makeOutput . head . sequence $ combinations

doer = do
  x <- getFile "input.dt"
  return $ map smartparse x

tests = do
  output <- doer
  return $ ["711111111     ",
            "123456789     ",
            "123456789     ",
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
