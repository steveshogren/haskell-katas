module PokerHands where

import Data.List.Split(splitOn)
import Data.List(any, groupBy, sortBy, all, transpose, sort)
import Control.Monad(mapM)
import Data.Maybe(catMaybes)
import Debug.Trace (trace)

parseSuit :: Char -> Maybe Suit
parseSuit 'S' = Just Spades
parseSuit 'H' = Just Hearts
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit _ = Nothing

parseFace :: Char -> Maybe Face
parseFace '2' = Just Two
parseFace '3' = Just Three
parseFace '4' = Just Four
parseFace '5' = Just Five
parseFace '6' = Just Six
parseFace '7' = Just Seven
parseFace '8' = Just Eight
parseFace '9' = Just Nine
parseFace '1' = Just Ten
parseFace 'J' = Just Jack
parseFace 'Q' = Just Queen
parseFace 'K' = Just King
parseFace 'A' = Just Ace
parseFace _ = Nothing

data Suit = Hearts | Clubs | Diamonds | Spades
  deriving (Show, Eq, Ord, Enum)

data Face = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
  deriving (Show, Eq, Ord, Enum)

data AHand = RoyalFlush Suit
           | StraightFlush (Face, Suit)
           | FourOfAKind Face
           | FullHouse (Face,Face)
           | Flush Suit
           | Straight Face
           | ThreeOfAKind Face
           | TwoPair (Face, Face)
           | TwoOfAKind Face
           | HighCard Face
  deriving (Show, Eq, Ord)

data Card = Card Face Suit
  deriving (Show, Eq, Ord)

type Hand = [Card]

parseCard :: String -> Maybe Card
parseCard [first,second] = do
  suit <- parseSuit second
  face <- parseFace first
  return $ Card face suit
parseCard [first,_,third] = do
  suit <- parseSuit third
  face <- parseFace first
  return $ Card face suit
parseCard _ = Nothing

parseHand :: String -> Maybe Hand
parseHand str =
  let cardStrs = splitOn " " str
  in mapM parseCard cardStrs

sameFace :: Card -> Card -> Bool
sameFace (Card f1 _) (Card f2 _) = f1 == f2

face :: Card -> Face
face (Card f1 _) = f1

isOfAKind :: Bool -> Int -> Hand -> (Face -> AHand) -> Maybe AHand
isOfAKind rev size hand t =
  let faceGrouped = groupBy sameFace $ sortBy compare hand
      moreThanN = filter (\a -> length a == size) faceGrouped
      reverseFn = if rev then reverse else id
  in if length moreThanN > 0 then
      let sorted = reverseFn $ sortBy (compare) $ map (face . head) moreThanN
      in Just $ t (head sorted)
     else Nothing

isTwoOfAKind :: Hand -> Maybe AHand
isTwoOfAKind hand = isOfAKind False 2 hand TwoOfAKind

isThreeOfAKind :: Hand -> Maybe AHand
isThreeOfAKind hand = isOfAKind False 3 hand ThreeOfAKind

isFourOfAKind :: Hand -> Maybe AHand
isFourOfAKind hand = isOfAKind False 4 hand FourOfAKind

isFullHouse :: Hand -> Maybe AHand
isFullHouse hand = do
  TwoOfAKind twoKind <- isTwoOfAKind hand
  ThreeOfAKind threeKind <- isThreeOfAKind hand
  return $ FullHouse (threeKind, twoKind)

isTwoPair :: Hand -> Maybe AHand
isTwoPair hand = do
  TwoOfAKind twoKindA <- isOfAKind False 2 hand TwoOfAKind
  TwoOfAKind twoKindB <- isOfAKind True 2 hand TwoOfAKind
  if (twoKindA /= twoKindB) then
    return $ TwoPair (twoKindA, twoKindB)
  else Nothing

isStraight :: Hand -> Maybe AHand
isStraight hand =
  let sorted = sortBy compare $ map face hand
      expected = enumFromTo (head sorted) (head . reverse $ sorted)
  in if sorted == expected then
       Just $ Straight (head sorted)
     else Nothing

suit :: Card -> Suit
suit (Card _ s) = s

isHighCard :: Hand -> Maybe AHand
isHighCard hand =
  let card = head $ sortBy compare $ hand
  in Just $ HighCard $ face card

isFlush :: Hand -> Maybe AHand
isFlush hand =
  let firstSuit = suit $ head hand
  in if all (== firstSuit) $ map suit hand then
       Just $ Flush firstSuit
     else Nothing

isRoyalFlush :: Hand -> Maybe AHand
isRoyalFlush hand = do
  Flush suit <- isFlush hand
  Straight straightHigh <- isStraight hand
  if straightHigh == Ace then
    return $ RoyalFlush suit
  else Nothing

isFunctions :: [Hand -> Maybe AHand]
isFunctions = [isRoyalFlush,isFlush, isStraight,isTwoPair,isThreeOfAKind,isTwoOfAKind,isFullHouse,isFourOfAKind, isHighCard]

pristineDeck :: [Card]
pristineDeck = [Card rank suit | suit <- [Hearts .. Spades], rank <- [Ace .. Two] ]

permutations :: Int -> [a] -> [[a]]
permutations depth list =
  let decks = replicate depth list
  in sequence decks

dropN n [] = []
dropN n xs = let (ys,zs) = splitAt n xs
             in ys ++ (tail zs)

deckPermutations :: (Maybe Face) -> Hand -> [Hand]
deckPermutations (Just wild) hand =
  let wilds = filter (\c -> face c == wild) hand
      notwilds = filter (\c -> face c /= wild) hand
      allHands = [wild ++ notwilds | wild <- (permutations (length wilds) pristineDeck)]
  in concatMap (deckPermutations Nothing) allHands
deckPermutations Nothing hand =
  if length hand > 5 then
    let idsToDrop = [sort [i1, i2] | i1 <- [0..(length hand)-1],
                                     i2 <- [0..(length hand)-1]]
    in foldl (\ret [i1,i2] ->
                     if i1 == i2 then ret
                     else [(dropN i1 $ dropN i2 hand)]++ret) [] idsToDrop
  else
    [hand]

convertHand :: String -> Maybe Face -> Maybe AHand
convertHand str wild = do
  hand <- parseHand str
  detectHand hand wild

detectHand :: [Card] -> Maybe Face -> Maybe AHand
detectHand hand wild = do
  let hands = deckPermutations wild hand
  return $ head $ sortBy compare $ catMaybes [f hand | hand <- hands, f <- isFunctions]

winningHand :: AHand -> AHand -> Either AHand AHand
winningHand left right =
  if left < right then Left left else Right right
