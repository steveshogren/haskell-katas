{-# LANGUAGE TemplateHaskell #-}
module Optimization (main) where

import Control.Lens
import Prelude hiding ((*), (/), (+), (-))
import qualified Data.Map as Map
import Data.LinearProgram as DLP
import Data.List
import qualified Data.Set as Set

upgradeTypes :: (Eq a, Num t, Num a) => a -> (t, [Char])
upgradeTypes 1 = (1, "a")
upgradeTypes 2 = (2, "a")
upgradeTypes 3 = (3, "a")
upgradeTypes 4 = (1, "b")
upgradeTypes 5 = (2, "b")
upgradeTypes _ = (3, "b")

flattenPermutations :: [(Integer, String)] -> (Integer, Integer)
flattenPermutations = foldl (\(ac, bc) (cost, t) -> if (t == "a") then (ac+cost, bc) else (ac,bc+cost)) (0, 0)

-- addName :: (Show a, Show a1) => (a, a1) -> (a, a1, [Char])
-- addName (ac, bc) = (ac, bc, "-" ++ (show ac) ++ "-" ++ (show bc) )

twoCardUpgrades :: [(Integer, Integer)]
twoCardUpgrades =
  let types = Set.toList $ Set.fromList $
                map ((map upgradeTypes) . sort) [ [x,y,z]
                          | x <- [1..6],
                            y <- [1..6],
                            z <- [1..6]]
  in map (flattenPermutations) types

data Card = Card
    { _cost :: Integer
    , _power :: Integer
    , _speed :: Integer
    , _crit :: Integer
    , _pen :: Integer
    , _lifesteal :: Integer
    , _crit_bonus :: Integer
    , _name :: String
    , _letter :: String
    , _firstType :: String
    , _secondType :: String
    }
makeLenses ''Card

toCard :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, String) -> String -> Card
toCard (cost, power, speed, crit, pen, lifesteal, crit_bonus, name) letter =
  Card { _cost = cost
       , _power = power
       , _speed = speed
       , _crit = crit
       , _pen = pen
       , _lifesteal = lifesteal
       , _crit_bonus = crit_bonus
       , _name = name
       , _letter = letter
       , _firstType = ""
       , _secondType = ""
       }

mainCards :: [Card]
mainCards =
  zipWith toCard
    [(2, 2, 1, 0, 0, 0, 0, "madstone gem")
    ,(2, 0, 2, 1, 0, 0, 0, "redeye nitro")
    ,(3, 2, 0, 2, 0, 0, 0, "impact hammer")
    ,(3, 3, 1, 0, 0, 0, 0, "windcarver blade")
    ,(3, 0, 0, 1, 0, 3, 0, "brand ironeater")
    ,(3, 3, 0, 0, 1, 0, 0, "rustbreaker")
    ,(3, 1, 0, 3, 0, 0, 0, "spear rifthunter")
    ,(3, 1, 3, 0, 0, 0, 0, "whirling wand")
    ,(3, 3, 0, 1, 0, 0, 0, "micro-nuke")
    ,(6, 1, 0, 0, 0, 0, 1, "blade of agora")
    ,(6, 0, 0, 0, 0, 1, 1, "hunger maul")
    ,(3, 2, 0, 0, 0, 0, 0, "sages ward")
    ,(6, 0, 1, 0, 0, 0, 1, "blast harness")
    ]
    (map (\a -> [a]) ['a'..])

showOne :: (Card -> t) -> [Card] -> [(t, String)]
showOne fn permutations =
  map (\next -> (fn next, (_name next))) permutations

showAll :: (Card -> t) -> [(t, String)]
showAll fn =
  foldl (\ret card -> ret ++ (showOne fn (twoTypeCardPermutations card))) [] mainCards

updateFields lens add lens2 add2 n1 n2 =
  (lens +~ add) . (lens2 +~ add2) . (firstType .~ n1) . (secondType .~ n2)

cardFields :: Bool -> Bool -> Bool -> Bool -> Bool -> Card -> Integer -> Integer -> Card
cardFields True True False False False card a b = updateFields power a speed b "pw" "sp" card
cardFields True False True False False card a b = updateFields power a crit b "pw" "cr" card
cardFields True False False True False card a b = updateFields power a pen b "pw" "pn" card
cardFields True False False False True card a b = updateFields power a lifesteal b "pw" "ls" card
cardFields False True True False False card a b = updateFields speed a crit b "sp" "cr" card
cardFields False True False True False card a b = updateFields speed a pen b "sp" "pn" card
cardFields False True False False True card a b = updateFields speed a lifesteal b "sp" "ls" card
cardFields False False True True False card a b = updateFields crit a pen b "cr" "pn" card
cardFields False False True False True card a b = updateFields crit a lifesteal b "cr" "ls" card
cardFields False False False True True card a b = updateFields pen a lifesteal b "pn" "ls" card
cardFields False False False False True card a b = updateFields lifesteal a lifesteal b "ls" "ls" card
cardFields True False False False False card a b = updateFields power a power b "pw" "pw" card
cardFields False True False False False card a b = updateFields speed a speed b "sp" "sp" card

showCosts ac bc card =
  (card^.firstType) ++ ":" ++ (show ac) ++ "," ++ (card^.secondType) ++ ":" ++ (show bc) ++ "-" 

twoTypeCardPermutations :: Card -> [Card]
twoTypeCardPermutations card =
  let hasPower = _power card > 0
      hasSpeed = _speed card > 0
      hasCrit = _crit card > 0
      hasPen = _pen card > 0
      hasLS = _lifesteal card > 0
  in
    concatMap (\c -> map (\(ac, bc) ->
                            let nc = cardFields hasPower hasSpeed hasCrit hasPen hasLS card ac bc
                                newCost = (_cost nc) + ac + bc
                            in nc { _name =  "s" ++ (show c) ++ "-" ++ (_name nc) ++ (showCosts ac bc nc) ++ "-" ++ (show newCost),
                                    _cost = newCost}) twoCardUpgrades) [1..2]



-- desired numbers (15,12,13,8.0,11,1)
-- "blast harness(sp:0,sp:5)--11"
-- "brand ironeater(cr:2,ls:6)--11"
-- "rustbreaker(pw:6,pn:1)--10"
-- "brand ironeater(cr:9,ls:0)--12"
-- "rustbreaker(pw:3,pn:5)--11"


lpCards :: LP String Integer
lpCards = execLPM $ do
  leqTo (linCombination (showAll _cost)) 61
  geqTo (linCombination (showAll _power)) 15
  geqTo (linCombination (showAll _speed)) 12
  geqTo (linCombination (showAll _crit)) 13
  geqTo (linCombination (showAll _pen)) 8
  geqTo (linCombination (showAll _lifesteal)) 11
  geqTo (linCombination (showAll _crit_bonus)) 1
  leqTo (linCombination (map (\(_,n) -> (1, n)) $ showAll _power)) 5
  mapM (\(_,n) -> setVarKind n IntVar) $ showAll _power
  mapM (\(_,n) -> varBds n 0 1) $ showAll _power

main = do
  x <- glpSolveVars mipDefaults lpCards
  case x of (Success, Just (obj, vars)) -> do
                             putStrLn "Success!"
                             putStrLn ("Cost: " ++ (show obj))
                             mapM (putStrLn . show) (filter (\(name, count) -> count > 0) $ Map.toList vars)
                             putStrLn "Variables: "
            (failure, result) -> putStrLn ("Failure: " ++ (show failure))
