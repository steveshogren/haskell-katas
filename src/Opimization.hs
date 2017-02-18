{-# LANGUAGE TemplateHaskell #-}
module Optimization (main) where

import Control.Lens
import Prelude hiding ((*), (/), (+), (-))
import qualified Data.Map as Map
import Data.LinearProgram as DLP
import Control.Monad.LPMonad
import Data.LinearProgram.GLPK
import Control.Monad.State
import Data.LinearProgram
import Data.List
import qualified Data.Set as Set

upgradeTypes 1 = (1, "a")
upgradeTypes 2 = (2, "a")
upgradeTypes 3 = (3, "a")
upgradeTypes 4 = (1, "b")
upgradeTypes 5 = (2, "b")
upgradeTypes 6 = (3, "b")

flattenPermutations :: [(Integer, String)] -> (Integer, Integer)
flattenPermutations = foldl (\(ac, bc) (cost, t) -> if (t == "a") then (ac+cost, bc) else (ac,bc+cost)) (0, 0)

addName (ac, bc) = (ac, bc, "-" ++ (show ac) ++ "-" ++ (show bc) )

twoCardUpgrades :: [(Integer, Integer, String)]
twoCardUpgrades =
  let types = Set.toList $ Set.fromList $
                map ((map upgradeTypes) . sort) [ [x,y,z]
                          | x <- [1..6],
                            y <- [1..6],
                            z <- [1..6]]
  in map (addName . flattenPermutations) types

oneCardUpgrades :: [Integer]
oneCardUpgrades = [1..9]

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


obj fn total =
  let elem = map (\next -> (show $ fn next) ++ _letter next)  mainCards
  in (intercalate " + ") elem ++  " = " ++ (show total)

showOne fn permutations =
  map (\next -> (fn next, (_name next))) permutations

showAll fn =
  foldl (\ret card -> ret ++ (showOne fn (twoTypeCardPermutations card))) [] mainCards


cardFields True True False False False  card a b = card {_power = (_power card) + a, _speed = (_speed card) + b}
cardFields True False True False False  card a b = card {_power = (_power card) + a, _crit = (_crit card) + b}
cardFields True False False True False  card a b = card {_power = (_power card) + a, _pen = (_pen card) + b}
cardFields True False False False True  card a b = card {_power = (_power card) + a, _lifesteal = (_lifesteal card) + b}
cardFields False True True False False  card a b = card {_speed = (_speed card) + a, _crit = (_crit card) + b}
cardFields False True False True False  card a b = card {_speed = (_speed card) + a, _pen = (_pen card) + b}
cardFields False True False False True  card a b = card {_speed = (_speed card) + a, _lifesteal = (_lifesteal card) + b}
cardFields False False True True False  card a b = card {_crit = (_crit card) + a, _pen = (_pen card) + b}
cardFields False False True False True  card a b = card {_crit = (_crit card) + a, _lifesteal = (_lifesteal card) + b}
cardFields False False False True True  card a b = card {_pen = (_pen card) + a, _lifesteal = (_lifesteal card) + b}
cardFields False False False False True card a b = card {_lifesteal = (_lifesteal card) + a + b}
cardFields True False False False False card a b = card {_power = (_power card) + a + b}
cardFields False True False False False card a b = card {_speed = (_speed card) + a + b}

twoTypeCardPermutations :: Card -> [Card]
twoTypeCardPermutations card =
  let hasPower = _power card > 0
      hasSpeed = _speed card > 0
      hasCrit = _crit card > 0
      hasPen = _pen card > 0
      hasLS = _lifesteal card > 0
  in
    concatMap (\c -> map (\(ac, bc, n) ->
                            let nc = cardFields hasPower hasSpeed hasCrit hasPen hasLS card ac bc
                                newCost = (_cost nc) + ac + bc
                            in nc { _name = (_name nc) ++ (n++ "-s" ++ (show c) ++ "-" ++ (show newCost)),
                                    _cost = newCost}) twoCardUpgrades) [1..5]

objFunCards :: LinFunc String Integer
objFunCards = linCombination (showAll _cost)

-- desired numbers (15,12,13,8.0,11,1)

lpCards :: LP String Integer
lpCards = execLPM $ do
  leqTo (linCombination (showAll _cost)) 66
  geqTo (linCombination (showAll _power)) 15
  geqTo (linCombination (showAll _speed)) 12
  geqTo (linCombination (showAll _crit)) 13
  geqTo (linCombination (showAll _pen)) 8
  geqTo (linCombination (showAll _lifesteal)) 11
  geqTo (linCombination (showAll _crit_bonus)) 1
  leqTo (linCombination (map (\(_,n) -> (1, n)) $ showAll _power)) 6
  mapM (\(_,n) -> setVarKind n IntVar) $ showAll _power
  mapM (\(_,n) -> varBds n 0 1) $ showAll _power

main = do
  x <- glpSolveVars mipDefaults lpCards
  case x of (Success, Just (obj, vars)) -> do
                             putStrLn "Success!"
                             putStrLn ("Cost: " ++ (show obj))
                             putStrLn ("Variables: " ++ (show (filter (\(name, count) -> count > 0) $ Map.toList vars)))
            (failure, result) -> putStrLn ("Failure: " ++ (show failure))
