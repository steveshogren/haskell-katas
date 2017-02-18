module Optimization (main) where

import Prelude hiding ((*), (/), (+), (-))
import qualified Data.Map as Map
import Data.LinearProgram as DLP
import Control.Monad.LPMonad
import Data.LinearProgram.GLPK
import Control.Monad.State
import Data.LinearProgram
import Data.List
import qualified Data.Set as Set

onDemandHourlyCost = 0.64
reservationFixedCosts = [("light", 552.0), ("medium", 1280.0), ("heavy", 1560.0)]
reservationVariableCosts = [("light", 0.312), ("medium", 0.192), ("heavy", 0.128)]
reservationTypes = map fst reservationFixedCosts

dailyCost :: Map.Map [Char] Double -> LinFunc [Char] Double
dailyCost loadPattern = let
    periods = Map.keys loadPattern
    reservationFixedCostsObj = [ ( cost / 365.0, "reservation_" ++ kind)
                             | (kind, cost) <- reservationFixedCosts ] -- cost of reserving an instance
    reservationVariableCostsObj = [ (cost*8, "reserved_" ++ kind ++ "_" ++ period)
                                 | (kind, cost) <- reservationVariableCosts,
                                 period <- periods ]
                 -- cost of *running* the reserved instance
    onDemandVariableCostsObj = [ (onDemandHourlyCost*8, "onDemand_" ++ p) | p <- periods ]
                           -- cost of running on-demand instances
                        in
                        linCombination (reservationFixedCostsObj ++ reservationVariableCostsObj ++ onDemandVariableCostsObj)

allVariables :: [String]
allVariables = ["crit", "power", "speed", "pen"]

-- pointContraints = (linCombination [(1, "speed"), (1, "crit"), (1, "power")])

lp :: Map.Map [Char] Double -> LP String Double
lp loadPattern = execLPM $ do
                           setDirection Max
                           setObjective (linCombination [(1, "power"), (1, "speed"), (1,"crit")])
                           --mapM (\(func, val) -> func `geqTo` val) (reservationConstraints loadPattern)
                           mapM (\var -> varGeq var 0.0) allVariables
                           mapM (\var -> setVarKind var IntVar) allVariables

printLPSolution :: Map.Map [Char] Double -> IO ()
printLPSolution loadPattern = do
  x <- glpSolveVars mipDefaults (lp loadPattern)
  putStrLn (show allVariables)
  case x of (Success, Just (obj, vars)) -> do
                             putStrLn "Success!"
                             putStrLn ("Cost: " ++ (show obj))
                             putStrLn ("Variables: " ++ (show vars))
            (failure, result) -> putStrLn ("Failure: " ++ (show failure))

-- dps formula to maximize
-- maximize = (86+6*power*1) * ((1+(0.055*speed))/1.35) * (1+(0.04*crit)*(2.5-1)) on power >= 0, speed >= 0, crit >= 0;

main :: IO ()
main = do
  printLPSolution (Map.fromList [ ("night", 12.2), ("morning", 25.1), ("evening", 53.5) ])


objFun :: LinFunc String Int
objFun = linCombination [(1, "x1"), (1, "x2"), (1, "x3")]

n *& v = linCombination [(n,v)]

lp2 :: LP String Int
lp2 = execLPM $ do
  setDirection Max
  setObjective objFun
  leqTo (add $ map (1 *&) ["x1", "x2", "x3"]) 100
  leqTo (10 *& "x1" + 4 *& "x2" + 5 *& "x3") 600
  leqTo (linCombination [(2, "x1"), (2, "x2"), (6, "x3")]) 300
  varGeq "x1" 0
  varBds "x2" 0 50
  varGeq "x3" 0
  setVarKind "x1" IntVar
  setVarKind "x2" ContVar

main2 = print =<< glpSolveVars mipDefaults lp2

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
    { cost :: Integer
    , power :: Integer
    , speed :: Integer
    , crit :: Integer
    , pen :: Integer
    , lifesteal :: Integer
    , crit_bonus :: Integer
    , name :: String
    , letter :: String
    }

toCard (cost, power, speed, crit, pen, lifesteal, crit_bonus, name) letter =
  Card { cost = cost
    , power = power
    , speed = speed
    , crit = crit
    , pen = pen
    , lifesteal = lifesteal
    , crit_bonus = crit_bonus
    , name = name
    , letter = letter
    } 

mainCards =
  zipWith toCard
    [(2, 2, 1, 0, 0, 0, 0, "madstone gem")
    ,(3, 2, 0, 2, 0, 0, 0, "impact hammer")
    ,(3, 3, 1, 0, 0, 0, 0, "windcarver blade")
    ,(3, 0, 0, 1, 0, 3, 0, "brand ironeater")
    ,(2, 0, 2, 1, 0, 0, 0, "redeye nitro")
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

-- desired numbers (15,12,13,8.0,11,1)

obj fn total =
  let elem = map (\next -> (show $ fn next) ++ letter next)  mainCards
  in (intercalate " + ") elem ++  " = " ++ (show total)

showOne fn total permutations =
  let elem = map (\next -> (show $ fn next) ++ letter next) permutations
  in (intercalate " + ") elem

showAll fn total =
  foldl (\ret card -> ret ++ " + " ++ (showOne fn total (twoTypeCardPermutations card))) "" mainCards

cardHasTwoTypes card =
  let points = (power card) + (speed card) + (crit card) + (pen card) + (lifesteal card)
  in points > 1

cardFields True True False False False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, power = (power card) + a, speed = (speed card) + b}
cardFields True False True False False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, power = (power card) + a, crit = (crit card) + b}
cardFields True False False True False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, power = (power card) + a, pen = (pen card) + b}
cardFields True False False False True card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, power = (power card) + a, lifesteal = (lifesteal card) + b}
cardFields False True True False False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, speed = (speed card) + a, crit = (crit card) + b}
cardFields False True False True False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, speed = (speed card) + a, pen = (pen card) + b}
cardFields False True False False True card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, speed = (speed card) + a, lifesteal = (lifesteal card) + b}
cardFields False False True True False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, crit = (crit card) + a, pen = (pen card) + b}
cardFields False False True False True card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, crit = (crit card) + a, lifesteal = (lifesteal card) + b}
cardFields False False False True True card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, pen = (pen card) + a, lifesteal = (lifesteal card) + b}
cardFields False False False False True card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, lifesteal = (lifesteal card) + a + b}
cardFields True False False False False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, power = (power card) + a + b}
cardFields False True False False False card a b n = card {letter = (letter card) ++ n, cost = (cost card) + a + b, speed = (speed card) + a + b}

twoTypeCardPermutations :: Card -> [Card]
twoTypeCardPermutations card =
  let hasPower = power card > 0
      hasSpeed = speed card > 0
      hasCrit = crit card > 0
      hasPen = pen card > 0
      hasLS = lifesteal card > 0
  in map (\(ac, bc, n) -> cardFields hasPower hasSpeed hasCrit hasPen hasLS card ac bc n) twoCardUpgrades

o1 = obj cost 65
o2 = obj power 15
o3 = obj speed 12
o4 = obj crit 13
o5 = obj pen 8
o6 = obj lifesteal 11
o7 = obj crit_bonus 1

allO = mapM putStrLn [o1 ++ " -- cxp"
                     , o2 ++ " -- power"
                     , o3 ++ " -- speed"
                     , o4 ++ " -- crit"
                     , o5 ++ " -- pen"
                     , o6 ++ " -- lifesteal"
                     , o7 ++ " -- crit_bonus"]


-- 2a + 3b + 3c + 3d + 2e + 3f + 3g + 3h + 6i + 6j + 3k + 3l + 6m = 65 -- cxp
-- 2a + 2b + 3c + 0d + 0e + 3f + 1g + 1h + 1i + 0j + 3k + 2l + 0m = 15 -- power
-- 1a + 0b + 1c + 0d + 2e + 0f + 0g + 3h + 0i + 0j + 0k + 0l + 1m = 12 -- speed
-- 0a + 2b + 0c + 1d + 1e + 0f + 3g + 0h + 0i + 0j + 1k + 0l + 0m = 13 -- crit
-- 0a + 0b + 0c + 0d + 0e + 1f + 0g + 0h + 0i + 0j + 0k + 0l + 0m = 8 -- pen
-- 0a + 0b + 0c + 3d + 0e + 0f + 0g + 0h + 0i + 1j + 0k + 0l + 0m = 11 -- lifesteal
-- 0a + 0b + 0c + 0d + 0e + 0f + 0g + 0h + 1i + 1j + 0k + 0l + 1m = 1 -- crit_bonus
