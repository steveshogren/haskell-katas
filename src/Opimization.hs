module Optimization (main) where

import Prelude hiding ((*), (/), (+), (-))
import qualified Data.Map as Map
import Data.LinearProgram as DLP
import Control.Monad.LPMonad
import Data.LinearProgram.GLPK
import Control.Monad.State
import Data.LinearProgram
import Data.List

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

data Card = Card
    { cost :: Integer
    , power :: Integer
    , speed :: Integer
    , crit :: Integer
    , pen :: Integer
    , lifesteal :: Integer
    , crit_bonus :: Integer
    , name :: String
    , letter :: Char
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
    ,(6, 1, 0, 0, 0, 0, 1, "blade of agora")
    ,(6, 0, 0, 0, 0, 1, 1, "hunger maul")
    ,(3, 3, 0, 1, 0, 0, 0, "micro-nuke")
    ,(3, 2, 0, 0, 0, 0, 0, "sages ward")
    ,(6, 0, 1, 0, 0, 0, 1, "blast harness")
    ]
    ['a'..]

upgrades = [1, 2, 3]

-- desired numbers (15,12,13,8.0,11,1)

obj fn total =
  let elem = map (\next -> (show $ fn next) ++ [letter next])  mainCards
  in (intercalate " + ") elem ++  " = " ++ (show total)

o1 = obj cost 65
o2 = obj power 15
o3 = obj speed 12
o4 = obj crit 13
o5 = obj pen 8
o6 = obj lifesteal 11
o7 = obj crit_bonus 1

allO = mapM putStrLn [o1, o2,o3,o4,o5,o6,o7]

