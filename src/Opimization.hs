module Optimization (main) where

import Prelude hiding ((*), (/))
import qualified Data.Map as Map
import Data.LinearProgram as DLP
import Control.Monad.LPMonad
import Data.LinearProgram.GLPK
import Control.Monad.State

loadPattern = Map.fromList [ ("night", 5), ("morning", 25), ("evening", 100) ]

onDemandHourlyCost = 0.64
reservationFixedCosts = [("light", 552.0), ("medium", 1280.0), ("heavy", 1560.0)]
reservationVariableCosts = [("light", 0.312), ("medium", 0.192), ("heavy", 0.128)]
reservationTypes = map (\(x,y) -> x) reservationFixedCosts

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

-- (# on-demand) + (# light reserved) + (# medium reserved) + (# heavy reserved) >= desired capacity
capacityConstraints loadPattern = [ (linCombination (
                                         [ (1.0, "onDemand_" ++ period) ]
                                      ++ [(1.0, "reserved_" ++ k ++ "_" ++ period) | k <- reservationTypes]
                                     ),
                                     load)
                                        | (period, load) <- (Map.assocs loadPattern)]

-- (# heavy instances reserved) - (# heavy reserved instances running at night) >= 0
reservationConstraints loadPattern = [ (linCombination [(1.0, "reservation_" ++ k),
                                       (-1.0, "reserved_" ++ k ++ "_" ++ p)] , 0.0)  |
                                       p <- (Map.keys loadPattern),
                                       k <- reservationTypes ]

allVariables loadPattern = ["onDemand_" ++ p | p <- periods] ++
                           ["reserved_" ++ k ++ "_" ++ p | p <- periods, k <- reservationTypes] ++
                           ["reservation_" ++ k | k <- reservationTypes]
    where periods = Map.keys loadPattern

lp :: Map.Map [Char] Double -> LP String Double
lp loadPattern = execLPM $ do
                           setDirection Min
                           setObjective (dailyCost loadPattern)
                           mapM (\(func, val) -> func `geqTo` val) (reservationConstraints loadPattern)
                           mapM (\(func, val) -> func `geqTo` val) (capacityConstraints loadPattern)
                           mapM (\var -> varGeq var 0.0) (allVariables loadPattern)
                           mapM (\var -> setVarKind var IntVar) (allVariables loadPattern)

printLPSolution loadPattern = do
  x <- glpSolveVars mipDefaults (lp loadPattern)
  putStrLn (show (allVariables loadPattern))
  case x of (Success, Just (obj, vars)) -> do
                             putStrLn "Success!"
                             putStrLn ("Cost: " ++ (show obj))
                             putStrLn ("Variables: " ++ (show vars))
            (failure, result) -> putStrLn ("Failure: " ++ (show failure))

main :: IO ()
main = do
  printLPSolution (Map.fromList [ ("night", 12.2), ("morning", 25.1), ("evening", 53.5) ])
