module DamagePen where

import Math.LinearEquationSolver

-- 2x + 3y + 4z = 20
-- 6x - 3y + 9z = -6
-- 2x      +  z = 8
-- x = solveIntegerLinearEqs Z3 [[2, 3, 4],[6, -3, 9],[2, 0, 1]] [20, -6, 8] 

attackSpeed :: Fractional a => a -> a -> a
attackSpeed bat asm =(1/bat)*asm

yesDoubleCrit :: Double
yesDoubleCrit = 2
noDoubleCrit :: Double
noDoubleCrit = 1

dmgReduction :: Fractional a => a -> a -> a -> a
dmgReduction ar pn lvl =
  let armor = (ar * 7) + 30
      pen = pn * 4
  in (armor-pen) / (100 + (armor - pen) + (10*(lvl-1)))

murdockDps :: Double
murdockDps = dps 21 14 18 7 yesDoubleCrit 86 1.35 1 15

dps :: (Fractional a, Fractional a1) => a -> a -> a -> a1 -> a -> a -> a -> a -> a1 -> a
dps power_points attack_speed_points crit_points pen crit_damage base_damage  base_attack_speed scaling lvl =
  let reduction = dmgReduction 31.5 pen lvl
      dps = (base_damage + 6 * power_points * scaling)
              * ((1 + (0.055 * attack_speed_points)) / base_attack_speed)
              * (1 + (0.04 * crit_points * (crit_damage - 1)))
  in dps
  -- * (1 - reduction)

calcDamagef :: Num a => a -> a -> a -> a
calcDamagef bp arc ar =
  calcDamage bp arc ar 0 0 0 0 0 0

calcDamage
  :: Num a => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
calcDamage bp arc ar aoerating dbs dbt dr pdr edr =
  ( bp  + (arc * ar ) ) * aoerating * ( dbs ) * ( dbt ) * ( 1 - dr ) * ( 1 - pdr ) * ( 1 - edr )

