module DamagePen where

import Data.List

attackSpeed :: Fractional a => a -> a -> a
attackSpeed bat asm =(1/bat)*asm

yesBonusCrit :: Double
yesBonusCrit = 2.5

noBonusCrit :: Double
noBonusCrit = 1.5

armorPointsToNum :: Num a => a -> a
armorPointsToNum ar = ar * 7

dmgReduction :: Fractional a => a -> a -> a -> a
dmgReduction ar pn lvl =
  let armor = ar + 31.5
      pen = pn * 4.0
  in (armor-pen) / (100+(armor-pen)+(10*(lvl-1)))

-- pp + att_spd + crit_points = 60

dps :: (Fractional a) => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
dps power_points attack_speed_points crit_points pen crit_damage base_damage base_attack_speed scaling lvl =
  let reduction = dmgReduction 0 pen lvl
      raw_dps = (base_damage+6*power_points*scaling)
              * ((1+(0.055*attack_speed_points))/base_attack_speed)
              * (1+(0.04*crit_points)*(crit_damage-1))
  in raw_dps * (1 - reduction)

murdockDps :: Double -> Double -> Double -> Double -> Double
murdockDps pwr speed crit pen = dps pwr speed crit pen yesBonusCrit 86 1.35 1 15

calcIfUnder dmg speed crit pen max =
  if (dmg + speed + crit + pen) < max
  then (murdockDps dmg speed crit pen, show (dmg,speed,crit,pen ))
  else (0, "")

maxDps =
  let points = 60
      totals = [ (calcIfUnder dmg speed crit pen points) |
                 dmg <- [0..30],
                 speed <- [0..30],
                 crit <- [0..30],
                 pen<- [0..30]]
  in maximum totals
