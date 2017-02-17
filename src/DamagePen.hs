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

-- old formula?
-- dmgReduction :: Fractional a => a -> a -> a -> a
-- dmgReduction ar pn lvl =
--   let armor = ar + 31.5
--       pen = pn * 4.0
--   in (armor-pen) / (100+(armor-pen)+(10*(lvl-1)))

dmgReduction2 :: Double -> Double -> Double
dmgReduction2 armorpts penpts =
  let effectiveArmor = 31.5 + (7 * armorpts) - (penpts * 4.0)
      realArmor = if (effectiveArmor < 0) then 0 else effectiveArmor
      reduction = (100/(100 + effectiveArmor))
  in if reduction > 1 then 1 else reduction

dps :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
dps power_points attack_speed_points crit_points pen crit_damage base_damage base_attack_speed scaling lvl =
  let reduction = dmgReduction2 0 pen
      base_dmg = (base_damage+(6*power_points*scaling))
      hits_second = ((1+(0.055*attack_speed_points))/base_attack_speed)
      crit_bonus = (1+((0.04*crit_points)*(crit_damage-1)))
  in base_dmg * hits_second * crit_bonus * reduction

murdockDps :: Double -> Double -> Double -> Double -> Double -> Double
murdockDps pwr speed crit pen bonus = dps pwr speed crit pen bonus 86 1.16 1 15

calcIfUnder :: Double -> Double -> Double -> Double -> Double -> Double -> (Double, String)
calcIfUnder dmg speed crit pen critbonus max =
  if (dmg + speed + crit + pen + (critbonus * 6)) == max
  then
    let bonus = if critbonus == 1 then yesBonusCrit else noBonusCrit
    in (murdockDps dmg speed crit pen bonus, show (dmg,speed,crit,pen,critbonus))
  else (0, "")

maxDps =
  let totalPoints = 66
      lifeSteal = 11
      ward = 1
      points = totalPoints - lifeSteal - ward
      totals = [ (calcIfUnder dmg speed crit pen critbonus points) |
                 dmg <- [0..30],
                 speed <- [0..30],
                 crit <- [0..30],
                 pen <- [0..30],
                 critbonus <- [0..1]]
  in take 2 $ reverse $ sort totals
