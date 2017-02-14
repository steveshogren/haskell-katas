module DamagePen where

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

murdockDps :: Double
murdockDps = dps 20 14 18 8 yesBonusCrit 86 1.35 1 15

-- pp + att_spd + crit_points = 60

dps :: (Fractional a) => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
dps power_points attack_speed_points crit_points pen crit_damage base_damage base_attack_speed scaling lvl =
  let reduction = dmgReduction 0 pen lvl
      raw_dps = (base_damage+6*power_points*scaling)
              * ((1+(0.055*attack_speed_points))/base_attack_speed)
              * (1+(0.04*crit_points)*(crit_damage-1))
  in raw_dps * (1 - reduction)


