power :: (Integral t, Num p) => t -> p -> p
power 0 _ = 1
power n x
  | even n    = let halfPower = power (n `div` 2) x
                in halfPower * halfPower
  | otherwise = x * power (n - 1) x
