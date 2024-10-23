gcd' :: Integer -> Integer -> Integer
gcd' a b
  | a == b = a
  | a > b = gcd' (a - b) b
  | otherwise = gcd' a (b - a)
