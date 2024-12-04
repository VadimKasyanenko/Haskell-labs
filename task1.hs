import Data.Function (fix)

gcdHelper f = \a b -> 
  if a == b 
    then a 
    else if a > b 
      then f (a - b) b 
      else f a (b - a)

gcd' = fix gcdHelper
