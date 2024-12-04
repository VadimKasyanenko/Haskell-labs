import Data.Function (fix)

sumHelper :: ([Integer] -> Integer) -> [Integer] -> Integer
sumHelper _ []     = 0
sumHelper f (x:xs) = x + f xs

sumList :: [Integer] -> Integer
sumList = fix sumHelper
