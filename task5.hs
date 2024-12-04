import Data.Function (fix)

minHelper :: ([Integer] -> Integer) -> [Integer] -> Integer
minHelper _ [x]    = x
minHelper f (x:xs) = min x (f xs)

minList :: [Integer] -> Integer
minList = fix minHelper
