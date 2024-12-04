import Data.Function (fix)

reverseHelper :: ([a] -> [a]) -> [a] -> [a]
reverseHelper _ []     = []
reverseHelper f (x:xs) = f xs ++ [x]

reverseList :: [a] -> [a]
reverseList = fix reverseHelper
