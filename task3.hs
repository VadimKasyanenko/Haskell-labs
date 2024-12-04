import Data.Function (fix)

matrixMultiply :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixMultiply a b = [[a11, a12], [a21, a22]]
  where
    a11 = (a !! 0 !! 0) * (b !! 0 !! 0) + (a !! 0 !! 1) * (b !! 1 !! 0)
    a12 = (a !! 0 !! 0) * (b !! 0 !! 1) + (a !! 0 !! 1) * (b !! 1 !! 1)
    a21 = (a !! 1 !! 0) * (b !! 0 !! 0) + (a !! 1 !! 1) * (b !! 1 !! 0)
    a22 = (a !! 1 !! 0) * (b !! 0 !! 1) + (a !! 1 !! 1) * (b !! 1 !! 1)

matrixPowerHelper :: ([[Integer]] -> Integer -> [[Integer]]) -> [[Integer]] -> Integer -> [[Integer]]
matrixPowerHelper f _ 0 = [[1, 0], [0, 1]] -- Единичная матрица
matrixPowerHelper f m n
  | even n    = let halfPower = f m (n `div` 2)
                in matrixMultiply halfPower halfPower
  | otherwise = matrixMultiply m (f m (n - 1))

matrixPower :: [[Integer]] -> Integer -> [[Integer]]
matrixPower = fix matrixPowerHelper

fibonacciMatrix :: Integer -> [[Integer]]
fibonacciMatrix n = matrixPower [[0, 1], [1, 1]] n

fibonacci :: Integer -> Integer
fibonacci n = fibonacciMatrix n !! 0 !! 1
