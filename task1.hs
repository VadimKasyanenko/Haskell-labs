import Data.List

unfoldNumbers :: Int -> [Int]
unfoldNumbers n = unfoldr (\b -> if b > 0 then Just (b - 1, b - 1) else Nothing) n

toBinaryDigits :: Int -> [Int]
toBinaryDigits 0 = [0]
toBinaryDigits n = reverse $ unfoldr (\b -> if b == 0 then Nothing else Just (b `mod` 2, b `div` 2)) n

fromBinaryDigits :: [Int] -> Int
fromBinaryDigits = foldl' (\acc x -> acc * 2 + x) 0

primeFactors :: Int -> [Int]
primeFactors n = unfoldr nextFactor (n, 2)
  where
    nextFactor (1, _) = Nothing
    nextFactor (m, d)
      | m `mod` d == 0 = Just (d, (m `div` d, d))
      | otherwise = if d * d > m then Just (m, (1, d)) else nextFactor (m, d + 1)

fibonacci :: Int -> [Int]
fibonacci n = take n $ unfoldr (\(a, b) -> Just (a, (b, a + b))) (1, 1)

fibonacciInf :: [Int]
fibonacciInf = unfoldr (\(a, b) -> Just (a, (b, a + b))) (1, 1)

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = unfoldr nextStep n
  where
    nextStep 1 = Nothing
    nextStep x = Just (x, if even x then x `div` 2 else 3 * x + 1)

sieve :: Int -> [Int]
sieve n = unfoldr nextPrime [2..n]
  where
    nextPrime [] = Nothing
    nextPrime (p:xs) = Just (p, filter (\x -> x `mod` p /= 0) xs)

sieveInf :: [Int]
sieveInf = unfoldr nextPrime [2..]
  where
    nextPrime [] = Nothing
    nextPrime (p:xs) = Just (p, filter (\x -> x `mod` p /= 0) xs)
