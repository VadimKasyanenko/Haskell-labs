divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n-1], n `mod` x == 0]

isPerfect :: Integer -> Bool
isPerfect n = sum (divisors n) == n
