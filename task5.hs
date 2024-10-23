collatz :: Integer -> Integer
collatz 1 = 1
collatz n
    | even n    = 1 + collatz (n `div` 2)
    | otherwise = 1 + collatz (3 * n + 1)
