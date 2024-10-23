clone :: Int -> [a] -> [a]
clone _ [] = []
clone n (x:xs) = replicate n x ++ clone n xs
