xZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
xZipWith _ [] _ = [] 
xZipWith _ _ [] = [] 
xZipWith f (x:xs) (y:ys) = f x y : xZipWith f xs ys
