evalPolynomial :: [Double] -> Double -> Double
evalPolynomial coeffs x = foldl (\acc c -> acc * x + c) 0 coeffs
