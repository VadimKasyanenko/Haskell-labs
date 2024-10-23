delannoy :: Integer -> Integer -> Integer
delannoy 0 _ = 1 
delannoy _ 0 = 1  
delannoy m n = delannoy (m-1) n + delannoy m (n-1) + delannoy (m-1) (n-1)
