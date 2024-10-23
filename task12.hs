delannoyPaths :: Int -> Int -> [[Int]]
delannoyPaths 0 0 = [[]]  
delannoyPaths a 0 = [replicate a 0]  
delannoyPaths 0 b = [replicate b 2]  
delannoyPaths a b = 
    [0:path | path <- delannoyPaths (a-1) b] ++
    [1:path | path <- delannoyPaths (a-1) (b-1)] ++
    [2:path | path <- delannoyPaths a (b-1)]
