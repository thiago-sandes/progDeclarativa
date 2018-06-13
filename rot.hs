-- definição de função transpose para uso em função rot;
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])    

-- função rot
rot :: [[Int]] -> [[Int]]
rot = reverse . transpose


