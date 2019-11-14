oddCountDown :: Int -> [Int]
oddCountDown k
  | k <= 0 = []
  | otherwise = helper k
    where 
      helper n
        | n <= 0 = []
        | otherwise = (2*n -1) : (helper (n-1))