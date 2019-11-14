removeEven :: [Int] -> [Int]
removeEven [] = []
removeEven xs = reverse (fst (helper ([], xs)))

helper :: ([Int], [Int]) -> ([Int], [Int])
helper (lst, []) = (lst, [])
helper (lst, (x:xs))
  |isEven x = helper(lst,xs)
  |otherwise = helper((x:lst), xs)
    where
      isEven :: Int -> Bool
      isEven n
        |n `mod` 2 == 0 = True
        |otherwise = False