removeEven :: [a] -> [a]
removeEven [] = []
removeEven xs = reverse (fst (helper ([], xs, length xs, length xs)))

helper :: ([a], [a], Int, Int) -> ([a], [a])
helper (lst, [], n, m) = (lst, [])
helper (lst, (x:xs), n, m)
  |isEven (n-m+1) = helper(lst, xs, n, length xs)
  |otherwise = helper((x:lst), xs, n, length xs)
    where
      isEven :: Int -> Bool
      isEven n
        |n `mod` 2 == 0 = True
        |otherwise = False

element :: Eq a => a -> [a] -> Bool
element _ [] = False
element n (x:xs)
  | n == x = True
  | otherwise = element n xs

removeEvenTest :: [a] -> [a]
removeEvenTest [] = []
removeEvenTest [x] = [x]
removeEvenTest (x:y:xs) = x: removeEvenTest xs

evens (x:xs) = (x:odds xs)
evens _ = []

odds (_:xs) = evens xs
odds _ = []