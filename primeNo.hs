factors :: Int -> [Int]
factors x | x <= 0 = []
          | x == 1 = [1]
          |otherwise = [a | a <- [1..x], x `mod` a == 0]

primeNo :: Int -> Bool
primeNo p = [1,p] == (factors p)