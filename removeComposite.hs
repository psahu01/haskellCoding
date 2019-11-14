import Data.List
import Data.Maybe

factors :: Int -> [Int]
factors x | x <= 0 = []
          | x == 1 = [1]
          |otherwise = [a | a <- [1..x], x `mod` a == 0]

isPrimeNo :: Int -> Bool
isPrimeNo p = factors p == [1,p]

removeCompositeNum :: [Int] -> [Int]
removeCompositeNum [] = []
removeCompositeNum xs = reverse (fst (helper ([], xs)))

helper :: ([Int], [Int]) -> ([Int], [Int])
helper (lst, []) = (lst, [])
helper (lst, (x:xs))
  |isPrimeNo x = helper((x:lst),xs)
  |otherwise = helper(lst, xs)

removeCompositePos :: [Int] -> [Int]
removeCompositePos [] = []
removeCompositePos xs = reverse (fst (helper2 ([], xs, length xs)))

helper2 :: ([Int], [Int], Int) -> ([Int], [Int])
helper2 (lst, [], n) = (lst, [])
helper2 (lst, (x:xs), n)
--  | n<=0 = (lst , (x:xs))
--  | n==1 = (lst, (x:xs))
  |isPrimeNo (findIndexMy x (x:xs)) = helper2((x:lst),xs, length xs)
  |otherwise = helper2(lst, xs, length xs)

findIndexMy :: Eq a => a -> [a] -> Int
findIndexMy _ [] = 0
findIndexMy n (x:xs)| not(n == x) = 1 + findIndexMy n xs
                    | otherwise = findIndexMy n xs