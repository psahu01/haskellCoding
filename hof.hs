foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f e [] = e
foldr_ f e (x:xs) = f x (foldr_ f e xs)

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ f e [] = e
foldl_ f e (x:xs) = foldl_ f (f e x) xs

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

mapHOF :: (a -> b) -> [a] -> [b]
mapHOF f = foldr (\a acc -> f a:acc) []

filterHOF :: (a -> Bool) -> [a] -> [a]
filterHOF p = foldr (\x acc -> if p x then x:acc else acc) []

takeWhileHOF :: (a -> Bool) -> [a] -> [a]
takeWhileHOF p = foldr (\x acc -> if p x then x:acc else []) []

dropWhileHOF :: (a -> Bool) -> [a] -> [a]
dropWhileHOF p xs= foldr (\x acc -> if p x then drop 1 xs else []) [] xs

dropFoldr :: Eq a => Int -> [a] -> [a]
dropFoldr n xs = foldr (\a acc -> if ((length acc) < (length xs)-n) then a:acc else acc) [] xs

len :: [a] -> Int
len = foldr (\x acc -> 1 + acc) 0

sumx :: [Int] -> Int
sumx = foldr (+) 0

lengthX :: [a] -> Int
lengthX = foldr (\_ n -> 1+n) 0

concatT :: [a] -> [a] -> [a]
concatT xs = foldr (:) xs

reverseFoldr :: [a] -> [a]
reverseFoldr = foldr (\x acc -> acc ++ [x]) []
--dropWhileLC :: (a -> Bool) -> [a] -> [a]
--dropWhileLC p xs = [ xs | x <- xs, p x]

--allHOF :: (a -> Bool) -> [a] -> Bool
--allHOF

--iter :: Int -> (a -> a) -> (a -> a)
--iter n f
--  |n <= 0 = 0
--  |otherwise = f iter (n-1) f

--map using foldr
--filter using foldr
--trees 
