import Prelude

--List comprehension
concatx :: [[a]] -> [a]
concatx xss = [x | xs <- xss, x <- xs]

concatxx :: [[[a]]] -> [a]
concatxx xsss = [x | xss <- xsss,xs <-xss, x <- xs]

replicate1     :: Int -> a -> [a]
replicate1 k r = [ r | x <-[1..k]]

lastx :: [a] -> a
lastx [x] = x
lastx (y:ys) = lastx (ys)

lengthx :: [a] -> Int
lengthx xs = sum [ 1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime x = factors x == [1,x]

nthprime :: Int -> Int
nthprime n = last (take n (filter prime1 [1..]))
                where prime1 x = factors x == [1,x]

primeList :: Int -> [Int]
primeList n = [ x | x <- [2..n],  prime x]

--Recursion
zipRecursion :: [a] -> [b] -> [(a,b)]
zipRecursion _ [] = []
zipRecursion [] _ = []
zipRecursion (x:xs) (y:ys) = (x,y):(zipRecursion xs ys)

--Recursion
reverseList :: [a] -> [a]
reverseList ns = fst (helperReverseList ([],ns))

helperReverseList :: ([a], [a]) -> ([a],[a])
helperReverseList (lst, []) = (lst,[])
helperReverseList (lst, (x:xs)) = helperReverseList ((x:lst), xs)

--List Comprehension
--reverseListComp :: [Char] -> [Char]
--reverseListComp xs = [x | x <- xs]

filterRecursion :: (a -> Bool) -> [a] -> [a]
filterRecursion p [] = []
filterRecursion p (x:xs)  | p x = x : filterRecursion p xs
                          | otherwise = filterRecursion p xs

filterx :: (a -> Bool) -> [a] -> [a]
filterx p xs = reverseList (fst (helpFilter ([], xs, p)))

helpFilter :: ([a] , [a], (a->Bool)) -> ([a], [a])
helpFilter (lst, [], p) = (lst, [])
helpFilter (lst, (x:xs), p) | p x = helpFilter((x:lst), xs, p)
                            | otherwise = helpFilter(lst, xs, p)

sumSqEven :: [Int] -> Int
sumSqEven [] = 0
sumSqEven xs = sum (map (^2) (filterRecursion even xs))

zipRecu :: [a] -> [b] -> [(a, b)]
zipRecu [] _ = []
zipRecu _ [] = []
zipRecu (x:xs) (y:ys) = (x,y):(zipRecu xs ys)

zipListComp :: [a] -> [b] -> [(a,b)]
zipListComp xs ys = [(xs!!n,ys!!n) | n <- [0..(min (length xs) (length ys)) - 1]]


--high order
sumx :: [Int] -> Int
sumx = foldr (+) 0

lengthX :: [a] -> Int
lengthX = foldr (\_ n -> 1+n) 0

concatT :: [a] -> [a] -> [a]
concatT xs = foldr (:) xs

reverseFoldr :: [a] -> [a]
reverseFoldr = foldr (\x xs -> xs ++ [x]) []