q1s :: [ Int ] -> [ Int ]
q1s [] = []
q1s ( x : xs )
  | f x = [2*x | x <- xs]
  | otherwise = x : [2*x + 1 | x <- xs]
    where
      f :: Int -> Bool
      f y = ( y*y*y > 3*y + 1)

test :: [Int] -> [Int]
test (x:xs) = x : [ 2*x + 1 | x <- xs ]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

oddsMy :: Int -> [Int]
oddsMy n = [xs|xs <- [1..double(n)], not(xs `mod` 2 == 0)]

double :: Int -> Int
double x = 2*x

odds :: Int -> [Int]
odds n = map f [0..n-1]
          where f x = x*2 + 1

mysignum :: Integral a => a -> a
mysignum n =  if n > 0 then 1 else 
                if n == 0 then 0 else -1

maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

main = putStrLn "asdfa"