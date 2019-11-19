import System.IO
import Data.List
import Data.Char
import Prelude

testing :: IO String
testing = do
	putStrLn "Enter name:"
	name <- getLine
	putStrLn("Yourname is: " ++ name)
	return ("hello")	

capitalName :: IO ()
capitalName = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
	
reverseWords :: IO ()
reverseWords = do
	putStrLn "Enter something"
	line <- getLine
	if null line
		then return ()
		else do
			putStrLn (unwords (map reverse (words line)))
			reverseWords
			
putStr_ :: String -> IO ()
putStr_ [] = return ()
putStr_ (x:xs) = do
	putChar x
	putStr_ xs
	


getInteger :: IO Integer
getInteger = do
	line <- getLine
	return (read line :: Integer)
				
getFloat :: IO Float
getFloat = do
	line <- getLine
	return (read line :: Float)


greeting :: IO Int
greeting = do
	putStrLn "Enter your name:"
	line <- getLine
	putStrLn ("Your Name is " ++ line)
	return (length line)
	
smallerThan :: Integer -> IO (Integer,[Integer])
smallerThan val = do x <- getInteger
                     if x >= val
                     then return (x,[])
                     else do (v,rest) <- smallerThan val
                             return (v,x:rest)
							 
getNumberSeries :: IO [Integer]
getNumberSeries = do
	x <- getInteger
	if x == 0 
		then return []
		else do 
			xs <- getNumberSeries
			return (x:xs)
							
sumSeries1 :: IO Integer
sumSeries1 = do
	xs <- getNumberSeries
	return(sum xs)
	
	
sumSeries2 :: IO Integer
sumSeries2 = do
	x <- getInteger
	if x == 0
		then return 0
		else do 
			xs <- sumSeries2
			return (x + xs)
			
smallerThanD :: Float -> IO (Float, [Float])
smallerThanD n = do
                putStrLn "Number daal, dhyan se float me"
                x <- getFloat
                if (x >= n)
				then return (x,[])
				else do
					(l,y) <- smallerThanD n
					return (l,x:y)
					
displayVert :: String -> IO Int
displayVert [] = return 0
displayVert (x:xs) = do
					putStrLn [x]
					y <- displayVert xs
					return (1 + y)
					
displayWords :: IO()
displayWords = do
				putStrLn "Daal BC"
				x <- getLine
				sequence_ [putStrLn y | y <- words x]
				
displayData :: [(String,Int)] -> IO()
displayData [] = return ()
displayData ((a,b):xs) = do
						putStrLn(a ++ show (b)) 
						displayData xs
						
displayWords_ :: IO ()
displayWords_ = do
	putStrLn "Enter the String"
	x <- getLine
	dispalyVerticle (words x)
	
dispalyVerticle :: [String] -> IO ()
dispalyVerticle [] = return ()
dispalyVerticle (x:xs) = do
	putStrLn x
	dispalyVerticle xs
					
					
displayVert_ :: String -> IO Int
displayVert_ [] = return 0 
displayVert_ (x:xs) = do
					  putChar x
					  putChar '\n'
					  y <- displayVert_ xs
					  return (1+y)
					  
adder :: IO Integer
adder = do
		putStrLn "How many Nos."
		n <- getInteger
		adderInner n
		
adderInner :: Integer -> IO Integer
adderInner 0 = return 1
adderInner n = do
				y <- getInteger
				x <- adderInner (n-1)
				return  (x*y)
				
fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)

fib :: Integer -> Integer
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

printFib :: IO Integer
printFib = do
			x<-getInteger
			return (fib x)
			
			
printFibSeries :: IO ()
printFibSeries = do
				x <- getInteger
				printFibSeriesInner x 1
				
printFibSeriesInner :: Integer -> Integer -> IO() 
printFibSeriesInner n y= if (y>n)
						then
						return ()
						else do
							putStrLn (show(fib y))
							printFibSeriesInner n (y+1)
						