--main = putStrLn "Hello World!!!"
main = do
      putStrLn "Naam ka hai be"
      name <- getLine
      putStrLn ("Tumhar naam " ++ name ++ " ba")

