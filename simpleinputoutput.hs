main = do
    putStrLn "Please enter your name:"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ ", how are you?")
  

trianglearea = do
    putStrLn "Base?"
    base <- getLine
    putStrLn "Height?"
    height <- getLine
    putStrLn $ "Area: " ++ show ((read base)*(read height)/2)

trianglearea2 = do
    putStrLn "Base?"
    base <- getLine
    putStrLn "Height?"
    height <- getLine
    putStrLn $ "Area: " ++ show ((read base)*(read height)/2)

greet = do
    putStrLn "Your name"
    name <- getLine
    let response = case name of
                    "axel" -> "hej"
                    x      -> "tja"
    putStrLn response
    
    
doStuff :: Int -> String
doStuff x
  | x < 3     = report "less than three"
  | otherwise = report "normal"
  where
    report y = "the input is " ++ y
 

(\\) :: (Eq a) => [a] -> [a] -> [a]
xs \\ ys = foldl (\zs y -> delete y zs) xs ys
    where delete x (y:xs) = if x==y then (delete x xs) else y:(delete x xs)
          delete x [] = []

