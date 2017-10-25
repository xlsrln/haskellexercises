main = do
        putStrLn "What is your name?"
        x <- getContents
        putStrLn ("Hello " ++ x)