sayHello :: String -> IO ()
sayHello x= putStrLn ( "Hello, " ++ x ++ "!")

main = do sayHello "axel"