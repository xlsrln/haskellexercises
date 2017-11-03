import Text.Read
import Control.Monad

main = do 
    putStrLn "enter number: "
    input <- getLine
    let x = readMaybe input :: Maybe Double
    case x of 
        Just d -> putStrLn ("double your number is " ++ show (2*d))
        Nothing -> do 
            putStrLn "not a number " -- 
            main
            
adding = do 
    putStrLn "enter two numbers"
    first <- readMaybe <$> getLine
    second <- readMaybe <$> getLine
    let x = (+) <$> first <*> second :: Maybe Double
    case x of Just d -> putStrLn $ "the sum is " ++ show d
              Nothing -> do putStrLn "not good"
                            adding

            
interactiveConcatenating :: IO ()
interactiveConcatenating = do
    putStrLn "Choose two strings:"
    sx <- getLine
    sy <- getLine
    putStrLn "Let's concatenate them:"
    putStrLn (sx ++ sy)
    
interactiveConcatenating2 :: IO ()
interactiveConcatenating2 = do
    putStrLn "Choose two strings:"
    sx <- (++) <$> getLine <*> getLine
    putStrLn "Let's concatenate them:"
    putStrLn (sx)