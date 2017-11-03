module Adding where
import Text.Read
import Control.Monad

adding = do 
    putStrLn "enter first number"
    first <- readMaybe <$> getLine
    putStrLn "enter second number"
    second <- readMaybe <$> getLine
    let x = (+) <$> first <*> second :: Maybe Double
    case x of
        Just d -> putStrLn $ "the sum is " ++ show d
        Nothing -> do 
            putStrLn "not good"