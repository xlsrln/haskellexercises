import Text.Read
import Control.Monad
import Control.Monad.Trans.Maybe 

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
        
adding2 :: IO ()        
adding2 = do 
    putStrLn "enter two numbers"
    let a = readMaybe <$> getLine
    a1 <- a
    a2 <- a
    let x = (+) <$> a1 <*> a2 :: Maybe Double
    putStrLn $ show (fmap (+) a1 <*> a2) 
    
adding5 :: IO ()        
adding5 = do 
    putStrLn "enter two numbers"
    let a = readMaybe <$> getLine
    a >>= \ a1 -> a >>= \ a2 ->
        case (+) <$> a1 <*> a2 of 
            Just d -> putStrLn $ "the sum is " ++ show d
            Nothing -> do putStrLn "not good"
                          adding5
                
adding6 :: IO ()
adding6 = do
    putStrLn "enter two numbers"
    [a1, a2] <- replicateM 2 (readMaybe <$> getLine)
    case (+) <$> a1 <*> a2 of 
        Just d -> putStrLn $ "the sum is " ++ show d
        Nothing -> do putStrLn "not good"
                      adding6
                
adding3 :: IO ()        
adding3 = do 
    putStrLn "enter two numbers"
    let a = readMaybe <$> getLine
    a >>= \ a1 -> a >>= \ a2 -> putStrLn $ "their sum is " ++ show (fmap (+) a1 <*> a2)                

-- adding4 :: IO ()        
adding4 = do 
    putStrLn "enter two numbers"
    let a = readMaybe <$> getLine
    let addmaybes x y = case fmap (+) x <*> y of 
                            Just d -> return d
                            Nothing -> return 0
    a >>= \x -> a >>= \y -> addmaybes x y >>= \disp -> putStrLn $ "result: " ++ show disp
    
    

a = readMaybe <$> getLine :: IO (Maybe Double)
g = \ x -> show (fmap (+) x <*> Just 1)

-- addmaybes :: Maybe Double -> Maybe Double -> Double
addmaybes2 x y = 
    case fmap (+) x <*> y of 
        Just d -> return d
        Nothing -> return 0

test = do 
        b <- a
        print $ g b

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
    
interactiveConcatenating3 :: IO ()    
interactiveConcatenating3 = do
    putStrLn "Choose two strings to be concatenated:"
    (++) <$> getLine <*> getLine >>= putStrLn