-- quickSort :: (Ord a) => [a] -> [a]
-- quickSort [] = []
-- quickSort (x : xs) = (quickSort less) ++ (x : equal) ++ (quickSort more)
    -- where
        -- less = filter (< x) xs
        -- equal = filter (== x) xs
        -- more = filter (> x) xs
import Data.Char (toUpper)
        
dictionary = ["I", "have", "a", "thing", "for", "Linux"]

quickSort' :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
quickSort' _ [] = []
quickSort' c (x : xs) = (quickSort' c less) ++ (x : equal) ++ (quickSort' c more)
    where
        less  = filter (\y -> y `c` x == LT) xs
        equal = filter (\y -> y `c` x == EQ) xs
        more  = filter (\y -> y `c` x == GT) xs
        
usual :: Ord a => a -> a -> Ordering
usual = compare

insensitive x y = compare (map toUpper x) (map toUpper y)  

-- insensitive' x y = compare (map toUpper x) (map toUpper y)  

quickSort :: (Ord a) => [a] -> [a]
quickSort x = (quickSort' usual x)


-- for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
-- for ::  -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i p f job = do
                 let x = map f [i | i<-[1..100], p i]
                 print $ head x
                 for (i+1) p f job
                 
-- for 1 (<10) (+1) print
