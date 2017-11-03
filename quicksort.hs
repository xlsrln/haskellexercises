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


for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()

-- wrong: should not do the first job if the condition is not satisfied
for i p f job = do
                 job i
                 if not (p i) then return () 
                              else for (f i) p f job
-- right
for' i p f job = do
                 if (p i) 
                 then do job i
                         for' (f i) p f job
                 else return ()
                 
                 
                 
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = do return []
sequenceIO (x:xs) = do
                result <- x
                rest <- sequenceIO xs
                return (result:rest)

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f [] = return []
mapIO f (x:xs) = do f x 
                    mapIO f xs