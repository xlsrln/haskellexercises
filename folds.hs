
andd :: [Bool] -> Bool
andd [] = True
andd (True:xs) = andd xs
andd (False:xs) = False

anddd :: [Bool] -> Bool
anddd = foldr (==) True

orr :: [Bool] -> Bool   
orr [] = False
orr (True:xs) = True
orr (False:xs) = orr xs

orrr :: [Bool] -> Bool
orrr = foldr (||) False

maximum :: Ord a => [a] -> a
maximum x = foldr1 max x

minimum :: Ord a => [a] -> a
minimum x = foldr1 min x

reverse :: [a] -> [a]
reverse x = foldl (\ a b -> b:a) [] x

scanrr :: (a -> b -> b) -> b -> [a] -> [b]
scanrr f c [] = [c]
scanrr f d (x:xs) =  f x (head (scanrr f d xs)):(scanrr f d xs)

scanrrr :: (a -> b -> b) -> b -> [a] -> [b]
scanrrr f c x = foldr g [c] x where
    g x y = (f x (head y)):y

--whatwewant :: (a -> b -> b) -> a -> b -> [b]
--whatwewant f x c = [f x c, c]

fac n = product [1..n]

factList :: Integer -> [Integer]
factList n = scanl1 (*) [1..n]

retaindivisible :: Int -> [Int] -> [Int]
retaindivisible x xs = [n | n <- xs, (mod n x)==0 ]

choosetails :: Eq a => [[a]] -> [[a]]
choosetails xs = [tail x | x <- xs, (x /= []) ]