testlist = [1,2,3,4,5,6,7,8,9]

takeint :: Int -> [a] -> [a]
takeint 0 x = []
takeint n (x:xs) = x:takeint (n-1) xs 

dropint :: Int -> [a] -> [a]
dropint 0 x = x
dropint n (x:xs) = dropint (n-1) xs

sumint :: [Integer] -> Integer
sumint [] = 0
sumint (x:xs) = x+sumint xs

scansum :: [Integer] -> [Integer]
scansum [] = []
scansum [x] = [x]
scansum (x:y:xs) = x:scansum ((x+y):xs)

diffs :: [Integer] -> [Integer]
diffs [] = []
diffs [x] = [x]
diffs (x:y:xs) = (x-y):diffs (y:xs)

applytointegers :: (Integer -> Integer) -> [Integer] -> [Integer]
applytointegers _ [] = []
applytointegers f (n:ns) = (f n) : applytointegers f ns

negation = map (0-)

divisors p = [ f | f <- [1..p], p `mod`f == 0]

listdivisors = map divisors
