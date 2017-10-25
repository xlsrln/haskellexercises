nthelement :: [a] -> Int -> a
nthelement x 0 = head x
nthelement (x:xs) n = nthelement xs (n-1)