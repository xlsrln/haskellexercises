
--fakeIf :: Bool

g :: Bool -> Int
g x = if x==True then 1 else 0 



--if' is if but with cases

if' :: Bool -> a -> a -> a
if' b y z = case b of
                    True -> y
                    False -> z
                    
                    
g' :: Bool -> Int
g' x = (if' (x==True) 1 0)  