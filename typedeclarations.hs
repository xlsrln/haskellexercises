data Anniversary = Birthday String Int Int Int       -- name, year, month, day
                 | Wedding String String Int Int Int -- spouse name 1, spouse name 2, year, month, day

data Date = Date Int Int Int

type AnniversaryBook = [Anniversary]

showdate :: Date -> String
showdate (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

showDate :: Int -> Int -> Int -> String
showDate y m d = show y ++ "-" ++ show m ++ "-" ++ show d

anniversaryDate :: Anniversary -> Date
anniversaryDate (Birthday n y m d) =
    Date y m d
anniversaryDate (Wedding n nn y m d) =
    Date y m d

showAnniversary :: Anniversary -> String
showAnniversary (Birthday name year month day) =
   name ++ " born " ++ showDate year month day
showAnniversary (Wedding name1 name2 year month day) =
   name1 ++ " married " ++ name2 ++ " on " ++ showDate year month day
   
   
   
fstPlusSnd :: (Num a) => (a, a) -> a
fstPlusSnd (x, y) = x + y


scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f c [] = [c]
scanr' f d list@(x:xs) =  f x (head (scanr' f d xs)):(scanr' f d xs)

scanR :: (a -> b -> b) -> b -> [a] -> [b]
scanR f c [] = [c]
scanR f c xxs@(x:xs) = (foldr f c xxs):(scanR f c xs)

scanR' :: (a -> b -> b) -> b -> [a] -> [b]
scanR' f c x = foldr g [c] x where
    g x y = (f x (head y)):y
    
data Foo = Bar | Baz Int

g :: Foo -> Bool
g Bar {} = True
g Baz {} = False