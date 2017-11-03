import Data.List (inits)
import Data.Tuple (swap)
-- import Control.Arrow (***)

fliptest = do 
            print (flip (-) 1 2)
            print ((-) 1 2)
          

myInits :: [a] -> [[a]]
myInits = map reverse . scanl (flip (:)) []

initstest = do
        print $ myInits [1,2,3,4]
        print $ inits [1,2,3,4]
        
functiontest = do
        print $ map ($2) [(+1), (+2)]
        print $ uncurry (-) (2,1)
        uncurry ($) (print, "wow")
        print $ id "hej"
        print $ const "*" "hej"
        

uncurry' f (x, y) = f x y 
curry' f x y = f (x, y)

currytest = do
        print $ curry fst 1 2
        print $ uncurry const (1,2)
        print $ swap (1,2)
        print $ curry swap 1 2


foldL f y xs = foldr (flip f) y (reverse xs)

coolFoldl f y xs = foldr (flip (.)) id (map (flip f) xs) y

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (a -> b -> a) -> a -> [b] -> a

--illustrate brackets and indents
doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num of
    LT -> do putStrLn "Too low!"
             doGuessing num
    GT -> do putStrLn "Too high!"
             doGuessing num
    EQ -> putStrLn "You Win!"

doGuessing2 num = do {putStrLn "Enter your guess:"; guess <- getLine; case compare (read guess) num of {LT -> do {putStrLn "Too low!"; doGuessing num}; GT -> do {putStrLn "Too high!"; doGuessing num};EQ -> putStrLn "You Win!"}}


-- doGuessing2 num = do { putStrLn "Enter your guess:"; guess <- getLine; case compare (read guess) num of {LT -> do { putStrLn "Too low!"; doGuessing num; }; GT -> do { putStrLn "Too high!"; doGuessing num }; EQ -> putStrLn "You Win!"}