module Main where
import Control.Monad



bunnyexample xs = 
    [xs] >>= (replicate 3)
    

bunnyseq n = concat $ forM [1..n] (\x -> bunnyexample "bunny")


main = do print $ bunnyseq 7