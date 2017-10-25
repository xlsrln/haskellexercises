import Data.List
import Control.Arrow ((&&&))

rle1 :: Eq a => [a] -> [(a, Int)]
rle1 x = map (head &&& length) (group x)

--repl (a, x) = replicate x a

rle2 :: Eq a => [(a, Int)] -> [a]
rle2 x = concat $ map (\ (a,x) -> replicate x a) x    