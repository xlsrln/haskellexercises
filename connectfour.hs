
-- data Player p = Player1 | Player2

-- nextPlayer Player1 = Player2
-- nextPlayer Player2 = Player1

-- x = Player1
-- o = Player2
nxt "x" = "o"
nxt "o" = "x"

--board stuff
vsize = 5
hsize = 6
emptyrow = replicate hsize []
initialboard = map (replicate vsize) $ replicate hsize "-"

main = do turn "x" initialboard     
        
--main action, asks for input and does the stuff
turn s board = do 
        putStrLn " "
        putStrLn $ "player " ++ s ++ " turn, enter column: "
        
        column <- read <$> getLine :: IO Int
        let newboard = putCoin s column board
        
        putStrLn " "
        printBoard $ map reverse newboard
        case status newboard of
            "-" -> turn (nxt s) newboard
            c -> putStrLn $ "player " ++ c ++ " won"


--main printing function, prints the current board as rows of strings
printBoard :: [[String]] -> IO ()
printBoard board = do
                    let rest = removeRow board
                    print $ printRow board
                    if rest == emptyrow then return ()
                        else printBoard rest

--the hard part, determining if someone won or not
status board  
    | listOr $ map (contains "xxxx") $ checklist board = "x"
    | listOr $ map (contains "oooo") $ checklist board = "o"
    | otherwise                                        = "-"

--which uses these logical operations
listOr :: [Bool] -> Bool
listOr = foldr (||) False

contains [] ys = True
contains xs [] = False
contains (x:xs) (y:ys) = (x == y) && contains xs ys || contains (x:xs) ys

--and the following functions
checklist board = concat [rows board, columns board]

--extract all columns to strings
columns :: [[String]] -> [String]
columns board = map concat board

--extract all rows to strings
rows :: [[String]] -> [String]
rows [] = []
rows board = row board : rows (removeRow board) where
    row board = concat [x | (x:xs) <- board ]

--extract all diagonals to strings
-- aah

--other functions
printRow :: [[String]] -> String
printRow board = concat [x | (x:xs) <- board]

removeRow :: [[String]] -> [[String]]
removeRow board = [xs | (x:xs) <- board]

-- putCoin :: Int -> [[String]] -> [[String]]
putCoin s 1 (col:cols) = putCoinCol s col : cols
putCoin s n (col:cols) = col : putCoin s (n-1) cols

-- putCoinCol :: [String] -> [String]
putCoinCol s ("-":rs) = (s:rs)
putCoinCol s (r:rs) = r : putCoinCol s rs