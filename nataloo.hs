data Nataloo = Nataloo 
    { name :: String, 
      ishungry :: Bool, 
      iscute :: Bool, 
      isSmart :: Bool}

test = do
    let darlo = Nataloo "ntloo" True True True
    print $ ishungry darlo
    putStrLn $ name darlo