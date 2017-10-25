fredrikkommentar :: String -> String
fredrikkommentar input = "Ja, jag är en väldigt " ++ input ++ " person"

fredriksvar input = putStrLn $ fredrikkommentar input