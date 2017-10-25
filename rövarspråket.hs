--module Kul

rovarspraket :: String -> String

rovarspraket         [] = []
rovarspraket (x:resten) = 
        rovar x (head resten) ++ rovarspraket resten

rovar x y =
    if elem x vowels
        then [x] ++ [y] ++ "o" -- ++ [y]
        else [x]

vowels = "aeiyou"    