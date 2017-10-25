--module Kul

rovarspraket :: String -> String

rovarspraket         [] = []
rovarspraket (x:resten) = 
        rovar x (head resten) ++ rovarspraket resten

--needs more tweaking, e.g. for double consonants ck,tt, etc

rovar x y =
    if elem x vowels
        then [x] ++ [y] ++ "o" -- ++ [y]
        else [x]

vowels = "aeiyouåäö"    
