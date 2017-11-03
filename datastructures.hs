data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

axel = Branch anders tiina

anders = Branch elisabeth karlaxel
tiina = Branch olli auli

olli = Leaf "olli"
auli = Leaf "auli"
elisabeth = Leaf "farmor"
karlaxel = Leaf "farfar"
