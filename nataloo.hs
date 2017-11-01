

data Nataloo = Nataloo Bool Bool

ishunger :: Nataloo -> Bool
ishunger (Nataloo a b) = b

iscute :: Nataloo -> Bool
iscute (Nataloo a b) = a

darlo = Nataloo True True