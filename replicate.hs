
elementreplicator :: Int -> a -> [a]
elementreplicator 0 x = []
elementreplicator n x = x:(elementreplicator (n-1) x)