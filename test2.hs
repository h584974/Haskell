
rev xs = reva xs []
reva :: [a] -> [a] -> [a]
reva [] acc = acc
reva (x:xs) acc = reva xs acc++[x]


foldll xs = foldla xs 0
foldla :: Num a => [a] -> a -> a
foldla [] s = s
foldla (x:xs) s = foldla xs (s+x)