l = [(1,2,3),(4,5,6),(7,8,9)]

f l n k p = map (\(a,b,c) -> (a+n,b+k,c+p)) l

new l1 l2 = [(a,b,c) | (a,b) <- l1, c <- l2]

splitOn x ll = splitOn' x ll "" []
splitOn' x [] acc list = if null acc then list else (list++[acc])
splitOn' x (a:as) acc list = if a == x then splitOn' x as [] (list++[acc]) else splitOn' x as (acc++a) list