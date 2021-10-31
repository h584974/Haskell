locate :: String -> String -> [(Int,Int)]
locate [] ys = []
locate ys [] = []
locate (x:xs) (y:ys) = locateA (x:xs) (y:ys) [] 0

locateA :: String -> String -> [(Int,Int)] -> Int -> [(Int,Int)]
locateA (x:xs) [] acc n = acc
locateA (x:xs) (y:ys) acc n = if isPrefix (x:xs) (y:ys)
                              then locateA (x:xs) ys (acc ++ [(n,n + length (x:xs))]) (n+1)
                              else locateA (x:xs) ys acc (n+1)

locat' :: String -> String -> [(Int,Int)]
locat' s1 s2 = locat s1 s2 0

locat :: String -> String -> Int -> [(Int,Int)]
locat [] _ _ = []
locat _ [] _ = []
locat (x:xs) (y:ys) n = if isPrefix (x:xs) (y:ys) then r ++ [(n,n + length (x:xs))] else r where r = locat (x:xs) ys (n+1)

isPrefix :: String -> String -> Bool 
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys