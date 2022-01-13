rep n x = repa n x []
repa :: Num a => Eq a => a -> b -> [b] -> [b]
repa 0 x akk = akk
repa n x akk = repa (n-1) x (x:akk)

exists :: Eq a => a -> [a] -> Bool
exists x [] = False
exists x (y:ys) = if x == y then True else exists x ys

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fib' n = go 0 1 n
go a b 0 = a
go a b n = go b (a+b) (n-1)

qsort []     = []
qsort (x:xs) = let smaller = [e | e <- xs, e <= x]
                   larger = [e | e <- xs, e > x]
               in qsort smaller ++ [x] ++ qsort larger

qsort' xs = if null xs then [] else qsort [e | e <- xs, e <= head xs] ++ [head xs] ++ [e | e <- xs, e > head xs]

fac 1 = 1
fac n = n * fac(n-1)

fac' n = go' n 1
go' 1 acc = acc
go' n acc = go' (n-1) (acc*n)