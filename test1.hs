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