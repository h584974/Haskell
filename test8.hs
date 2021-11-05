fac 1 = 1
fac n = n * fac (n-1)

fac' n = go n 1
go 1 acc = acc
go n acc = go (n-1) (acc*n)

-------------------------------

fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fib' n = go' n (0,1)
go' 1 (a,b) = a
go' 2 (a,b) = b
go' n (a,b) = go' (n-1) (b,a+b)
