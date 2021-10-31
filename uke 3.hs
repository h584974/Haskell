-- Opg D
foo1 :: a -> b -> (a,b)
foo1 x y = (x,y)

foo2 :: a -> b -> (a,b)
foo2 x = \ y -> (x,y)

foo3 :: a -> b -> (a,b)
foo3 = \ x y -> (x,y)

foo4 :: a -> b -> (a,b)
foo4 = \ x -> \ y -> (x,y)

foo5 :: a -> b -> (b,a)
foo5 = \ x -> \ y -> (y,x)

foo6 :: a -> b -> (a,b)
foo6 = \ y -> \ x -> (y,x)

-- Opg E
f1 :: a -> (a,a)
f1 a = (a,a)

f2 :: (a,b) -> a
f2 (a,b) = a

f3 :: (a,b) -> b
f3 (a,b) = b

f4 :: a -> b -> a
f4 a b = a

f5 :: a -> b -> b
f5 a b = b

-- Opg F
f :: Int -> Int -> Int
g :: (Int,Int) -> Int

f x y = if x < y then x else y
g (x,y) = if x < y then x else y

-- alternativ g
g' (x,y) = f x y