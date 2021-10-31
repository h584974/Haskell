-- Opg B
f = \ x -> x*x
g = \ y -> f (f y)
h y s = y (y s)
k x = h g x

k' :: Num a => a -> a 
k' x = x^16

-- Opg C
s = \ f g x -> f x (g x)
c = \ x y -> x

s' x = x

-- Opg E
isPrefix :: Eq t => [t] -> [t] -> Bool
isPrefix [] ys = True
isPrefix xs [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- Opg F
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] y = []
rem1 (x:xs) y = if x == y then xs else x : rem1 xs y