-- Opg C
s = \ f g x -> f x (g x)
k = \ x y -> x

-- kall av s k k x returnerer alltid x
s' k k' x = x

-- Opg F
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] y = []
rem1 (x:xs) y = if x == y then xs else x : rem1 xs y