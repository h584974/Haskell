-- A
data Ast = V Int | P Ast Ast | M Ast Ast

eval :: Ast -> Int
eval (V n) = n
eval (P a b) = eval a + eval b
eval (M a b) = eval a * eval b

-- B
evalb (V n) = n `mod` 2 == 1
evalb (P a b) = evalb a || evalb b
evalb (M a b) = evalb a && evalb b

-- C
ev :: Ast -> (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> a
ev (V n) v p m = v n
ev (P a b) v p m = p (ev a v p m) (ev b v p m)
ev (M a b) v p m = m (ev a v p m)  (ev b v p m)

vi n = n
pi' a b = a + b
mi a b = a * b

vb n = n `mod` 2 == 1
pb a b = a || b 
mb a b = a && b

vStr n = show n
pStr a b = "(" ++ a ++ " + " ++ b ++ ")"
mStr a b = "(" ++ a ++ " * " ++ b ++ ")"

-- D
data Exmpl = Val Int | Add Exmpl Exmpl

folde :: Exmpl -> (Int -> a) -> (a -> a -> a) -> a
folde (Val n) f _ = f n
folde (Add a b) f g = g (folde a f g) (folde b f g)