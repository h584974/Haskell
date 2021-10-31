data Op = Add | Sub | Mul | Div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x >= y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o e1 e2) = [apply o x y | x <- eval e1, y <- eval e2, valid o x y]

-- 9.2
rem1 k [] = []
rem1 k (x:xs) = if k == x then xs else x : rem1 k xs

isChoice :: Eq a => [a] -> [a] -> Bool 
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) (y:ys) = if length (x:xs) > length (y:ys) then False else isChoiceR (x:xs) (y:ys) (length (x:xs)) (length (y:ys))

isChoiceR [] ys nx ny = if ny - (length ys) == nx then True else False
isChoiceR (x:xs) (y:ys) nx ny = isChoiceR (xs) (rem1 x (y:ys)) nx ny

-- 9.2
