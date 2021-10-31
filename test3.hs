data By = BRG | OSL | TNH | SVR deriving (Eq)
instance Show By where
    show BRG = "Bergen"
    show OSL = "Oslo"
    show TNH = "Trondheim"
    show SVR = "Stavanger"

data Tre = Leaf Int | Node Tre Int Tre | Emp

list Emp = []
list (Node l v r) = list l ++ [v] ++ list r

find :: Int -> Tre -> Int
find x Emp = 0
find x (Node l v r)
 | x == v = v
 | x < v = find x l
 | otherwise = find x r