
-- opg A
prod :: Num a => [a] -> a
prod [] = 0
prod (x:xs)
 | length (x:xs) > 1 = x * (prod xs)
 | otherwise = x
  
n = a `div` length xs
 where
  a = 10
  xs = [1,2,3,4,5]

last2 list = head (reverse list)

-- opg C
dobb :: [Int] -> [Int]
dobb list = [x + x | x <- list]

-- opg D
fire :: [Int] -> [Int]
fire list = [x * 4 | x <- list]

-- opg G
addobb :: [Int] -> [Int]
addobb list = list ++ dobb list

-- opg H
pali, pali' :: Eq a => [a] -> Bool
pali list = list == reverse list

pali' list = list == rev list
rev [] = []
rev (x:xs) = rev xs ++ [x]
