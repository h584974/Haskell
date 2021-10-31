pali :: Eq a => [a] -> Bool
pali list = list == reverse list

palis :: Eq a => [[a]] -> [[a]]
palis list = [x | x <- list, pali x]

fac n 
 | n < 1 = n
 | n == 1 = 1
 | otherwise = n * fac (n-1)

facA 1 acc = acc
facA n acc = fac (n-1) (acc*n)