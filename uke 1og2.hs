-- opg C
dobb :: [Int] -> [Int]
dobb list = [x + x | x <- list]

-- opg G
addobb :: [Int] -> [Int]
addobb list = list ++ dobb list

-- opg H
pali :: Eq a => [a] -> Bool
pali list = list == reverse list

