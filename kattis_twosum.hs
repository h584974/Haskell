main = interact (writeOutput . solve . readInput)

readInput = (map read) . words
solve[a,b] = [a+b]
writeOutput = unlines . (map show)

-- other method
main' = do
    nums <- (map read) . words <$> getLine
    let out = add (head nums) (last nums)
    print out

add a b = a+b

-- test method
main'' = do
    nums <- (map read) . words . head <$> getLines []
    let out = add (head nums) (last nums)
    print out

getLines ls = do
    l <- getLine
    if null l then
        return ls
    else
        getLines (ls++[l])