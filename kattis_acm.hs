main = do
    ls <- getLines []
    s <- solve ls
    print s

getLines ls = do
    l <- getLine
    if null l then
        return ls
    else
        getLines (ls++[l])

solve :: [String] -> IO String
solve l = evaluate l 0 [] 0

evaluate :: [String] -> Int -> [String] -> Int -> IO String
evaluate [x] numRight _ scoreSum = return (show numRight ++ " " ++ show scoreSum)
evaluate (x:xs) numRight wrongSubs scoreSum = do
    let ws = words x
        mins = (read $ ws !! 0) :: Int
        prob = show . head $ ws !! 1
        answer = ws !! 2
    if answer == "wrong" then do
        evaluate xs numRight (wrongSubs ++ [prob]) scoreSum
    else do
        let numWrong = countWrong prob wrongSubs
        evaluate xs (numRight + 1) wrongSubs (scoreSum + mins + (numWrong*20))

countWrong prob wrongSubs = countWrong' prob wrongSubs 0
countWrong' _ [] sum = sum
countWrong' prob (x:xs) sum = if x == prob then countWrong' prob xs (sum+1) else countWrong' prob xs sum

-- Example inputs
str1 = "3 E right\n10 A wrong\n30 C wrong\n50 B wrong\n100 A wrong\n200 A right\n250 C wrong\n300 D right\n-1"

str2 = "7 H right\n15 B wrong\n30 E wrong\n35 E right\n80 B wrong\n80 B right\n100 D wrong\n100 C wrong\n300 C right\n300 D wrong\n-1"