main = do
    n <- read <$> getLine
    solve n

solve 0 = return ()
solve n = do
    [p,r,f] <- map read . words <$> getLine
    let y = findYears p r f 0
    print y
    solve (n-1)

findYears :: Int -> Int -> Int -> Int -> Int
findYears p r f n = if p > f then n else findYears (p*r) r f (n+1)