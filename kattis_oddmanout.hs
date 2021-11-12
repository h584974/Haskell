main = do
    n <- read <$> getLine
    inputs <- readInputs n
    solvedInputs <- mapM solve inputs
    mapM_ (\(n,s) -> putStrLn ("Case #" ++ show n ++ ": " ++ s)) $ zip [1..] solvedInputs

readInputs n = readInputs' n []
readInputs' 0 inputs = return inputs
readInputs' n inputs = do
    getLine
    s <- getLine
    readInputs' (n-1) (inputs++[s])

solve :: String -> IO String
solve s = do
    let ws = words s
        el = [e | e <- ws, (count e ws) == 1]
    return (head el)
        
count e l = length [a | a <- l, a == e]

