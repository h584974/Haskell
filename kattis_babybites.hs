main = do
    getLine
    s <- solve . words <$> getLine
    putStrLn s

solve ws = solve' ws 1
solve' [] _ = "makes sense"
solve' (x:rest) n = if x == "mumble" || read x == n then solve' rest (n+1) else "something is fishy"