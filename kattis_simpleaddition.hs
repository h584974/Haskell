main = do
    n1 <- read <$> getLine
    n2 <- read <$> getLine
    putStrLn $ show (n1+n2)