import Data.List

main = do
    nums <- sort . map read . words <$> getLine :: IO [Int]
    order <- getLine
    let zipped = zip nums ["A","B","C"]
        ordered = [getNum [char] zipped | char <- order]
    mapM_ (\n -> putStr (show n ++ " ")) ordered

getNum char list = head [n | (n,c) <- list, c == char]