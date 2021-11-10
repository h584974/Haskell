import Data.List

main = interact (print . solve . readInput)

readInput input = splitOn ";" input

solve input = show . solve' $ input 0
solve' :: [String] -> Int
solve' [] sum = sum
solve' (x:xs) sum = if length x == 1 then solve' xs (sum+1) else solve' xs (sum + (read . last $ x) (read . head $ x))

splitOn x ll = splitOn' x ll "" []
splitOn' x [] acc list = if null acc then list else (list++[acc])
splitOn' x (a:as) acc list = if a == x then splitOn' x as [] (list++[acc]) else splitOn' x as (acc++a) list