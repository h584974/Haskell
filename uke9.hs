import Data.Char

-- Nim
type Board = [Int]

next :: Int -> Int
next 1 = 2
next 2 = 1

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do 
    putStr prompt
    s <- getLine
    newline
    if not (null s) && all isDigit s then return (read s)
    else do 
        putStrLn "ERROR: Invalid row"
        getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
    newline
    putBoard' board
    if finished board then do
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do
        newline
        putStrLn ("Player: " ++ show player)
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove : "
        if valid board row num then play (move board row num) (next player)
        else do
            newline
            putStrLn "ERROR: Invalid move"
            play board player

nim :: IO ()
nim = play' initial 1             

-- A
gRep :: (t -> Bool) -> t -> [t] -> [t]
gRep f y [] = []
gRep f y (x:xs) = if f x
                  then x : (gRep f y xs)
                  else y : (gRep f y xs)

gRep' :: (t -> Bool) -> t -> [t] -> [t]
gRep' f y xs = let bools = map f xs 
                   zipped = zip xs bools
               in
               [if b then x else y | (x,b) <- zipped]

-- B
putBoard' :: Board -> IO ()
putBoard' board = putBoard'' board 1 (length board)

putBoard'' :: Board -> Int -> Int -> IO ()
putBoard'' [] _ _ = return ()
putBoard'' (x:xs) n len = do
    let a = 10 `mod` n
    putStr (concat (replicate a " "))
    putRow n x 
    putBoard'' xs (n+1) len

-- D
play' :: Board -> Int -> IO ()
play' board player = play'' board player []

play'' :: Board -> Int -> [(Int,Int)] -> IO ()
play'' board player moves = do
    newline
    putBoard' board
    newline
    if finished board then do
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else do
        putStrLn ("Player: " ++ show player)
        row <- getDigit "Enter a row number: "
        if row == 0 then do
            if null moves then do 
                putStrLn "Board is already at starting point"
                play'' board player moves
            else
                play'' (moveBack board (last moves)) (next player) (take (length moves - 1) moves)
        else if row == -1 then 
            putStrLn "Game has ended"
        else do
            num <- getDigit "Stars to remove : "
            if valid board row num then do
                play'' (move board row num) (next player) (moves ++ [(row, num)])
            else do
                newline
                putStrLn "ERROR: Invalid move"
                play'' board player moves

moveBack :: Board -> (Int,Int) -> Board
moveBack board (row, num) = (take (row-1) board) ++ [(board !! (row-1) + num)] ++ (drop row board)
