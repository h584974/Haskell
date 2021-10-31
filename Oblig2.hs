-- Oblig2, Oliver

import System.IO
import Data.Char

-- Game data structure
type Pillar = [Int]
type Tower = [Pillar]

-- Auxiliary functions
ring n = take (2*n - 1) (concat (repeat "# "))

width pillar = (pillar !! (length pillar - 1)) * 2

clr = putStr "\ESC[2J"

newline = putChar '\n'

goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt x y str = do
    goto x y
    putStr str

centerWrite x y str = writeAt (x - ((length str) `div` 2)) y str

ringStr r = if r == 0 then "|" else ring r

remove [] = []
remove [x] = if x > 0 then [0] else [-1]
remove (x:xs) = if x > 0 then [0] ++ xs else x ++ remove xs

add [] = []
add [x] y = [y]
add (x:xs) y =

-- Primary functions

-- Creates a new tower with n rings
newTower :: Int -> Tower
newTower n = [take (n+1) [0..],take (n+1) (repeat 0),take (n+1) (repeat 0)] :: Tower

-- Returns the maximum width of a tower
maxWidth :: Tower -> IO Int
maxWidth tower = do
    let p1 = tower !! 0
        p2 = tower !! 1
        p3 = tower !! 2
        w1 = width p1
        w2 = width p2
        w3 = width p3
        w = max (max w1 w2) w3
    return w

-- Determines if the game has been won or not
finished :: Tower -> Bool
finished tower = all (==0) (tower !! 0) && all (==0) (tower !! 1) && validPillar (tower !! 2)

-- Determines if a tower has valid structure or not
valid :: Tower -> Bool
valid tower = validPillar (tower !! 0) && validPillar (tower !! 1) && validPillar (tower !! 2)

-- Determines if a pillar has valid structure or not
validPillar :: Pillar -> Bool
validPillar [] = True
validPillar [x] = True
validPillar xs = head xs < head (tail xs) && head xs >= 0 && validPillar (tail xs)

-- Updates the tower according to the given move
updateTower :: Tower -> (Int,Int) -> Tower
updateTower tower (a,b) = do
    let p01 = tower !! a
        p02 = tower !! b
        p3 = other tower p01 p02

-- Writes tower on screen
putTower :: Tower -> IO ()
putTower tower = do
    w <- maxWidth tower
    putTower' tower 1 w

-- Recursively writes each next line of the tower
putTower' :: Tower -> Int -> Int -> IO ()
putTower' [[],_,_] _ _ = return ()
putTower' [(x:xs),(a:as),(z:zs)] y w = do
    let hw = (w + 1) `div` 2
        x1 = hw
        x2 = (hw + w + 1)
        x3 = (hw + 2*w + 2)
    centerWrite x1 y (ringStr x)
    centerWrite x2 y (ringStr a)
    centerWrite x3 y (ringStr z)
    putTower' [xs,as,zs] (y+1) w

-- Starts program
main :: IO ()
main = do
    putStrLn "Start a new game with b <Number of rings>, or quit with q"
    line <- getLine
    let com1 = head (words line)
        com2 = head (tail (words line))
    if com1 == "b" then do
        if not (null com2) && all isDigit com2 then do
            hanoi (newTower (read com2)) [] ""
        else do
            putStrLn "ERROR: Not a valid number of rings"
            main
    else if com1 == "q" then do
        clr
        writeAt 1 1 "Quit game"
        newline
    else do
        putStr "ERROR: Not a valid command"
        main

-- Controls the game
hanoi :: Tower -> [(Int,Int)] -> String -> IO ()
hanoi tower moves error = do
    clr
    putTower tower
    newline
    newline
    putStrLn ("Number of moves: " ++ show (length moves) ++ "   " ++ error) 
    line <- getLine
    let com1 = head (words line)
        com2 = head (tail (words line))
    if com1 == "q" then do
        clr
        writeAt 1 1 "Quit game"
        newline
    else if not (null com1) && all isDigit com1 then do
        let p = read com1
        if p > 0 && p < 4 then do
            if not (null com2) && all isDigit com2 then do
                return () -- TODO
            else do
                hanoi tower moves "ERROR: Second -> Must choose a pillar between 1, 2, 3"
        else do
            hanoi tower moves "ERROR: First -> Must choose a pillar between 1, 2, 3"
    else if com1 == "z" then do
        if not (null com2) && all isDigit com2 then do
            let x = read com2
            if x >= length moves then do
                main
            else do
                return () -- TODO
        else do
            hanoi tower moves "ERROR: Not a valid number, z <number of moves>"
    else if com1 == "h" then do
        hanoi tower moves "h" -- TODO
    else do
        hanoi tower moves "Error: Not a valid command"




    
