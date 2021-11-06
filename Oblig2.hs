-- Oblig 2, Oliver

import System.IO
import Data.Char

-- Game data structure
type Pillar = [Int]
type Tower = [Pillar]
data Op = Rem | Add | Keep

-- Auxiliary functions

-- Creates a Stirng with n-numbers of "# " in sequence
ring n = take (2*n - 1) (concat (repeat "# "))

-- Finds the maximum width of a pillar, which is the length of it's largest ring
width pillar = (pillar !! (length pillar - 1)) * 2

clr = putStr "\ESC[2J"

newline = putChar '\n'

goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt x y str = do
    goto x y
    putStr str

-- Writes a String with it's center at the given x value as opposed to from x
centerWrite x y str = writeAt (x - ((length str) `div` 2)) y str

-- Createst a String-representation of a ring with length r, if r = 0 then | is drawn instead
ringStr r = if r == 0 then "|" else ring r

-- Finds the top ring of a pillar
top [] = 0
top (x:xs) = if x > 0 then x else top xs

-- Checks if an element exists in a list
exists _ [] = False
exists a (x:xs) = if a == x then True else exists a xs

-- Finds the shortest list in a list of lists
shortest list = shortest' list (head list) (length (head list))
shortest' [] l _ = l
shortest' (x:xs) l len = if (length x) < len then shortest' xs x (length x) else shortest' xs l len

-- Primary functions

-- Creates a new tower with n rings
newTower :: Int -> Tower
newTower n = [take (n+1) [0..],take (n+1) (repeat 0),take (n+1) (repeat 0)]

-- Reverses a given number of moves on a tower, returns the new state of
-- the tower and the list of moves applied up until that state
reverseMoves :: Tower -> [(Int,Int)] -> Int -> IO (Tower,[(Int,Int)])
reverseMoves tower moves n = do
    let startTower = newTower (length (tower !! 0) - 1)
    if n >= length moves then
        return (startTower,[])
    else do
        let newMoves = take (length moves - n) moves
        newTower <- applyMoves startTower newMoves
        return (newTower,newMoves)

-- Recursively applies all moves from a list of moves to a tower
applyMoves :: Tower -> [(Int,Int)] -> IO Tower
applyMoves tower [] = return tower
applyMoves tower (x:xs) = do
    tempTower <- updateTower tower x
    t <- applyMoves tempTower xs
    return t

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
validPillar [x] = x >= 0
validPillar (x:y:ys) = x <= y && x >= 0 && validPillar (y:ys)

-- Updates the tower according to the given move
updateTower :: Tower -> (Int,Int) -> IO Tower
updateTower tower (a,b) = do
    if a == b then
        return [[-1],[-1],[-1]]
    else do
        let ring = top (tower !! a)
        -- This may be more complicated than it had to be...
        -- For each pillar in the tower it zips the pillar, the tail of the pillar + (-1) to use for checking, and 0 + the inital array of the tower
        -- Then on each element of the pillar it uses the function update to update the value according to the operators Add, Rem and Keep
        return [[update r h t ring (if a == 0 then Rem else if b == 0 then Add else Keep) | (r,t,h) <- zip3 (tower !! 0) (tail (tower !! 0) ++ [-1]) ([0] ++ init (tower !! 0))], [update r h t ring (if a == 1 then Rem else if b == 1 then Add else Keep) | (r,t,h) <- zip3 (tower !! 1) (tail (tower !! 1) ++ [-1]) ([0] ++ init (tower !! 1))], [update r h t ring (if a == 2 then Rem else if b == 2 then Add else Keep) | (r,t,h) <- zip3 (tower !! 2) (tail (tower !! 2) ++ [-1]) ([0] ++ init (tower !! 2))]] :: IO Tower

-- Updates a ring from a pillar according to the given operator
update :: Int -> Int -> Int -> Int -> Op -> Int
update r _ _ _ Keep = r
update r _ t ring Add = if r == 0 && (t > 0 || t == -1) then ring else r
update r h t _ Rem = if r > 0 && h == 0 then 0 else if r == 0 && t == -1 then -1 else r

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

-- Returns the optimal next move to make
bestMove :: Tower -> IO (Int,Int)
bestMove tower = do
    bestMoves <- solve tower [] []
    return (head bestMoves)

-- Recursively solves a given tower from it's current layout in all different possible ways, 
-- and returns the solution with the least nummber of moves. Seems to work, but is
-- hopelessly slow for any towers with > 3 rings
solve :: Tower -> [(Int,Int)] -> [Tower] -> IO [(Int,Int)]
solve tower moves towers = do
    if finished tower then
        return moves
    else do
        let prevMove = (if null moves then (-1,-1) else last moves)
            ms =[(a,b) | (a,b) <- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)], not (a == snd prevMove)]
        ts <- mapM (\m -> updateTower tower m) ms
        let towersMoves = zip ts ms
            validTowersMoves = [(t,m) | (t,m) <- towersMoves, valid t, not (exists t towers)]
        solutions <- mapM (\(t,m) -> solve t (moves ++ [m]) (towers ++ [tower])) validTowersMoves
        let validSolutions = [s | s <- solutions, not (null s)]
        if null validSolutions then
            return []
        else 
            return (shortest validSolutions)

-- Starts the program and shows the start menu
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
hanoi tower moves prompt = do
    clr
    putTower tower
    newline
    newline
    if finished tower then do
        putStrLn ("Congratulations, you won using " ++ show (length moves) ++ " moves at level " ++ show (length (tower !! 0) - 1))
        putStrLn "Press ENTER to continue"
        getChar
        main
    else do
        putStrLn ("Number of moves: " ++ show (length moves) ++ "   " ++ prompt) 
        putStrLn "Commands: q - quit, <from pillar> <to pillar> - move"
        putStrLn "z <num moves> - regret moves, h - Show next optimal move"
        line <- getLine
        if null line then
            hanoi tower moves "ERROR: No command given"
        else do
            let com1 = head (words line)
                com2 = head (tail (words line))
            if com1 == "q" then do
                clr
                writeAt 1 1 "Quit game"
                newline
            else if not (null com1) && all isDigit com1 then do
                let p1 = read com1
                if p1 > 0 && p1 < 4 then do
                    if not (null com2) && all isDigit com2 then do
                        let p2 = read com2
                            move = (p1-1,p2-1)
                        newTower <- updateTower tower move
                        if valid newTower then
                            hanoi newTower (moves ++ [move]) ""
                        else
                            hanoi tower moves "ERROR: Invalid move"
                    else do
                        hanoi tower moves "ERROR: Second must be a pillar between 1-3"
                else do
                    hanoi tower moves "ERROR: First number must be a pillar between 1-3"
            else if com1 == "z" then do
                if not (null com2) && all isDigit com2 then do
                    let n = read com2
                    (newTower,newMoves) <- reverseMoves tower moves n
                    hanoi newTower newMoves ""
                else do
                    hanoi tower moves "ERROR: Not a valid number, z <number of moves>"
            else if com1 == "h" then do
                (a,b) <- bestMove tower
                hanoi tower moves ("Optimal move: (" ++ show (a+1) ++ "," ++ show (b+1) ++ ")")
            else do
                hanoi tower moves "Error: Not a valid command"

--solve' :: [(Tower,[(Int,Int)])] -> IO [(Tower,[(Int,Int)])]
--solve' tms = do
--    let nms = [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
--        tmsn = [ (t,ms,n) | (t,ms) <- tms, n <- nms, not (fst n == snd (if null ms then (-1,-1) else last ms)) ]    
--   nts <- mapM (\(t,ms,n) -> update t n) tmsn
--    let ms = [ms++[n] | (t,ms,n) <- tmsn]
--       ntms = zip nts ms
--        vtms = [(t,ms) | (t,ms) <- ntms, valid t, not (towerExists t tms)]
--    if null vtms then 
--        return tms
--    else 
--        solve' vtms 

--bestMove' :: Tower -> IO (Int,Int)
--bestMove' tower = do
--    tms <- solve' [(tower,[])]
--    let fms = collectFinishedMovesets tms
--        bestMoveset = shortest fms
--        nextBestMove = head bestMoveset
--    return nextBestMove

--towerExists :: Tower -> [(Tower,[(Int,Int)])] -> Bool
--towerExists _ [] = False
--towerExists t1 ((t2,ms):xs) = t1 == t2 || towerExists t1 xs

--collectFinishedMovesets :: [(Tower,[(Int,Int)])] -> [[(Int,Int)]]
--collectFinishedMovesets tms = [ms | (t,ms) <- tms, finished t]
