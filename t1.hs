import System.IO

exx n = do putStrLn "Gi kommando"
           c <- getLine
           let com = words c
           if head com == "q" then return ()
           else if head com == "cc" then do clr
                                            exx n
           else if head com == "cs" then do cs
                                            exx n
           else if head com == "ny" then do let x = read (head (tail com)) :: Int
                                                y = read (last com) :: Int
                                                s = "X"
                                            goto (2 * x + 1) (y + 1)
                                            putStr s
                                            goto 0 (n+2)
                                            cs
                                            exx n
           else do print "Ukjent kommando"
                   exx n

brett :: Int -> IO ()
brett n = do clr
             writeTop n
             writeRows 1 n
             cs
             exx n

clr = putStr "\ESC[2J"

cs = putStr "\ESC[0J"

writeTop n = writeAt (3,0) (concat [show (mod i 10) ++ " " | i <- [1..n]])

writeAt (x,y) str = do goto x y
                       putStr str
                       putStrLn ""

goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeRows a n = if a > n then return ()
                else do writeRow a n
                        writeRows (a+1) n


writeRow i n = putStrLn ( show (mod i 10) ++ concat (take n (repeat " .") ) )