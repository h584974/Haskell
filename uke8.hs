-- C
juletre3 :: Int -> Int -> Int -> IO ()
juletre3 a b c = do clr
                    juletreR a (a-1) a 1
                    juletreR b (b-1) ((2*a) + b + 1) 1
                    juletreR c (c-1) ((2*a) + (2*b) + c + 2) 1

juletreR h 0 x y = do goto x y
                      putStrLn (stars h)
juletreR h n x y = do goto x y
                      putStr (stars (h-n))
                      juletreR h (n-1) (x-1) (y+1)

stars n = take (n*2) (concat (repeat "* "))

clr = putStr "\ESC[2J"

goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- D
data FileOrFolder = File Int | Folder [FileOrFolder]

prettyPrint fof = do clr
                     prettyPrintList [fof] 1 1
                     putStrLn ""

prettyPrintList [] x y = return ()

prettyPrintList ((File n):as) x y = do goto x y
                                       putStr ("-File " ++ show n)
                                       prettyPrintList as x (y+1)

prettyPrintList ((Folder l):as) x y = do goto x y
                                         putStr ("-Folder " ++ show (length l))
                                         prettyPrintList l (x+4) (y+1)
                                         prettyPrintList as x (y + (len l) + 1)

len [] = 0
len ((Folder l) : xs) = 1 + len l + len xs
len ((File n) : xs) = 1 + len xs
