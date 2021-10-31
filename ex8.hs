-- A
trekant :: Int -> IO ()
trekant h = trekantR h (h-1)

trekantR h 0 = putStrLn (stars h)
trekantR h n = do putStrLn (stars (h-n))
                  trekantR h (n-1)

stars n = take (n*2) (concat (repeat "* "))

-- B
juletre :: Int -> IO ()
juletre h = do clr
               juletreR h (h-1) h 1

juletreR h 0 x y = do goto x y
                      putStrLn (stars h)

juletreR h n x y = do goto x y
                      putStr (stars (h-n))
                      juletreR h (n-1) (x-1) (y+1)

-- C
juletre3 :: Int -> Int -> Int -> IO ()
juletre3 a b c = do clr
                    juletreR a (a-1) a 1
                    juletreR b (b-1) ((2*a) + b + 1) 1
                    juletreR c (c-1) ((2*a) + (2*b) + c + 2) 1

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

clr = putStr "\ESC[2J"

goto x y = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

f = (Folder [Folder [File 5, Folder [File 1, File 1, File 2] ], File 3, Folder [], File 4])

-- E 1
fp a f = prettyPrint f
fr a (File n) = putStrLn ("This is File " ++ (show n))
fa a = a+1

trav :: (a -> FileOrFolder -> IO ()) -> (a -> FileOrFolder -> IO ()) -> (a -> a) -> a -> FileOrFolder -> IO ()
trav fi fo f a (Folder l) = fo a (Folder l)
trav fi fo f a (File n) = fi a (File n)