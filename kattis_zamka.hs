main = do
    ls <- getLines []
    print ls

getLines ls = do
    l <- getLine
    if null l then
        return ls
    else
        getLines (ls++[l])