main = do
    n <- read <$> getLine
    solve n []

solve 0 _ = return ()
solve n quests = do
    ws <- words <$> getLine
    if head ws == "add" then do
        solve (n-1) (quests++[(read . head . tail $ ws, read . last $ ws)])
    else do
        let energy = read . last $ ws :: Int
        (qs,gold) <- doQuests quests energy 0 :: IO ([(Int,Int)],Int)
        print gold
        solve (n-1) qs

doQuest quests 0 goldSum = return (quests,goldSum)
doQuests quests energy goldSum = do
    let (en,gold) = findQuest quests energy (0,0)
    if en == 0 then
        return (quests,goldSum)
    else do
        let qs = rem1 (en,gold) quests
        doQuests qs (energy-en) (goldSum+gold)

rem1 e [] = []
rem1 e (x:rest) = if e == x then rest else x:(rem1 e rest)

findQuest [] _ q = q
findQuest ((a,b):rest) energy (en,gold) = if energy >= a && a > en then findQuest rest energy (a,b) else findQuest rest energy (en,gold)
