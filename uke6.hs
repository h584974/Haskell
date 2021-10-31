-- Opg 1
remg :: [a] -> (a -> Bool) -> [a]
remg [] _ = []
remg (x:xs) f = 
                if f x
                then xs
                else [x]++remg xs f

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs