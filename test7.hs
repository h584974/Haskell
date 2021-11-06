l = [(1,2,3),(4,5,6),(7,8,9)]

f l n k p = map (\(a,b,c) -> (a+n,b+k,c+p)) l

new l1 l2 = [(a,b,c) | (a,b) <- l1, c <- l2]