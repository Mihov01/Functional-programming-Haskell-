main :: IO()
main = do 
    print $ combine [(1, 2)]  == (1, 2)
    print $ combine [(1, 2), (1, 2)]  == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)]  == (377802, 989859)


type Comb = (Int, Int)

combine :: [Comb] -> Comb
combine [] = error "Empty"
combine xs = helper xs (0, 0)
 where 
     helper :: [Comb] -> Comb -> Comb
     helper [] c = c
     helper [x] (0, 0) = x
     helper (x:xs) c 
      | c == (0, 0) = helper xs (min(fst x) (snd x), max (fst x) (snd x) )
      | otherwise = helper xs (fst c * 10 + min (fst x) (snd x), snd c* 10 + max (fst x) (snd x) )