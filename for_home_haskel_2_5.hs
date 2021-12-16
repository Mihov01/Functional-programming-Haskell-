main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6

reverseOrdSuff :: Int -> Int
reverseOrdSuff x =  read (helper (reverse $ show x) [])
 where 
    helper :: [Char] -> [Char]-> [Char]    
    helper [] current =  current
    helper (x:y:xs) current 
     | x >= y =  current ++ [x]
     | otherwise = helper (y:xs) ( current ++ [x])