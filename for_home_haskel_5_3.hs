main::IO()
main = do 
    print $ dominates (+4) (*2) [1..4] == True
    print $ dominates (+4) (*2) [1..5] == False 
    print $ test 1071225
    print $ test 40539911473216
    
dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = length xs == length  [x | x <- xs , abs (f x) >=  abs (g x) ]

test :: Integer -> Int
test  xs = let l = takeWhile (\ x -> (sum $ map ( ^ 3) [ y | y <- [1 .. x-1]]) < xs ) [1 .. ] in if ((sum $ map ( ^ 3) l) == xs) then length l else -1