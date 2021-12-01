main :: IO()
main = do
    print $ perfect 1 == False
    print $ perfect 6 == True
    print $ perfect 495 == False
    print $ perfect 33550336 == True 

perfect :: Int -> Bool
perfect x
 | x < 0 = error "x is not natural"
 | x == 1 = False
 | otherwise = (sum $ [ y | y <- [1 .. x -1 ],  mod x y == 0 ]) == x