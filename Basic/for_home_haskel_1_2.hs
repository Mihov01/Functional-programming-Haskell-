main :: IO()
main = do

    print $ myGcd 13 5 == 1
    print $ myGcd 13 1235 == 13

myGcd :: Int -> Int -> Int
myGcd x y
 | 0 == x = y 
 | 0 == y = x
 | otherwise = myGcd  (min x y) (mod (max x y) (min x y))


