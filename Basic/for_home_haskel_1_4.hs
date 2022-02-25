main :: IO()
main = do

    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
   

sumDivisors :: Int -> Int
sumDivisors x -- = if x < 0 then error "Negative number" else helper1 0 x x
 | x < 0 = error "Negative number"
 | otherwise = helper1 0 x x
 where
     helper1 :: Int -> Int -> Int -> Int
     helper1 result 1 x = result + 1
     helper1 result leftover num
      | (mod num leftover) == 0 = helper1 (result + leftover) (leftover - 1) num
      | otherwise = helper1 result (leftover - 1) num

areAmicable :: Int -> Int -> Bool
areAmicable x y = sumDivisors x == sumDivisors y

