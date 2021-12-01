main :: IO()
main = do
    
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5
    print $ myGcd 13 5 == 1
    print $ myGcd 13 1235 == 13
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False
    --print $ isPalindrome (-654)
    --print $ sumDivisors 3
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
    -- print $ cntDivisors 6
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True
    print $ perfect 1 == False
    print $ perfect 6 == True
    print $ perfect 495 == False
   -- print $ perfect 33550336 == True закоментирал съм го защото много време иска да го  сметне
    --print $ isPrimeG (-6)
sqAvg :: Int -> Int -> Double
sqAvg x y = (fromIntegral $ ((x * x) + (y * y))) / 2

myGcd :: Int -> Int -> Int
myGcd x y
 | 0 == x = y 
 | 0 == y = x
 | otherwise = myGcd  (min x y) (mod (max x y) (min x y))

rev :: Int -> Int
rev x = helper 0 x
 where
     helper :: Int -> Int -> Int
     helper result leftover = if (leftover == 0) then result else helper (result * 10 + mod leftover 10) (div leftover 10)
isPalindrome :: Int -> Bool
isPalindrome x = if x < 0 then error "Negative number" else x == rev x

sumDivisors :: Int -> Int
sumDivisors x = if x < 0 then error "Negative number" else helper1 0 x x
 where
     helper1 :: Int -> Int -> Int -> Int
     helper1 result leftover num
      | leftover == 1 = result + 1
      | (mod num leftover) == 0 = helper1 (result + leftover) (leftover - 1) num
      | otherwise = helper1 result (leftover - 1) num

areAmicable :: Int -> Int -> Bool
areAmicable x y = if (sumDivisors x == sumDivisors y) then True else False

hasIncDigits :: Int -> Bool
hasIncDigits x = if x < 0 then error "Negative number" else helper2 (div x 10) (mod x 10)
 where
     helper2 :: Int -> Int ->Bool
     helper2 leftover digit
      | digit < mod leftover 10 = False
      | leftover == 0 = True
      | otherwise = helper2 (div leftover 10) (mod leftover 10)
  
cntDivisors :: Int -> Int
cntDivisors x = helper3 0 x x
 where 
     helper3 :: Int -> Int -> Int -> Int
     helper3 cnt leftover num
      | leftover == 0 = cnt
      | mod num leftover == 0 = helper3 (cnt + 1) (leftover -1) num
      | otherwise = helper3 cnt (leftover - 1) num 
isPrimeG :: Int -> Bool
isPrimeG x 
 | x < 0 = error "Negative number"
 | cntDivisors x == 2 = True
 | otherwise = False

isPrimeLC :: Int -> Bool
isPrimeLC x = if x < 0 then error "Negative number" else null $ [ x | x <- [x], cntDivisors x /= 2 ]

perfect :: Int -> Bool
perfect x = if x < 0 then error "x is not natural" else null $ [ x | x <- [x], sumDivisors x - x /= x ]