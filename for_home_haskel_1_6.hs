main :: IO()
main = do
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
 | x < 0 = error "Negative number"
 | x == 1 = False
 | otherwise = null $ [ y | y <- [2 .. x - 1], mod x y == 0 ]