import Data.Char
main :: IO()
main = do 
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
   


prime :: Int -> Bool 
prime n =  n > 1 && (null $ filter (\ d -> mod n d == 0) [2 .. n - 1])

inside :: Int -> Int -> Bool
inside x n = elem (intToDigit x)  (show n)

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum (take n [x | x <- [2 ..], prime x && inside d x])



