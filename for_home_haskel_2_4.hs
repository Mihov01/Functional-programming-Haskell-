import Data.List
main :: IO()
main = do 
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

prime :: Int -> Bool 
prime n =  n > 1 && (null $ filter (\ d -> mod n d == 0) [2 .. n - 1])

primesInRange :: Int -> Int -> [Int]
primesInRange x y = filter (\ x -> prime x ) [min x y .. max x y]