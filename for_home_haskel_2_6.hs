import Data.List
main :: IO()
main = do 
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45



 
sumUnique1 :: [Int] -> [Int] -> Int 
sumUnique1 [] xss = 0
sumUnique1 (x:xs) xss 
 | elem x xss || elem x xs = 0 + sumUnique1 xs ([x] ++ xss)
 | otherwise = x + sumUnique1 xs ([x] ++ xss)

sumUnique :: [[Int]] -> Int
sumUnique [] = 0
sumUnique (xs:xss) = sumUnique1 xs [] + sumUnique xss