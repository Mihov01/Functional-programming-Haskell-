import Data.List
main :: IO()
main = do 

    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2 == [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 4 == [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10]]


subLists :: [a] -> Int -> [[a]]
subLists xs k = helper xs k []
 where 
     helper :: [a] -> Int -> [[a]] -> [[a]]
     helper [] _ ys = ys
     helper xs k ys = helper (drop k xs ) k (ys ++ [(take k xs)])
