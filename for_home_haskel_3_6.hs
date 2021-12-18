import Data.List
main :: IO()
main = do 
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
    print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 6] == (False, 0)
    print $ isImage [1, 2] [-1, -2] == (False, 0)
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 4] == (False, 0)
    

type Image = (Bool,Int)

findImage :: [Int] -> [Int] -> [Int] -> [Int]
findImage [] [] res = res
findImage (x:xs) (y:ys) res = findImage xs ys (res ++ [(y - x)])

isImage :: [Int] -> [Int] -> Image
isImage xs ys = helper (nub (findImage xs ys []))
 where 
     helper :: [Int] -> Image
     helper (x : xs)
      | null xs = (True, x)
      | otherwise = (False, 0)