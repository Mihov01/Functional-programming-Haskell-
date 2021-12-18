main :: IO()
main = do 
 print $ willItFly [1, 4, 2, 3] == True
 print $ willItFly [1, 4, 2, -1, 6] == False
 
willItFly :: [Int] -> Bool
willItFly xs = null $ filter (>= length xs ) (helper xs [])
 where 
     helper :: [Int] -> [Int] -> [Int]
     helper [x] ys = ys
     helper (x:y:xs) ys = helper (y : xs) (ys ++ [abs (y - x)]) 

