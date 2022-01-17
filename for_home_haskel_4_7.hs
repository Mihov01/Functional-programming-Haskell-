main :: IO()
main = do 



    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2 


getOddCompositionValue :: [(Int->Int)] -> Int ->Int
getOddCompositionValue xs x = let functions = helper xs 0 in foldl (\ acc f -> f acc) x functions 
 where 
     helper :: [(Int->Int)] -> Int -> [(Int->Int)] 
     helper []  _ = []
     helper (x : xs) index 
      | even index = helper xs (index +1)
      | otherwise = x : (helper xs) (index +1)