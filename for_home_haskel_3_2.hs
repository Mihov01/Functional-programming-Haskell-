main :: IO()
main = do 
    print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeFirst :: (Eq a) => a -> [a] ->[a]
removeFirst e xs = helper e xs []
 where 
     helper :: (Eq a) => a -> [a] -> [a] -> [a]
     helper _ [] _ = error "No such element"
     helper e (x: xs) ys 
      | e == x = ys ++ xs
      | otherwise = helper e xs (ys ++ [x])