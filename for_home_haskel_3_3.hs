main :: IO()
main = do 
    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll e xs = helper e xs []
 where
     helper :: (Eq a) => a -> [a] -> [a] -> [a]
     helper _ [] ys = ys 
     helper e (x:xs) ys 
      | e == x = helper e xs ys
      | otherwise = helper e xs (ys ++ [x])

removeAllHOF :: (Eq a) => a -> [a] -> [a]
removeAllHOF e  = filter (/= e) 
