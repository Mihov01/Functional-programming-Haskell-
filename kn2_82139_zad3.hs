main :: IO()
main = do 

    print $ matching  "[][]"


test :: String -> Int -> [(Char, Int)] ->[(Char, Int)]
test [] _ res = res 
test (x:xs) i res = test xs (i +1) (res ++ [(x,i)])


matching :: String  -> [(Int , Int)]
matching xs  =  zip (map (\ (_ , i) -> i) $ filter (\ (x,_) -> x == '[') $ test xs 0 []) (map (\ (_ , i) -> i) $ filter (\ (x,_) -> x == ']') $ test xs 0 []) 

