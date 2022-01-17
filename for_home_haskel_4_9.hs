main :: IO()
main = do 

    print $ g [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998


g :: [Double] -> (Double -> Int -> Double)
g xs = (\ x y -> foldl (\ acc z -> acc * (x-z)) 1 (take y xs))