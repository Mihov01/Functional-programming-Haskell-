import Data.List
main :: IO()
main = do 
    
    print $  squareDigits (-9119) 


makeList :: Int -> [Int]
makeList 0 = []
makeList x = [mod x 10] ++ makeList (div x 10) 

lengthNUm :: Int -> Int 
lengthNUm 0 = 0
lengthNUm x = 1 + lengthNUm (div x 10)


squareDigits :: Int -> Int
squareDigits x
 | x >= 0 =    (squareDigits1 ( map (\ y -> y*y) $ reverse $ (makeList x)) 0 )
 | otherwise = -squareDigits1 ( map (\ y -> y*y) $ reverse $ (makeList (- x))) 0

squareDigits1 :: [Int] ->Int -> Int
squareDigits1 [] res = res
squareDigits1 (x:xs) res= squareDigits1 xs ( res * ( 10 ^ (lengthNUm x)) + x )  