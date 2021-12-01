main :: IO()
main = do

    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
   

hasIncDigits :: Int -> Bool
hasIncDigits x -- = if x < 0 then error "Negative number" else helper2 (div x 10) (mod x 10)
 | x < 0 = error "Negative number"
 | otherwise = helper2 (div x 10) (mod x 10)
 where
     helper2 :: Int -> Int ->Bool
     helper2 0 digit = True
     helper2 leftover digit
      | digit < mod leftover 10 = False
      | otherwise = helper2 (div leftover 10) (mod leftover 10)

