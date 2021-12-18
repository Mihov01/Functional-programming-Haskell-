import Data.List
main::IO()
main = do 

    print $ isSorted [-5, -5, -6] == True
    print $ isSorted [-5, -5, -4] == True
    print $ isSorted [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSorted [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSorted [-100, -99, -99, -99] == True
    print $ isSorted [-100, -99, -99, -99, 100] == True
    print $ isSorted [100, 101, -102] == False
    print $ isSorted [1, 2, 3, 4, 5, 6] == True
    print $ isSorted [-1, -2, -3, -4, -5, -6] == True
    print $ isSorted [] == True
    print $ isSorted [1] == True

    print $ isSortedXs [-5, -5, -6] == True
    print $ isSortedXs [-5, -5, -4] == True
    print $ isSortedXs [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSortedXs [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSortedXs [-100, -99, -99, -99] == True
    print $ isSortedXs [-100, -99, -99, -99, 100] == True
    print $ isSortedXs [100, 101, -102] == False
    print $ isSortedXs [1, 2, 3, 4, 5, 6] == True
    print $ isSortedXs [-1, -2, -3, -4, -5, -6] == True

isSortedAcending :: (Num a , Ord a) => [a] -> Bool
isSortedAcending [] = True
isSortedAcending [x] =True
isSortedAcending (x:y:xs)
 | x > y = False 
 |otherwise = isSortedAcending (y:xs)


isSorted :: (Num a , Ord a) => [a] -> Bool
isSorted xs = isSortedAcending xs || isSortedAcending (reverse xs)

isSortedXs :: (Num a , Ord a) => [a] -> Bool
isSortedXs xs =  (xs == sort xs ) || (reverse xs  == sort  xs)
