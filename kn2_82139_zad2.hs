main :: IO()
main = do 


    print $ stocklist stocks  ['A','B'] ==[('A',200),('B',1140)]
    print $ stocklist stocks ['C','X'] == [('C',500),('X',0)]
    print $ stocklist stocks ['Y','X'] ==  [('Y',0),('X',0)]
    print $ stocklist stocks ['C'] ==  [('C', 500)]

data Stock = Stock String Int
 deriving (Show, Eq)

stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]

func :: [Stock] -> Char -> Int
func xs a =  sum $ map (\ (Stock _ p ) -> p) $ filter (\ (Stock (y : ys) _ ) -> y == a) xs


stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist  xs ys = map (\ x -> (x, func xs x)) ys