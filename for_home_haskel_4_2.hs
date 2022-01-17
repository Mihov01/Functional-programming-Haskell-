import Data.List
main :: IO()
main = do 
    print $ getAverage db1  == 4.457142857142857

    print $ getNeeded 750 db1  == [Product "Cheese" 750 5.0,Product "Water" 500 0.5,Product "Soap" 250 4.5]

    print $ closestToAverage db1 == ["Milk","Soap"]

    print $ cheaperAlternatives "Lamb" 5.50 db2 == 1
    print $ cheaperAlternatives "Lamb" 10  db2 == 2

type Name = String
type Quantity = Int
type Price = Double
type Database = [Product]

data Product = Product Name Quantity Price
 deriving (Show, Eq)
db1 :: Database
db1 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Butter" 1000 5.50, Product "Water" 500 0.50, Product "Soap" 250 4.50 ]

db2 :: Database
db2 = [ Product "Bread" 1000 1.20, Product "Milk" 2000 4.50, Product "Lamb" 5000 10.00, Product "Cheese" 750 5.00, Product "Lamb" 1000 5.50, Product "Water" 500 0.50, Product "Lamb" 250 4.50 ]

getTotal :: Database -> Price
getTotal = foldl (\ acc (Product _ _ p) -> p + acc) 0

getAverage :: Database ->Price
getAverage d = (getTotal d) / (fromIntegral . length ) d

getNeeded :: Quantity -> Database -> Database
getNeeded amount  = filter (\ (Product _ q _) -> q <= amount)

mapAverage :: Price -> Database -> Database
mapAverage av  =  map (\ (Product name q p ) -> (Product name q (abs (av -p))) )

getMinimum :: Database -> Price
getMinimum d = minimum  (map( \ (Product _ _ p) -> p) d)

closestToAverage :: Database -> [Name]
closestToAverage  d = let  min = getMinimum (mapAverage (getAverage d) d) in map( \ (Product name _ _) -> name) $ filter (\ (Product _ _ p) -> p == min) $ mapAverage (getAverage d) d


cheaperAlternatives :: Name -> Price -> Database -> Int 
cheaperAlternatives name price = length . filter (\ (Product _ _ p) -> p < price) . filter (\ (Product n _ _) -> name == n)