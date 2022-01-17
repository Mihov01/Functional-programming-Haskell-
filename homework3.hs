import Data.List
main :: IO()
main =do 
    print $ getSunk database ==  [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
    print $ getSunk database == [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
    print $ inBattleAfterDamaged database == ["California","Prince of Wales"]
    print $ grandchildrenIncreased  t1 == True
    print $ grandchildrenIncreased t2 == False

type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int
data Battle = Battle Name Date deriving Show
data Ship = Ship Name Class Launched deriving Show
data Outcome = Outcome Name Name Result deriving Show
type Database = ([Outcome], [Battle], [Ship]) 

outcomes :: [Outcome]
outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", Outcome "California" "Surigao Strait" "ok", Outcome "Duke of York" "North Cape" "ok", Outcome "Fuso" "Surigao Strait" "sunk", Outcome "Hood" "North Atlantic" "sunk", Outcome "King George V" "North Atlantic" "ok", Outcome "Kirishima" "Guadalcanal" "sunk", Outcome "Prince of Wales" "North Atlantic" "damaged", Outcome "Rodney" "North Atlantic" "ok", Outcome "Schamhorst" "North Cape" "sunk", Outcome "South Dakota" "Guadalcanal" "damaged", Outcome "Tennessee" "Surigao Strait" "ok", Outcome "Washington" "Guadalcanal" "ok", Outcome "Prince of Wales" "Guadalcanal" "ok", Outcome "West Virginia" "Surigao Strait" "ok", Outcome "Yamashiro" "Surigao Strait" "sunk", Outcome "California" "Guadalcanal" "damaged" ]
battles :: [Battle]
battles = [ Battle "Guadalcanal" "1942-11-15", Battle "North Atlantic" "1941-05-25", Battle "North Cape" "1943-12-26", Battle "Surigao Strait" "1944-10-25" ]
ships :: [Ship]
ships = [ Ship "California" "Tennessee" 1921, Ship "Haruna" "Kongo" 1916, Ship "Hiei" "Kongo" 1914, Ship "Iowa" "Iowa" 1943, Ship "Kirishima" "Kongo" 1915, Ship "Kongo" "Kongo" 1913, Ship "Missouri" "Iowa" 1944, Ship "Musashi" "Yamato" 1942, Ship "New Jersey" "Iowa" 1943, Ship "North Carolina" "North Carolina" 1941, Ship "Ramillies" "Revenge" 1917, Ship "Renown" "Renown" 1916, Ship "Repulse" "Renown" 1916, Ship "Resolution" "Renown" 1916, Ship "Revenge" "Revenge" 1916, Ship "Royal Oak" "Revenge" 1916, Ship "Royal Sovereign" "Revenge" 1916, Ship "Tennessee" "Tennessee" 1920, Ship "Washington" "North Carolina" 1941, Ship "Wisconsin" "Iowa" 1944, Ship "Yamato" "Yamato" 1941, Ship "Yamashiro" "Yamato" 1947, Ship "South Dakota" "North Carolina" 1941, Ship "Bismarck" "North Carolina" 1911, Ship "Duke of York" "Renown" 1916, Ship "Fuso" "Iowa" 1940, Ship "Hood" "Iowa" 1942, Ship "Rodney" "Yamato" 1915, Ship "Yanashiro" "Yamato" 1918, Ship "Schamhorst" "North Carolina" 1917, Ship "Prince of Wales" "North Carolina" 1937, Ship "King George V" "Iowa" 1942, Ship "West Virginia" "Iowa" 1942 ]
database :: Database
database = (outcomes, battles, ships)

getSunk :: Database -> [(Name, [Name])]
getSunk  (o,b,_) = map (\ (Battle n _ ) -> (n , map (\ (Outcome ship_name _ _) -> ship_name) $ filter (\ (Outcome _ n1 r) -> n == n1 && r == "sunk") o )) b



inBattleAfterDamaged :: Database -> [Name]
inBattleAfterDamaged (o,b,s) = helper (o,sortOn (\ (Battle _ d) -> d) b,s )
 where 
     helper :: Database -> [Name]
     helper ([], _ , _) = []
     helper (o , ((Battle bname _ ) : bs) , s) =
          let 
              damaged_ships = map (\ (Outcome s_name _  _ ) -> s_name ) $ filter (\ (Outcome _  bat  res ) -> bat == bname && res == "damaged") o 
              out = filter (\ (Outcome _  bat  _ ) -> bat /= bname ) o 
           in 
               helper (out , bs , s) ++ filter (\ x -> elem x  (map (\ (Outcome s_name  _  _ ) -> s_name)  out) ) damaged_ships 


data BTree = Nil | Node Int BTree BTree
t1 = Node 1 (Node (-1) (Node 2 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)) (Node (-1) Nil Nil )
t2 = Node 1 (Node 2 (Node 1 Nil Nil) (Node 1 (Node 10 Nil Nil) Nil)) (Node 3 Nil Nil )

checkFather :: Int -> BTree -> Bool
checkFather _ Nil = True
checkFather value (Node _ Nil Nil) = True
checkFather value (Node _ (Node value1 _ _) Nil) =  (value +1) == value1
checkFather value (Node _ Nil (Node value1 _ _) ) =  (value +1) == value1
checkFather value (Node _ (Node value1 _ _) (Node value2 _ _) ) =  (value +1) == value1 &&  (value +1) == value2

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Nil = True
grandchildrenIncreased (Node value left rigth ) = (checkFather value left) && (checkFather value rigth) && grandchildrenIncreased left && grandchildrenIncreased rigth