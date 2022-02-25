import Data.List
main ::IO()
main = do 


    print $ colourBTree
    print $ highest Red colourBTree  == 4
    print $ highest Green colourBTree ==3
    print $ highest Blue colourBTree == 4

    print $ highest1 Red colourBTree  == 4
    print $ highest1 Green colourBTree ==4
    print $ highest1 Blue colourBTree == 4

    print $ highest2 Red colourBTree   == 3
    print $ highest2 Green colourBTree ==3
    print $ highest2 Blue colourBTree  == 3
    
data BTree  = Nil | Node Colour BTree  BTree 
 deriving (Show, Eq)

data Colour = Red | Green | Blue
 deriving (Show, Eq)


colourBTree = Node Blue (Node Green (Node Blue (Node Red Nil Nil) Nil) (Node Blue Nil Nil)) (Node Red (Node Green (Node Blue Nil Nil) Nil) (Node Red Nil Nil))

height :: BTree  -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

getLevel :: BTree  -> Int -> [Colour]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

highest :: Colour -> BTree  -> Int
highest color tree = helper tree (height tree) color
 where 
     helper :: BTree -> Int ->Colour -> Int
     helper tree level color 
      | elem color (getLevel tree (level -1)) = level 
      | otherwise = helper tree (level -1) color

-- a version of the task , finds the longest road , containing the color
highest1 ::  Colour -> BTree  -> Int 
highest1  color a =   maximum $ map (length) $ filter (elem color ) (helper1 a)
 where 
     helper1:: BTree  -> [[Colour]]
     helper1 Nil = []
     helper1 (Node word Nil Nil) = [[word]]
     helper1 tree@(Node word left right) =  (map (word:) $ helper1 left ++ helper1 right) 

-- a version of the task , finds the shortest road , containing the color

highest2 ::  Colour -> BTree  -> Int 
highest2  color a =   minimum $ map (length) $ filter (elem color ) (helper2 a)
 where 
     helper2:: BTree  -> [[Colour]]
     helper2 Nil = []
     helper2 (Node word Nil Nil) = [[word]]
     helper2 tree@(Node word left right) =  (map (word:) $ helper2 left ++ helper2 right) 
