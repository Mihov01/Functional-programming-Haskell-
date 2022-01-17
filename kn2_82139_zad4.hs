main :: IO()
main = do 

     print $  isPerfectlyBalanced tree1 == True


data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

tree1 = Node 'H' (Node 'a' (Node 'k' Nil Nil) (Node 'e' Nil Nil)) (Node 's' (Node 'I' Nil Nil) (Node 'I' Nil Nil ))

test :: BTree a -> Int
test Nil = 0
test (Node value Nil Nil) = 0
test (Node value left rigth) = 1 + test left + test rigth

height :: BTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced tree = height tree == test tree