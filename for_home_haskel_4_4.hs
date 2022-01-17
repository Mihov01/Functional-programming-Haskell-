import Data.List
main::IO()
main = do 


    print $ isGraceful t1 == True -- t1 = A
    print $ isGraceful t2 == True -- t2 = B
    print $ isGraceful t3 == False -- t3 = C

data NTree = Nil | Node Int [NTree ]
 deriving (Show)



t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]
t2 = Node 7 [Node 3 [Node 9 [Node 5 [Nil], Node 1 [Nil]]]]
t3 = Node 1 [Node 3 [Nil], Node 5 [Node 42 [Nil]], Node 7 [Nil], Node 9 [Nil]]


isGraceful :: NTree ->Bool
isGraceful tree = all even (helper tree)
 where
    helper :: NTree  -> [Int]
    helper (Node _ [Nil]) = []
    helper (Node value  children) = map (\(Node value1 _) -> abs (value  - value1)) children ++ (concatMap (helper) children)
