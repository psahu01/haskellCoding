
data BTree a = Empty 
                | BNode a ( BTree a ) ( BTree a ) deriving (Show)
data Dir = Lft | Rght deriving (Show)
type Path = [ Dir ]

trace

dirStruct :: Path
dirStruct = [Lft, Rght]

makeBTree :: BTree String
makeBTree = BNode "Root" (Empty) (Empty)

--Question 1
trace :: Path -> BTree a -> BTree a
trace _ Empty = Empty
trace [] (BNode n l r)  = (BNode n l r) 
trace (Lft:xs) (BNode n l r) = trace xs l
trace (Rght:xs) (BNode n l r) = trace xs r

--Question 4
height :: BTree a -> Int
height Empty = 0
height (BNode _ left right) = (max (height left) (height right)) + 1

--Bonus
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)         