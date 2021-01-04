data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node left y right)
  | x == y = Node left x right
  | x < y = Node (insert x left) y right
  | x > y = Node left y (insert x right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node l x r) = x : preOrder l ++ preOrder r

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node l x r) = postOrder l ++ postOrder r ++ [x]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ y Leaf = y
foldTree f y (Node l x r) =
    f x ( foldTree f (foldTree f y l) l  )
