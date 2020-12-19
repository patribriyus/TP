module BinaryTree
(Arbol
, empty
, size
, leaf
, tree
, add
, between
, inorder
, preorder
, postorder
) where

data Arbol a = Empty | Node a (Arbol a) (Arbol a) deriving (Show)

empty :: Arbol a
empty = Empty

size :: Arbol a -> Integer
size Empty = 0
size (Node _ i d) = 1 + size i + size d

leaf :: a -> Arbol a
leaf x = Node x empty empty

tree :: a -> Arbol a -> Arbol a -> Arbol a
tree x lb rb =  Node x lb rb

add :: (Ord a) => Arbol a -> a -> Arbol a
add Empty  x = leaf x 
add  (Node a left right) x
       | x == a = Node a left right
       | x < a = Node a (add left x) right
       | x > a = Node a left (add right x)
			
between :: (Ord a) => Arbol a -> a -> a -> [a]
between Empty _ _ = []
between (Node a left right) x y
 | y==a = [a] ++ between left x y
 | x==a = [a] ++ between right x y
 | x>a = between right x y
 | y<a = between left x y
 | otherwise = [a] ++ between left x y ++ between right x y

inorder :: Arbol a -> [a]
inorder Empty = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

preorder :: Ord a => Arbol a -> [a]
preorder Empty = []
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

postorder :: Ord a => Arbol a -> [a]
postorder Empty = []
postorder (Node a left right) = postorder left ++ postorder right ++ [a]