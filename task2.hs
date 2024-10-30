module BinaryTree
  ( BinaryTree(..)
  , treeMap
  , treeSize
  , treeTraverseD
  , treeTraverseW
  ) where

data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap _ Empty = Empty
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)

treeSize :: BinaryTree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

treeTraverseD :: (a -> b -> b) -> b -> BinaryTree a -> b
treeTraverseD _ acc Empty = acc
treeTraverseD f acc (Node x left right) =
  let accLeft = treeTraverseD f acc left
      accNode = f x accLeft
  in treeTraverseD f accNode right

treeTraverseW :: (a -> b -> b) -> b -> BinaryTree a -> b
treeTraverseW f acc tree = go [tree] acc
  where
    go [] acc = acc
    go (Empty:xs) acc = go xs acc
    go (Node x left right:xs) acc =
      let newAcc = f x acc
      in go (xs ++ [left, right]) newAcc
      
tree :: BinaryTree String
tree = Node "1"
         (Node "2"
           (Node "3" Empty Empty)
           (Node "4" Empty Empty))
         (Node "6"
           (Node "7" Empty Empty)
           (Node "8" Empty Empty))
