module Data.Collection.BinaryTree (BinaryTree(Node, Leaf)) where

import Data.Collection.Set

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf

instance Set BinaryTree where
  empty = Leaf

  isEmpty Leaf = True
  isEmpty _    = False

  contains a Leaf = False
  contains a (Node v l r)
    | a < v     = contains a l
    | a > v     = contains a r
    | otherwise = True

  insert a Leaf = Node a Leaf Leaf
  insert a s@(Node v l r)
    | a < v = Node v (insert a l) r
    | a > v = Node v l (insert a r)
    | otherwise = s
