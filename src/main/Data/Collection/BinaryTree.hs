module Data.Collection.BinaryTree (BinaryTree(Node, Leaf)) where

import Data.Collection.Set

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf

instance Set BinaryTree where
  empty = Leaf

  isEmpty Leaf = True
  isEmpty _    = False

  contains Leaf _         = False
  contains (Node v l r) a
    | a < v     = contains l a
    | a > v     = contains r a
    | otherwise = True

  insert Leaf a = Node a Leaf Leaf
  insert s@(Node v l r) a
    | a < v = Node v (insert l a) r
    | a > v = Node v l (insert r a)
    | otherwise = s
