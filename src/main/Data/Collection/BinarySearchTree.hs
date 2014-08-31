{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Collection.BinarySearchTree (BinarySearchTree(Node, Leaf)) where

import Data.Collection.Set

data BinarySearchTree a = Node a (BinarySearchTree a) (BinarySearchTree a)
                        | Leaf

btContains :: Ord a => BinarySearchTree a -> a -> Bool
btContains Leaf _ = False
btContains (Node v l r) a
  | a < v     = btContains l a
  | a > v     = btContains r a
  | otherwise = True

btInsert :: Ord a => BinarySearchTree a -> a -> BinarySearchTree a
btInsert Leaf a = Node a Leaf Leaf
btInsert s@(Node v l r) a
  | a < v = Node v (btInsert l a) r
  | a > v = Node v l (btInsert r a)
  | otherwise = s

btEmpty :: BinarySearchTree a -> Bool
btEmpty Leaf = True
btEmpty _    = False



instance Set BinarySearchTree where
  type SetEntry BinarySearchTree a = Ord a
  empty    = Leaf
  isEmpty  = btEmpty
  contains = btContains
  insert   = btInsert
