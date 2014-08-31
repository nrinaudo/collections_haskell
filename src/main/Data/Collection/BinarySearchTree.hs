{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}

module Data.Collection.BinarySearchTree (BinarySearchTree(Node, Leaf)) where

import Data.Collection.Set


-- BinarySearchTree ----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
data BinarySearchTree a = Ord a => Node a (BinarySearchTree a) (BinarySearchTree a)
                        | Ord a => Leaf

-- Checks whether a BST contains an element.
btContains :: BinarySearchTree a -> a -> Bool
btContains Leaf _ = False
btContains (Node v l r) a
  | a < v     = btContains l a
  | a > v     = btContains r a
  | otherwise = True

-- Inserts an element in a BST.
btInsert :: BinarySearchTree a -> a -> BinarySearchTree a
btInsert Leaf a = Node a Leaf Leaf
btInsert s@(Node v l r) a
  | a < v = Node v (btInsert l a) r
  | a > v = Node v l (btInsert r a)
  | otherwise = s

-- Checks whether a BST is empty.
btEmpty :: BinarySearchTree a -> Bool
btEmpty Leaf = True
btEmpty _    = False




-- Set instance --------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
instance Set BinarySearchTree where
  type SetEntry BinarySearchTree a = Ord a
  empty    = Leaf
  isEmpty  = btEmpty
  contains = btContains
  insert   = btInsert
