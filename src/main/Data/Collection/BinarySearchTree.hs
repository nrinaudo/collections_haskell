{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}

module Data.Collection.BinarySearchTree (BinarySearchTree(Node, Leaf)) where

import Data.Collection.Set


-- BinarySearchTree ----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
data BinarySearchTree a = Ord a => Node a (BinarySearchTree a) (BinarySearchTree a)
                        | Ord a => Leaf



-- Set instance --------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
instance Set BinarySearchTree where
  type SetEntry BinarySearchTree a = Ord a

  empty = Leaf

  isEmpty Leaf = True
  isEmpty _    = False

  contains Leaf _ = False
  contains (Node v l r) a
    | a < v     = contains l a
    | a > v     = contains r a
    | otherwise = True

  insert Leaf a = Node a Leaf Leaf
  insert s@(Node v l r) a
    | a < v = Node v (insert l a) r
    | a > v = Node v l (insert r a)
    | otherwise = s
