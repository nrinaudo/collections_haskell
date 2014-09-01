{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}

module Data.Collection.LeftistTree (LeftistTree (Leaf, Node)) where

import Data.Collection.Heap



-- LeftistTree ---------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
data LeftistTree a = Ord a => Node a Int (LeftistTree a) (LeftistTree a)
                   | Ord a => Leaf

-- Creates a leftist tree containing the specified element.
singleton :: Ord a => a -> LeftistTree a
singleton a = Node a 1 Leaf Leaf

-- Extracts the rank of a leftist tree
rank :: LeftistTree a -> Int
rank Leaf           = 0
rank (Node _ r _ _) = r

-- Merges two leftist trees together.
merge :: (Ord a) => LeftistTree a -> LeftistTree a -> LeftistTree a
merge Leaf t    = t
merge t    Leaf = t
merge t1@(Node a1 _ l1 r1) t2@(Node a2 _ l2 r2)
  | a1 < a2   = tag a1 l1 (merge r1 t2)
  | otherwise = tag a2 (merge t1 l2) r2

-- Creates a leftist tree with the specified value and left and right children.
tag :: Ord a => a -> LeftistTree a -> LeftistTree a -> LeftistTree a
tag a l r = if   rank l > rank r
            then Node a (rank l + 1) l r
            else Node a (rank r + 1) r l



-- Heap instance -------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- Leftisttree-specific implementation of Heap.
instance Heap LeftistTree where
  type HeapEntry LeftistTree a = Ord a

  empty = Leaf

  isEmpty Leaf = True
  isEmpty _    = False

  insert p a = merge p (singleton a)

  min Leaf           = Nothing
  min (Node a _ _ _) = Just a

  deleteMin Leaf           = error "Leaf.deleteMin"
  deleteMin (Node _ _ l r) = merge l r
