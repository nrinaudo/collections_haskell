{-# LANGUAGE ExistentialQuantification #-}

module Data.Collection.LeftistTree (LeftistTree (Leaf, Node)) where

import Data.Collection.Heap

data LeftistTree a = Ord a => Node a Int (LeftistTree a) (LeftistTree a)
                   | Ord a => Leaf

instance Heap LeftistTree where
  isEmpty Leaf = True
  isEmpty _    = False

  -- Not the cleanest, I'd much rather use merge directly, but we need to explicitly reference Leaf and Node to bring
  -- the Ord a class constraint in scope.
  insert Leaf             a = singleton a
  insert t@(Node _ _ _ _) a = merge t (singleton a)

  min Leaf           = Nothing
  min (Node a _ _ _) = Just a

  deleteMin Leaf           = error "Leaf.deleteMin"
  deleteMin (Node _ _ l r) = merge l r


singleton :: Ord a => a -> LeftistTree a
singleton a = Node a 1 Leaf Leaf

rank :: LeftistTree a -> Int
rank Leaf           = 0
rank (Node _ r _ _) = r

merge :: (Ord a) => LeftistTree a -> LeftistTree a -> LeftistTree a
merge Leaf t    = t
merge t    Leaf = t
merge t1@(Node a1 _ l1 r1) t2@(Node a2 _ l2 r2)
  | a1 < a2   = tag a1 l1 (merge r1 t2)
  | otherwise = tag a2 (merge t1 l2) r2

tag :: Ord a => a -> LeftistTree a -> LeftistTree a -> LeftistTree a
tag a l r = if   rank l > rank r
            then Node a ((rank l) + 1) l r
            else Node a ((rank r) + 1) r l
