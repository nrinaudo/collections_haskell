module Data.Collection.LeftistTree (LeftistTree (Leaf, Node)) where

import Data.Collection.Heap

data LeftistTree a = Leaf | Node a Int (LeftistTree a) (LeftistTree a)

instance Heap LeftistTree where
  empty = Leaf

  isEmpty Leaf = True
  isEmpty _    = False

  insert t a = merge t (Node a 1 Leaf Leaf)

  min Leaf           = Nothing
  min (Node a _ _ _) = Just a

  deleteMin Leaf           = error "Leaf.deleteMin"
  deleteMin (Node _ _ l r) = merge l r


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
