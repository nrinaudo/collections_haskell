{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}

module Data.Collection.BinomialHeap where

import Data.Collection.Heap


-- Binomial Heap -------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
newtype BinomialHeap a = BinomialHeap [BinomialTree a]

insTree :: BinomialTree a -> [BinomialTree a] -> [BinomialTree a]
insTree t [] = [t]
insTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  | otherwise        = insTree (link t t') ts'

instance Heap BinomialHeap where
  type HeapEntry BinomialHeap  a = Ord a
  empty = BinomialHeap []

  isEmpty = error "todo"

  insert (BinomialHeap ts) a = BinomialHeap $ insTree (singleton a) ts

  min     = error "todo"
  deleteMin = error "todo"



-- Binomial Tree -------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
data BinomialTree a = Ord a => BinomialTree Int a [BinomialTree a]

singleton :: Ord a => a -> BinomialTree a
singleton a = BinomialTree 0 a []

rank :: BinomialTree a -> Int
rank (BinomialTree r _ _) = r

root :: BinomialTree a -> a
root (BinomialTree _ a _) = a

-- Note that both trees are expected to be of the same rank.
link :: BinomialTree a -> BinomialTree a -> BinomialTree a
link t1@(BinomialTree r a1 d1) t2@(BinomialTree _ a2 d2)
  | a1 <= a2  = BinomialTree (r + 1) a1 (t2 : d1)
  | otherwise = BinomialTree (r + 1) a2 (t1 : d2)
