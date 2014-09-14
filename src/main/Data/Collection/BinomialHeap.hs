{-# LANGUAGE TypeFamilies, FlexibleInstances, ExistentialQuantification #-}

module Data.Collection.BinomialHeap(BinomialHeap) where

import Data.Collection.Heap


-- Binomial Heap -------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
data BinomialHeap a = Ord a => BinomialHeap [BinomialTree a]

insTree :: BinomialTree a -> [BinomialTree a] -> [BinomialTree a]
insTree t [] = [t]
insTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  | otherwise        = insTree (link t t') ts'


merge :: [BinomialTree a] -> [BinomialTree a] -> [BinomialTree a]
merge [] h2 = h2
merge h1 [] = h1
merge as1@(h1 : t1) as2@(h2 : t2)
  | rank h2 < rank h1 = h1 : merge t1 as2
  | rank h1 < rank h2 = h2 : merge t2 as1
  | otherwise         = insTree (link h1 h2) (merge t1 t2)

removeMinTree :: Ord a => [BinomialTree a] -> (Maybe (BinomialTree a), [BinomialTree a])
removeMinTree []    = (Nothing, [])
removeMinTree [a]   = (Just a, [])
removeMinTree (h:t) = let ((Just m), t') = removeMinTree t in
  if root h < root m then (Just h, t)
  else                    (Just m, h:t')

instance Heap BinomialHeap where
  type HeapEntry BinomialHeap a = Ord a

  empty = BinomialHeap []

  isEmpty (BinomialHeap []) = True
  isEmpty _                 = False

  insert (BinomialHeap ts) a = BinomialHeap $ insTree (singleton a) ts

  findMin (BinomialHeap as) = let (m, _) = removeMinTree as in fmap root m

  deleteMin (BinomialHeap as) = case removeMinTree as of
    (Nothing, _)                        -> error "empty.deleteMin"
    ((Just (BinomialTree _ _ r1),  r2)) -> BinomialHeap $ merge (reverse r1) r2



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
