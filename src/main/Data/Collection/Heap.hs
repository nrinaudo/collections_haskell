{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Data.Collection.Heap where

import GHC.Prim

class Heap h where
  type HeapEntry h a :: Constraint
  type HeapEntry h a = Ord a

  isEmpty   :: h a -> Bool
  min       :: h a -> Maybe a
  deleteMin :: h a -> h a
  insert    :: HeapEntry h a => h a -> a -> h a
  empty     :: HeapEntry h a => h a
