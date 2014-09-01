{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Data.Collection.Heap where

import GHC.Prim

class Heap h where
  -- Constraint that elements of h must respect.
  type HeapEntry h a :: Constraint
  type HeapEntry h a = ()

  -- Returns the empty heap.
  empty :: HeapEntry h a => h a

  -- Checks whether a heap is empty.
  isEmpty :: h a -> Bool

  -- Returns the minimum element of a heap, or Nothing if empty.
  min :: h a -> Maybe a

  -- Removes the minimum element of a heap (fails if empty).
  deleteMin :: h a -> h a

  -- Inserts an element in a heap.
  insert :: HeapEntry h a => h a -> a -> h a
