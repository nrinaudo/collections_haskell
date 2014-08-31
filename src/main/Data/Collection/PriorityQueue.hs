{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Data.Collection.PriorityQueue where

import GHC.Prim

class PriorityQueue p where
  -- Constraint that elements of h must respect.
  type PQEntry p a :: Constraint
  type PQEntry p a = ()

  -- Returns the empty priority queue.
  empty :: PQEntry p a => p a

  -- Checks whether a priority queue is empty.
  isEmpty :: p a -> Bool

  -- Returns the minimum element of a priority queue, or Nothing if empty.
  min :: p a -> Maybe a

  -- Removes the minimum element of a priority queue (fails if empty).
  deleteMin :: p a -> p a

  -- Inserts an element in a priority queue.
  insert :: PQEntry p a => p a -> a -> p a
