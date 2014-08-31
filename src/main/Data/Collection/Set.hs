{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Data.Collection.Set where

import GHC.Prim

class Set s where
  -- Constraint that elements of s must respect.
  type SetEntry s a :: Constraint
  type SetEntry s a = ()

  -- Returns the empty set.
  empty    :: SetEntry s a => s a

  -- Checks whether a set is empty.
  isEmpty  :: s a -> Bool

  -- Inserts an element in a set.
  insert   :: SetEntry s a => s a -> a -> s a

  -- Checks whether the set contains an element.
  contains :: SetEntry s a => s a -> a -> Bool
