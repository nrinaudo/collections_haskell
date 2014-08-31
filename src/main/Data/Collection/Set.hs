{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Data.Collection.Set where

import GHC.Prim

class Set s where
  type SetEntry s a :: Constraint
  type SetEntry s a = Eq a

  empty    :: SetEntry s a => s a
  isEmpty  :: s a -> Bool
  insert   :: SetEntry s a => s a -> a -> s a
  contains :: SetEntry s a => s a -> a -> Bool
