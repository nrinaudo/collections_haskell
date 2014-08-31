{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Data.Collection.Stack where

import GHC.Prim

class Stack s where
  type StackEntry s a :: Constraint
  type StackEntry s a = ()

  empty   :: StackEntry s a => s a
  isEmpty :: s a -> Bool

  push    :: StackEntry s a => s a -> a -> s a
  top     :: s a -> Maybe a
  pop     :: s a -> s a

instance Stack [] where
  empty       = []
  isEmpty     = null
  push s a    = a : s
  top []      = Nothing
  top (a : _) = Just a
  pop         = tail
