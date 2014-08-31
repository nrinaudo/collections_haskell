{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Data.Collection.Stack where

import GHC.Prim

class Stack s where
  -- Constraint that elements of s must respect.
  type StackEntry s a :: Constraint
  type StackEntry s a = ()

  -- Returns the empty stack.
  empty   :: StackEntry s a => s a

  -- Checks whether a stack is empty.
  isEmpty :: s a -> Bool

  -- Pushes an element on the stack.
  push    :: StackEntry s a => s a -> a -> s a

  -- Returns the element at the top of the stack, or Nothing if it's empty.
  top     :: s a -> Maybe a

  -- Removes the stack's top (this fails if the stack is empty).
  pop     :: s a -> s a

-- Stack implementation for [].
instance Stack [] where
  empty       = []
  isEmpty     = null
  push s a    = a : s
  top []      = Nothing
  top (a : _) = Just a
  pop         = tail
