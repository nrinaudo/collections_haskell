module Data.Collection.Stack where

-- Contract for stacks
class Stack f where
  empty   :: f a
  isEmpty :: f a -> Bool

  push :: f a -> a -> f a
  top :: f a -> Maybe a
  pop :: f a -> f a

-- Marks lists as stacks.
instance Stack [] where
  empty       = []
  isEmpty     = null
  push s a    = a : s
  top []      = Nothing
  top (a : _) = Just a
  pop         = tail
