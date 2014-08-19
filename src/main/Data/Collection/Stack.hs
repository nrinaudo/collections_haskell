module Data.Collection.Stack where

-- Contract for stacks
class Stack f where
  empty   :: f a
  isEmpty :: f a -> Bool

  push :: a -> f a -> f a
  top :: f a -> a
  pop :: f a -> f a

-- Marks lists as stacks.
instance Stack [] where
  empty   = []
  isEmpty = null
  push    = (:)
  top     = head
  pop     = tail
