module Data.Collection.Stack where

-- Contract for stacks
class Stack f where
  empty   :: f a
  isEmpty :: f a -> Bool

  push :: a -> f a -> f a
  top :: f a -> a
  pop :: f a -> f a

  cat :: f a -> f a -> f a
  cat a b = if isEmpty a
               then b
               else push (top a) (cat (pop a) b)

  update :: f a -> Int -> a -> f a
  update _     i _ | i < 0 = error "index out of bounds"
  update stack 0 v = push v (pop stack)
  update stack i v = push (top stack) (update (pop stack) (i - 1) v)

-- Marks lists as stacks.
instance Stack [] where
  empty   = []
  isEmpty = null
  push    = (:)
  top     = head
  pop     = tail
