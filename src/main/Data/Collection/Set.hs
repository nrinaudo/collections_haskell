module Data.Collection.Set where

class Set s where
  empty :: s a
  isEmpty :: s a -> Bool
  insert :: Ord a => s a -> a -> s a
  contains :: Ord a => s a -> a -> Bool
