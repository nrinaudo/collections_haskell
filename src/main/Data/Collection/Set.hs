module Data.Collection.Set where

class Set s where
  empty :: s a
  isEmpty :: s a -> Bool
  insert :: Ord a => a -> s a -> s a
  contains :: Ord a => a -> s a -> Bool
