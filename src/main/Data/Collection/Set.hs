module Data.Collection.Set where

class Set s where
  empty    :: Ord a => s a
  isEmpty  :: Ord a => s a -> Bool
  insert   :: Ord a => s a -> a -> s a
  contains :: Ord a => s a -> a -> Bool
