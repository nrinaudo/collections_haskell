module Data.Collection.Set where

class Set s where
  isEmpty  :: s a -> Bool
  insert   :: s a -> a -> s a
  contains :: s a -> a -> Bool
