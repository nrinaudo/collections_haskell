module Data.Collection.Heap where

class Heap h where
  empty     :: Ord a => h a
  isEmpty   :: Ord a => h a -> Bool
  insert    :: Ord a => h a -> a -> h a
  min       :: Ord a => h a -> Maybe a
  deleteMin :: Ord a => h a -> h a
