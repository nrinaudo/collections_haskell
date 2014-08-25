module Data.Collection.Heap where

class Heap h where
  isEmpty   :: h a -> Bool
  insert    :: h a -> a -> h a
  min       :: h a -> Maybe a
  deleteMin :: h a -> h a
