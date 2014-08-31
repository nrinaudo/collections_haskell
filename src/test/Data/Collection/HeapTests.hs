{-# LANGUAGE ConstraintKinds #-}

module Data.Collection.HeapTests (heapTests) where

import Test.Framework                       (testGroup, Test)
import Data.Collection.Heap
import Data.Maybe
import Data.List                            (sort, reverse)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property
import Test.QuickCheck

-- Creates tests for the specified set implementation.
heapTests :: (Heap h, Arbitrary a, Show a, HeapEntry h a, Eq a, Ord a) => h a -> String -> Test
heapTests h label = testGroup label [
  testProperty "the empty heap should be empty"                 (prop_emptyIsEmpty  h),
  testProperty "the empty heap should have no min"              (prop_emptyHasNoMin h),
  testProperty "the singleton heap should have the correct min" (prop_emptyInsert   h),
  testProperty "a heap should unfold in ascending order"        (prop_sortedUnfold  h)
  ]

createHeap :: (Heap h, HeapEntry h a) => h a -> [a] -> h a
createHeap h as = foldl insert h as

prop_emptyIsEmpty :: Heap h => h a -> Bool
prop_emptyIsEmpty = isEmpty

prop_emptyHasNoMin :: Heap h => h a -> Bool
prop_emptyHasNoMin = isNothing . Data.Collection.Heap.min

prop_emptyInsert :: (Heap h, Eq a, HeapEntry h a) => h a -> a -> Bool
prop_emptyInsert h a = Data.Collection.Heap.min (insert h a) == Just a

isSorted :: (Heap h, Ord a) => h a -> a -> Bool
isSorted h prev = case Data.Collection.Heap.min h of
  Just a  -> if prev <= a
             then isSorted (deleteMin h) a
             else False
  Nothing -> True

prop_sortedUnfold :: (Heap h, HeapEntry h a, Ord a) => h a -> NonEmptyList a -> Bool
prop_sortedUnfold h (NonEmpty as) = let heap = createHeap h as in
  isSorted (deleteMin heap) (head $ sort as)
