{-# LANGUAGE ConstraintKinds #-}

module Data.Collection.PriorityQueueTests (priorityQueueTests) where

import Test.Framework                       (testGroup, Test)
import Data.Collection.PriorityQueue
import Data.Maybe
import Data.List                            (sort, reverse)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property
import Test.QuickCheck

-- Creates tests for the specified set implementation.
priorityQueueTests :: (PriorityQueue p, Arbitrary a, Show a, PQEntry p a, Eq a, Ord a) => p a -> String -> Test
priorityQueueTests p label = testGroup label [
  testProperty "the empty priority queue should be empty"                 (prop_emptyIsEmpty  p),
  testProperty "the empty priority queue should have no min"              (prop_emptyHasNoMin p),
  testProperty "the singleton priority queue should have the correct min" (prop_emptyInsert   p),
  testProperty "a priority queue should unfold in ascending order"        (prop_sortedUnfold  p)
  ]

createPQ :: (PriorityQueue p, PQEntry p a) => p a -> [a] -> p a
createPQ p as = foldl insert p as

prop_emptyIsEmpty :: PriorityQueue p => p a -> Bool
prop_emptyIsEmpty = isEmpty

prop_emptyHasNoMin :: PriorityQueue p => p a -> Bool
prop_emptyHasNoMin = isNothing . Data.Collection.PriorityQueue.min

prop_emptyInsert :: (PriorityQueue p, Eq a, PQEntry p a) => p a -> a -> Bool
prop_emptyInsert p a = Data.Collection.PriorityQueue.min (insert p a) == Just a

isSorted :: (PriorityQueue p, Ord a) => p a -> a -> Bool
isSorted p prev = case Data.Collection.PriorityQueue.min p of
  Just a  -> if prev <= a
             then isSorted (deleteMin p) a
             else False
  Nothing -> True

prop_sortedUnfold :: (PriorityQueue p, PQEntry p a, Ord a) => p a -> NonEmptyList a -> Bool
prop_sortedUnfold p (NonEmpty as) = let heap = createPQ p as in
  isSorted (deleteMin heap) (head $ sort as)
