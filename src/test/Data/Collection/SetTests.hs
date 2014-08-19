module Data.Collection.SetTests where

import Test.Framework                       (testGroup, Test)
import Data.Collection.Set
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property
import Test.QuickCheck

-- Creates tests for the specified set implementation.
setTests :: (Set s, Ord a, Arbitrary a, Show a) => s a -> String -> Test
setTests s label = testGroup label [
  testProperty "the empty set should be empty"            (prop_emptyIsEmpty s),
  testProperty "the empty should not contain any element" (prop_emptyContainsNothing s)
  ]

create_set :: (Set s, Ord a) => s a -> [a] -> s a
create_set s as = foldl (flip insert) s as

prop_emptyContainsNothing :: (Set s, Ord a) => s a -> a -> Bool
prop_emptyContainsNothing s a = not $ contains a s

prop_emptyIsEmpty :: (Set s, Ord a) => s a -> Bool
prop_emptyIsEmpty s = isEmpty s
