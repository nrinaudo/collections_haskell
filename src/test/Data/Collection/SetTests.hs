{-# LANGUAGE ConstraintKinds #-}

module Data.Collection.SetTests (setTests) where

import Test.Framework                       (testGroup, Test)
import Data.Collection.Set
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property
import Test.QuickCheck

-- Test suite ----------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- Creates tests for the specified set implementation.
setTests :: (Set s, Arbitrary a, Show a, SetEntry s a) => s a -> String -> Test
setTests s label = testGroup label [
  testProperty "the empty set should be empty"             (prop_emptyIsEmpty s),
  testProperty "the empty should not contain any element"  (prop_emptyContainsNothing s),
  testProperty "a set should contains all of its elements" (prop_containsContent s)
  ]



-- Tests ---------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
prop_emptyContainsNothing :: (Set s, SetEntry s a) => s a -> a -> Bool
prop_emptyContainsNothing s = not . contains s

prop_emptyIsEmpty :: Set s => s a -> Bool
prop_emptyIsEmpty = isEmpty

prop_containsContent :: (Set s, SetEntry s a) => s a -> [a] -> Bool
prop_containsContent s as = all (contains set) as
                            where set = createSet s as


-- Helper functions ----------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
createSet :: (Set s, SetEntry s a) => s a -> [a] -> s a
createSet s as = foldl insert s as
