{-# LANGUAGE ConstraintKinds #-}

module Data.Collection.StackTests (stackTests, listStackTests) where

import Test.Framework                       (testGroup, Test)
import Data.Collection.Stack
import Data.Maybe
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property
import Test.QuickCheck

stackTests :: (Stack s, Arbitrary a, Show a, Eq a, StackEntry s a) => s a -> String -> Test
stackTests s label = testGroup label [
  testProperty "the empty stack should be empty"                           (prop_emptyIsEmpty s),
  testProperty "the empty stack not have a top"                            (prop_emptyNoTop s),
  testProperty "the empty stack must have the correct value after push"    (prop_emptyPushPopValue s),
  testProperty "the empty stack must have an empty pop after push"         (prop_emptyPushPopEmpty s),
  testProperty "a stack should pop the elements that are added to it LIFO" (prop_insertOrder s)
  ]

createStack :: (Stack s, StackEntry s a) => s a -> [a] -> s a
createStack s as = foldr (flip push) s as


prop_emptyIsEmpty :: (Stack s) => s a -> Bool
prop_emptyIsEmpty = isEmpty

prop_emptyNoTop :: (Stack s) => s a -> Bool
prop_emptyNoTop = isNothing . top

prop_emptyPushPopValue :: (Stack s, Eq a, StackEntry s a) => s a -> a -> Bool
prop_emptyPushPopValue s a = top (push s a) == Just a

prop_emptyPushPopEmpty :: (Stack s, Eq a, StackEntry s a) => s a -> a -> Bool
prop_emptyPushPopEmpty s a = isEmpty $ pop (push s a)

prop_insertOrder :: (Stack s, Eq a, StackEntry s a) => s a -> [a] -> Bool
prop_insertOrder s as = isSorted (createStack s as) as

isSorted :: (Stack s, Eq a) => s a -> [a] -> Bool
isSorted s (head : tail) = ((top s) == Just head) && isSorted (pop s) tail
isSorted s _             = isEmpty s


-- TODO: work out how to catch errors.
--prop_emptyNoPop :: (Stack s) => s a -> Bool
--prop_emptyNoPop s =

listStackTests = stackTests ([] :: [Int]) "Data.Collection.ListStack"
