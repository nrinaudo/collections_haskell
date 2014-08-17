module Data.Collection.BinaryTreeTests (propTests) where

import           Test.Framework                       (testGroup)
import           Data.Collection.BinaryTree
import           Data.Collection.Set
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck.Property
import           Test.QuickCheck

propTests = testGroup "Data.Collection.BinaryTree" [
  testProperty "Leaf is empty"                     prop_leafEmpty,
  testProperty "leaf does not contain any element" prop_leafNoContent,
  testProperty "node is not empty"                 prop_nodeNotEmpty,
  testProperty "node contains its content"         prop_nodeContent
  ]


-- Leaf specific tests -------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
prop_leafEmpty :: Bool
prop_leafEmpty = isEmpty Leaf == True

prop_leafNoContent :: Int -> Bool
prop_leafNoContent i = contains i Leaf == False



-- Node specific tests -------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
prop_nodeNotEmpty :: Int -> Bool
prop_nodeNotEmpty i = isEmpty (Node i Leaf Leaf) == False

prop_nodeContent :: Int -> Bool
prop_nodeContent i = contains i (Node i Leaf Leaf) == True
