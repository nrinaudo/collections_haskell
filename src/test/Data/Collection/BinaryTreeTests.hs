module Data.Collection.BinaryTreeTests (propTests) where

import           Test.Framework                       (testGroup)
import           Data.Collection.BinaryTree
import           Data.Collection.Set
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck.Property

propTests = testGroup "Data.Collection.Set" [
  testProperty "leafEmpty" prop_leafEmpty
  ]

prop_leafEmpty :: Bool
prop_leafEmpty = isEmpty Leaf == True
