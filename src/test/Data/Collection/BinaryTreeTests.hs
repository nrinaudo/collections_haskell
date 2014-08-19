module Data.Collection.BinaryTreeTests (binaryTreeTests) where

import Data.Collection.BinaryTree
import Data.Collection.SetTests

binaryTreeTests = setTests (Leaf :: BinaryTree Int) "Data.Collection.BinaryTree"
