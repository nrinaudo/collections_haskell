module Data.Collection.BinarySearchTreeTests (binarySearchTreeTests) where

import Data.Collection.BinarySearchTree
import Data.Collection.SetTests

binarySearchTreeTests = setTests (Leaf :: BinarySearchTree Int) "Data.Collection.BinarySearchTree"
