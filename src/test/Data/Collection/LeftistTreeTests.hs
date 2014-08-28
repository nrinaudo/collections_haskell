module Data.Collection.LeftistTreeTests (leftistTreeTests) where

import Data.Collection.LeftistTree
import Data.Collection.HeapTests

leftistTreeTests = heapTests (Leaf :: LeftistTree Int) "Data.Collection.LeftistTreeTree"
