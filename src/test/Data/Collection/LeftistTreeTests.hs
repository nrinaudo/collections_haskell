module Data.Collection.LeftistTreeTests (leftistTreeTests) where

import Data.Collection.LeftistTree
import Data.Collection.PriorityQueueTests

leftistTreeTests = priorityQueueTests (Leaf :: LeftistTree Int) "Data.Collection.LeftistTreeTree"
