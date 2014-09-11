module Data.Collection.LeftistHeapTests (leftistHeapTests) where

import Data.Collection.LeftistHeap
import Data.Collection.HeapTests

leftistHeapTests = heapTests (Leaf :: LeftistHeap Int) "Data.Collection.LeftistHeap"
