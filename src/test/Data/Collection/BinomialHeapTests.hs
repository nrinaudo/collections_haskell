module Data.Collection.BinomialHeapTests (binomialHeapTests) where

import Data.Collection.BinomialHeap
import Data.Collection.Heap
import Data.Collection.HeapTests

binomialHeapTests = heapTests (empty :: BinomialHeap Int) "Data.Collection.BinomialHeap"
