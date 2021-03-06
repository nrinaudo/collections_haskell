module Main where

import Test.Framework (Test, defaultMain)

import Data.Collection.BinomialHeapTests
import Data.Collection.LeftistHeapTests
import Data.Collection.BinarySearchTreeTests
import Data.Collection.StackTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [binarySearchTreeTests, listStackTests, leftistHeapTests, binomialHeapTests]
