module Main where

import Test.Framework (Test, defaultMain)

import Data.Collection.BinaryTreeTests
import Data.Collection.StackTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [binaryTreeTests, listStackTests]
