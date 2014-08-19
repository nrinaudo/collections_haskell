module Main where

import Test.Framework           (Test, defaultMain)

import Data.Collection.BinaryTreeTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [binaryTreeTests]
