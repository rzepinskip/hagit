module Main where

import qualified DiffTests
import qualified ObjectsTests
import qualified StatusTests
import Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "Status" [StatusTests.test]]
