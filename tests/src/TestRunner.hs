module Main where

import qualified ObjectTest
import Test.Framework

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [testGroup "Tests" [ObjectTest.test]]
