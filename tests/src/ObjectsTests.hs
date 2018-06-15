{-# LANGUAGE OverloadedStrings #-}

module ObjectsTests
  ( main
  , test
  ) where

import Data.Maybe
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.HUnit as H
import Test.QuickCheck hiding ((.&.))

import qualified Data.ByteString.Lazy as Lazy
import System.FilePath ((</>), takeDirectory)
import Utils

main :: IO ()
main = defaultMain [test]

test :: Test
test = testGroup "Objects" []
