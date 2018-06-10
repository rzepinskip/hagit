{-# LANGUAGE OverloadedStrings #-}

module ObjectTest
  ( main
  , test
  ) where

import Data.Maybe
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.HUnit as H
import Test.QuickCheck hiding ((.&.))

import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as Lazy
import System.FilePath ((</>), takeDirectory)
import Utils

main :: IO ()
main = defaultMain [test]

tree_1 =
  "100644 M.hs\NUL\130N\229H6\233\249\USd\n\DC3I\223'\CANp;\165\158\150\&100644 RunMain.hs\NUL\240i\182\&3g\183\194\241-\131\187W\137\ESC\CAN\f\SOHX\180\174"

test_parseObjectFile = do
  compressedContents <-
    Lazy.readFile
      (objectsDir "." </> "807b6f470a6f9db5c25f845e2b642a1056c2d18a")
  H.assertEqual
    "Not equal content"
    (decompress compressedContents)
    "import Distribution.Simple\nmain = defaultMain\n"

-- =================================================================================
test_parseValidTree =
  H.assertBool "Should be able to parse a valid Tree blob" (isJust $ Just "")

test :: Test
test =
  testGroup
    "Objects"
    [ testCase "parseCommit/1" test_parseObjectFile
    , testCase "parseTree/1" test_parseValidTree
    ]
