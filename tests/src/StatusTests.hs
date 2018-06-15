module StatusTests
  ( main
  , test
  ) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.HUnit as H
import Test.QuickCheck hiding ((.&.))

import qualified Data.Map as M
import Status

main :: IO ()
main = defaultMain [test]

test :: Test
test =
  testGroup
    "Status"
    [ testCase "Staged files" staged_1
    , testCase "Unstaged files" unstaged_1
    , testCase "Untracked files" untracked_1
    ]

test_1_commit =
  "fromList [(\"./a\",\"12d81f50767d4e09aa7877da077ad9d1b915d75b\"), (\"./b\",\"991d6bd2a5b689c67cc7602228a28cd12d3adcee\")]"

test_1_index =
  "fromList [(\"./a\",\"12d81f50767d4e09aa7877da077ad9d1b915d75b\"), (\"./b\",\"541d6bd2a5b689c67cc7602228a28cd12d3adcee\"), (\"./c\",\"70949e558320376aaac23634010f7e74248dbbf4\")]"

test_1_work =
  "fromList [(\"./a\",\"2295134321d1047818bcbd080f571c772398039a\"), (\"./c\",\"70949e558320376aaac23634010f7e74248dbbf4\"), (\"./d\",\"67135a3e47a5da5691f94104f20053d3eaa6ad33\")]"

staged_1 = do
  let (staged, _, _) = compareFiles workMap indexMap commitMap
  H.assertEqual [] (read "[Addition \"./c\",Change \"./b\"]") staged
  where
    indexMap = read test_1_index
    workMap = read test_1_work
    commitMap = read test_1_commit

unstaged_1 = do
  let (_, unstaged, _) = compareFiles workMap indexMap commitMap
  H.assertEqual [] (read "[Change \"./a\",Deletion \"./b\"]") unstaged
  where
    indexMap = read test_1_index
    workMap = read test_1_work
    commitMap = read test_1_commit

untracked_1 = do
  let (_, _, untracked) = compareFiles workMap indexMap commitMap
  H.assertEqual [] (read "[Addition \"./d\"]") untracked
  where
    indexMap = read test_1_index
    workMap = read test_1_work
    commitMap = read test_1_commit
