module IndexTests
  ( main
  , test
  ) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.HUnit as H
import Test.QuickCheck hiding ((.&.))

import Index

main :: IO ()
main = defaultMain [test]

test :: Test
test =
  testGroup
    "Diff"
    [ testCase "Add new object" update_index_1
    , testCase "Add modifed object" update_index_2
    , testCase "Remove index subset" remove_index_1
    , testCase "Remove nonexistent object" remove_index_2
    ]

test_1_index =
  "fromList [(\"./a\",\"12a81f50767d4e09aa7877da077ad9d1b915d75b\")]"

test_1_work =
  "[(\"./a\",\"12a81f50767d4e09aa7877da077ad9d1b915d75b\"), (\"./b\",\"54b26bd2a5b689c67cc7602228a28cd12d3adcee\")]"

update_index_1 = do
  let updatedIndex = mergeIndexWith index work
  H.assertEqual [] (read $ "fromList " ++ test_1_work) updatedIndex
  where
    index = read test_1_index
    work = read test_1_work

test_2_index =
  "fromList [(\"./a\",\"12a81f50767d4e09aa7877da077ad9d1b915d75b\"), (\"./b\",\"54b26bd2a5b689c67cc7602228a28cd12d3adcee\")]"

test_2_work =
  "[(\"./a\",\"12a81f50767d4e09aa7877da077ad9d1b915d75b\"), (\"./b\",\"99bd6bd2a5b689c67cc7602228a28cd12d3adcee\")]"

update_index_2 = do
  let updatedIndex = mergeIndexWith index work
  H.assertEqual [] (read $ "fromList " ++ test_2_work) updatedIndex
  where
    index = read test_2_index
    work = read test_2_work

test_3_index =
  "fromList [(\"./a\",\"12a81f50767d4e09aa7877da077ad9d1b915d75b\"), (\"./b\",\"54b26bd2a5b689c67cc7602228a28cd12d3adcee\")]"

test_3_work = "[\"./a\"]"

remove_index_1 = do
  let updatedIndex = removeFromIndex index work
  H.assertEqual
    []
    (read "fromList [(\"./b\",\"54b26bd2a5b689c67cc7602228a28cd12d3adcee\")]")
    updatedIndex
  where
    index = read test_3_index
    work = read test_3_work

test_4_index =
  "fromList [(\"./a\",\"54b26bd2a5b689c67cc7602228a28cd12d3adcee\")]"

test_4_work = "[\"./x\"]"

remove_index_2 = do
  let updatedIndex = removeFromIndex index work
  H.assertEqual
    []
    (read "fromList [(\"./a\",\"54b26bd2a5b689c67cc7602228a28cd12d3adcee\")]")
    updatedIndex
  where
    index = read test_4_index
    work = read test_4_work
