{-|
Module      : Status
Description : Prints out the status of files - new/modified/deleted files in working directory/staging area/newest commit
Copyright   : (c) Paweł Rzepiński 2018
License     :  BSD 3
Maintainer  : rzepinski.pawel@email.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE PatternSynonyms #-}

module Status
  ( statusCommand
  , compareFiles
  ) where

import Control.Monad (unless)
import qualified Data.List as L (partition)
import qualified Data.Map as M

import Branch (readHeadCommit)
import Commit (loadCommitObjects)
import Hashing (hashFile)
import Index (loadIndex)
import Utils

-- | Prints out the status of files - new/modified/deleted files in working directory/staging area/newest commit
statusCommand :: IO ()
statusCommand = executeIfInitialized execStatus

execStatus :: IO ()
execStatus = do
  files <- readDirectoryRec workingDir
  workFiles <- mapM toFilePathWithHashTuple files
  commitHead <- readHeadCommit
  putStrLn $ "HEAD: " ++ commitHead
  indexFiles <- loadIndex
  headFile <- readHeadCommit
  committedFiles <- loadCommitObjects headFile
  printComparison workFiles indexFiles committedFiles

toFilePathWithHashTuple :: FilePath -> IO (FilePath, ShaHash)
toFilePathWithHashTuple path = do
  hash <- hashFile path
  return (path, hash)

printComparison ::
     [(FilePath, ShaHash)]
  -> M.Map FilePath ShaHash
  -> M.Map FilePath ShaHash
  -> IO ()
printComparison work indexMap commitMap = do
  let (staged, unstaged, untracked) = compareFiles workMap indexMap commitMap
  printDiffs "\nChanges to be committed:\n" staged
  printDiffs "\nChanges not staged for commit:\n" unstaged
  printDiffs "\nUntracked files:\n" untracked
  where
    workMap = M.fromList work

-- | Compares work, index and commit files to detect staged, unstaged, untracked files.
compareFiles ::
     M.Map FilePath ShaHash
  -> M.Map FilePath ShaHash
  -> M.Map FilePath ShaHash
  -> ( [DiffOperation FilePath]
     , [DiffOperation FilePath]
     , [DiffOperation FilePath])
compareFiles workMap indexMap commitMap = (staged, unstaged, untracked)
  where
    staged = getDiffs indexMap commitMap
    (unstaged, untracked) =
      L.partition (not . isAddition) $ getDiffs workMap indexMap

isAddition :: DiffOperation a -> Bool
isAddition (Addition _) = True
isAddition _ = False

getDiffs ::
     M.Map FilePath ShaHash
  -> M.Map FilePath ShaHash
  -> [DiffOperation FilePath]
getDiffs a b =
  (map (\path -> Addition path) added) ++
  (map (\path -> Change path) modified) ++
  (map (\path -> Deletion path) deleted)
  where
    added = M.keys $ M.difference a b
    modified =
      M.keys $
      M.differenceWith
        handleEqualPaths
        (M.intersection b a)
        (M.intersection a b)
    deleted = M.keys $ M.difference b a

data DiffOperation a
  = Addition a
  | Change a
  | Deletion a
  deriving (Show, Read, Eq, Ord)

printDiffs :: String -> [DiffOperation FilePath] -> IO ()
printDiffs header diffs = do
  unless (null diffs) $ putStrLn $ header
  mapM_ printDiff diffs

printDiff :: DiffOperation FilePath -> IO ()
printDiff (Addition path) = putStrLn $ "\t added:" ++ path
printDiff (Change path) = putStrLn $ "\t modified:" ++ path
printDiff (Deletion path) = putStrLn $ "\t deleted:" ++ path

handleEqualPaths :: ShaHash -> ShaHash -> Maybe ShaHash
handleEqualPaths a b =
  if a == b
    then Nothing
    else Just a
