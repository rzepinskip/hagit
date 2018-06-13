{-# LANGUAGE PatternSynonyms #-}

module Status
  ( statusCommand
  , compareFiles
  ) where

import Control.Monad (unless)
import qualified Data.Map as M
import System.FilePath ((</>))

import Branch (readHeadCommit)
import Commit (loadCommit)
import Hashing
import Index (loadIndex)
import Utils

-- | Prints status of work repository - new/modified/deleted files in comparison to latest commit
statusCommand :: IO ()
statusCommand = execIfStore execStatus

execStatus :: IO ()
execStatus = do
  files <- readDirectoryRec workingDir
  workFiles <- mapM toFileWithHash files
  commitHead <- readHeadCommit
  putStrLn $ "HEAD: " ++ commitHead
  indexFiles <- loadIndex
  headFile <- readHeadCommit
  committedFiles <- loadCommit $ commitsDir </> headFile
  compareFiles workFiles indexFiles committedFiles

compareFiles :: [FileWithHash] -> [FileWithHash] -> [FileWithHash] -> IO ()
compareFiles work index commit = do
  printDiffs "\nChanges to be committed:" $ getDiffs indexMap commitMap
  let unstaged = getDiffs workMap indexMap
  printDiffs "\nChanges not staged for commit:" $
    filter (not . isAddition) unstaged
  printDiffs "\nUntracked files:" $ filter isAddition unstaged
  where
    workMap = M.fromList (map toFileWithHashTuple work)
    indexMap = M.fromList (map toFileWithHashTuple index)
    commitMap = M.fromList (map toFileWithHashTuple commit)

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
    modified = M.keys $ M.differenceWith handleEqualPaths b $ M.intersection a b
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

toFileWithHashTuple :: FileWithHash -> (FilePath, ShaHash)
toFileWithHashTuple file = (getPath file, getContentHash file)

handleEqualPaths :: ShaHash -> ShaHash -> Maybe ShaHash
handleEqualPaths a b =
  if a == b
    then Nothing
    else Just a
