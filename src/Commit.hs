module Commit
  ( commitCommand
  , storeCommit
  , loadCommitObjects
  ) where

import Data.Time (getCurrentTime)
import System.FilePath ((</>))

import Branch (readHeadCommit, storeHeadCommit)
import qualified Data.Map as M
import Hashing (bsToHex, hashString)
import Index (loadIndex)
import Utils

-- | Commits currently staged files with specified message.
commitCommand :: String -> IO ()
commitCommand msg = executeIfInitialized $ execCommit msg

execCommit :: String -> IO ()
execCommit msg = do
  index <- loadIndex
  if null index
    then putStrLn "Commit: no files to commit."
    else do
      commitHash <- storeCommit msg index
      putStrLn "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

-- | Stores commit with specified message and using staged files.
storeCommit :: String -> M.Map FilePath ShaHash -> IO ShaHash
storeCommit msg index = do
  commitHash <- storeCommitData msg index
  storeHeadCommit commitHash
  return commitHash

-- | Stores commit information.
storeCommitData :: String -> M.Map FilePath ShaHash -> IO ShaHash
storeCommitData msg index = do
  date <- getCurrentTime
  parentHash <- readHeadCommit
  let infoString = show $ CommitInfo msg (show date) parentHash
  let filesHashes = M.elems index
  let commitHash = bsToHex . hashString $ concat $ infoString : filesHashes
  writeFile (commitsDir </> commitHash) (infoString ++ "\n" ++ (show index))
  return commitHash

-- | Loads specified commits' objects.
loadCommitObjects :: ShaHash -> IO (M.Map FilePath ShaHash)
loadCommitObjects hash = do
  contents <- readFile $ commitsDir </> hash
  let line = lines contents !! 1
  return $ read line
