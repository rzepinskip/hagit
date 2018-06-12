module Commit
  ( commitCommand
  , storeCommit
  ) where

import Data.Time (getCurrentTime)
import System.FilePath ((</>))

import Hashing
import Index (loadIndex)
import Utils

-- | Commits all files in directory with specified message.
commitCommand :: String -> IO ()
commitCommand msg = execIfStore (execCommit msg)

execCommit :: String -> IO ()
execCommit msg = do
  index <- loadIndex
  if null index
    then putStrLn "Commit: no files to commit."
    else do
      commitHash <- storeCommit msg index
      putStrLn "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

-- | Stores commit on disc
storeCommit :: String -> [FileWithHash] -> IO ObjectHash
storeCommit msg index = do
  let filesHashes = map getContentHash index
  let commitHash = bsToHex . hashString $ concat filesHashes
  storeCommitData msg commitHash index
  storeCommitHead commitHash
  return commitHash

-- | Stores commit information
storeCommitData :: String -> ObjectHash -> [FileWithHash] -> IO ()
storeCommitData msg hash index = do
  date <- getCurrentTime
  parentHash <- readCommitHead
  let info = show $ CommitInfo msg (show date) hash parentHash
  let objects = show index
  writeFile (commitsDir </> hash) (info ++ "\n" ++ objects)
