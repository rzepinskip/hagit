module Commit
  ( commitCommand
  , storeCommit
  , loadCommit
  ) where

import Data.Time (getCurrentTime)
import System.FilePath ((</>))

import Branch (readHeadCommit, storeHeadCommit)
import Hashing
import Index (loadIndex)
import Utils

-- | Commits all files in directory with specified message.
commitCommand :: String -> IO ()
commitCommand msg = execIfStore $ execCommit msg

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
storeCommit :: String -> [FileWithHash] -> IO ShaHash
storeCommit msg index = do
  let filesHashes = map getContentHash index
  let commitHash = bsToHex . hashString $ concat filesHashes
  storeCommitData msg commitHash index
  storeHeadCommit commitHash
  return commitHash

-- | Stores commit information
storeCommitData :: String -> ShaHash -> [FileWithHash] -> IO ()
storeCommitData msg hash index = do
  date <- getCurrentTime
  parentHash <- readHeadCommit
  let info = show $ CommitInfo msg (show date) hash parentHash
  let objects = show index
  writeFile (commitsDir </> hash) (info ++ "\n" ++ objects)

loadCommit :: FilePath -> IO [FileWithHash]
loadCommit path = do
  contents <- readFile path
  let line = lines contents !! 1
  return $ read line
