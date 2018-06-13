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
  commitHash <- storeCommitData msg index
  storeHeadCommit commitHash
  return commitHash

-- | Stores commit information
storeCommitData :: String -> [FileWithHash] -> IO ShaHash
storeCommitData msg index = do
  date <- getCurrentTime
  parentHash <- readHeadCommit
  let infoString = show $ CommitInfo msg (show date) parentHash
  let filesHashes = map getContentHash index
  let commitHash = bsToHex . hashString $ concat $ infoString : filesHashes
  writeFile (commitsDir </> commitHash) (infoString ++ "\n" ++ (show index))
  return commitHash

loadCommit :: FilePath -> IO [FileWithHash]
loadCommit path = do
  contents <- readFile path
  let line = lines contents !! 1
  return $ read line
