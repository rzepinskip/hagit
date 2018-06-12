module Commit
  ( commitCommand
  , storeCommit
  ) where

import Conduit ((.|), liftIO, mapMC, runConduit, sinkList, yieldMany)
import qualified Data.ByteString.Lazy as Lazy
import Data.Time (getCurrentTime)
import Hashing
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (IOMode(..), hPutStrLn, withFile)
import Utils

-- | Commits all files in directory with specified message.
commitCommand :: String -> IO ()
commitCommand msg = execIfStore (execCommit msg)

execCommit :: String -> IO ()
execCommit msg = do
  files <- readDirectoryRec workingDir
  filesWithHashes <-
    runConduit $ yieldMany files .| mapMC (liftIO . storeObject) .| sinkList
  if null filesWithHashes
    then putStrLn "Commit: no files to commit."
    else do
      commitHash <- storeCommit msg filesWithHashes
      putStrLn "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

-- | Stores commit on disc
storeCommit :: String -> [FileWithHash] -> IO ObjectHash
storeCommit msg filesWithHashes = do
  let filesHashes = map getContentHash filesWithHashes
  let commitHash = bsToHex . hashString $ concat filesHashes
  storeCommitData msg commitHash filesWithHashes
  storeCommitHead commitHash
  return commitHash

-- | Stores commit information
storeCommitData :: String -> ObjectHash -> [FileWithHash] -> IO ()
storeCommitData msg hash filesWithHashes = do
  withFile (commitsDir </> hash) WriteMode $ \file -> do
    date <- getCurrentTime
    parentHash <- readCommitHead
    hPutStrLn file (show $ CommitInfo msg (show date) hash parentHash)
    hPutStrLn file (show filesWithHashes)

storeObject :: FilePath -> IO FileWithHash
storeObject path = do
  hash <- hashFile path
  let res = FileWithHash path hash
  let finalName = objectsDir </> hash
  exists <- doesFileExist finalName
  if exists
    then return res
    else do
      content <- Lazy.readFile path
      Lazy.writeFile finalName content
      return res
