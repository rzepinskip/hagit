module Commit
  ( commitCommand
  , storeCommit
  ) where

import Codec.Compression.GZip
import Conduit
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as Lazy
import Data.List (isPrefixOf)
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
  files <-
    runConduitRes $
    sourceDirectoryDeep False "." .| filterC isValidPath .| sinkList
  filesWithHashes <- mapM tryToCopyObject files
  if null filesWithHashes
    then putStrLn "Commit: no files to commit."
    else do
      commitHash <- storeCommit msg filesWithHashes
      putStrLn "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

isValidPath :: FilePath -> Bool
isValidPath path =
  not $ any (`isPrefixOf` path) ["./.hagit", "./.git", "./.stack-work"]

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

data FileWithHash = FileWithHash
  { getPath :: FilePath
  , getContentHash :: ObjectHash
  } deriving (Show)

hashFileData :: FilePath -> IO (Lazy.ByteString, String)
hashFileData path = do
  content <- Lazy.readFile path
  let hash = bsToHex $ hashLazyBS content
  return (content, hash)

tryToCopyObject :: FilePath -> IO FileWithHash
tryToCopyObject path = do
  (content, hash) <- hashFileData path
  let finalName = objectsDir </> hash
  let res = FileWithHash path hash
  exists <- doesFileExist finalName
  if exists
    then return res
    else do
      Lazy.writeFile finalName content
      return res
