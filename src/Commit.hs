module Commit
  ( commitHvc
  , storeCommit
  ) where

import Codec.Compression.GZip
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as Lazy
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (IOMode(..), hPutStrLn, withFile)

import DirTree
import DirTreeUtils
import Utils

-- | Commits all files in directory with specified message.
commitHvc :: FilePath -> String -> IO ()
commitHvc dir msg = execIfHvc dir (execCommit dir msg)

execCommit :: FilePath -> String -> IO ()
execCommit dir msg = do
  tree <- treeFromDir dir
  let hashContents = extrasFromTree tree
  if null hashContents
    then putStrLn "Commit: no files to commit."
    else do
      storeObjects dir hashContents
      commitHash <- storeCommit dir msg tree
      putStrLn "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

storeObjects :: FilePath -> [HashContents] -> IO ()
storeObjects base obs = forM_ obs $ \hc -> storeObject (objectsDir base) hc

-- | Stores object's hashed content in specified destination
storeObject :: FilePath -> HashContents -> IO ()
storeObject dest hc = do
  let finalName = dest </> hcHash hc
  exists <- doesFileExist finalName
  if exists
    then return ()
    else Lazy.writeFile finalName (compress $ hcContents hc)

-- | Stores commit on disc
storeCommit :: FilePath -> String -> DirTree HashContents -> IO String
storeCommit base msg tree = do
  let hashesTree = removeByteStrings tree
  let commitHash = treeHash hashesTree
  storeCommitHead base commitHash
  storeCommitData base msg commitHash hashesTree
  return commitHash

-- | Stores commit information
storeCommitData :: FilePath -> String -> String -> DirTree String -> IO ()
storeCommitData base msg hash hashesTree = do
  withFile (commitsDir base </> hash) WriteMode $ \file -> do
    date <- getCurrentTime
    hPutStrLn file (show $ CommitSummary msg (show date) hash)
    hPutStrLn file (show hashesTree)
