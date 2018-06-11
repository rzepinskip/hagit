module Commit
  ( commitCommand
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
commitCommand :: String -> IO ()
commitCommand msg = execIfStore (execCommit msg)

execCommit :: String -> IO ()
execCommit msg = do
  tree <- treeFromDir workingDir
  let hashContents = extrasFromTree tree
  if null hashContents
    then putStrLn "Commit: no files to commit."
    else do
      storeObjects hashContents
      commitHash <- storeCommit msg tree
      putStrLn "Commit successful."
      putStrLn $ "Commit hash: " ++ commitHash

storeObjects :: [HashContents] -> IO ()
storeObjects obs = forM_ obs $ \hc -> storeObject objectsDir hc

-- | Stores object's hashed content in specified destination
storeObject :: FilePath -> HashContents -> IO ()
storeObject dest hc = do
  let finalName = dest </> hcHash hc
  exists <- doesFileExist finalName
  if exists
    then return ()
    else Lazy.writeFile finalName (compress $ hcContents hc)

-- | Stores commit on disc
storeCommit :: String -> DirTree HashContents -> IO ObjectHash
storeCommit msg tree = do
  let hashesTree = removeByteStrings tree
  let commitHash = treeHash hashesTree
  storeCommitData msg commitHash hashesTree
  storeCommitHead commitHash
  return commitHash

-- | Stores commit information
storeCommitData :: String -> ObjectHash -> DirTree String -> IO ()
storeCommitData msg hash hashesTree = do
  withFile (commitsDir </> hash) WriteMode $ \file -> do
    date <- getCurrentTime
    parentHash <- readCommitHead
    hPutStrLn file (show $ CommitInfo msg (show date) hash parentHash)
    hPutStrLn file (show hashesTree)
