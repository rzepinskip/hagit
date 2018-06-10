module Utils
  ( hagitDir
  , commitsDir
  , objectsDir
  , headPath
  , printStoreDirError
  , readCommitHead
  , storeCommitHead
  , execIfStore
  , loadCommit
  , CommitInfo(..)
  ) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), combine)
import System.IO (IOMode(..), hGetLine, hPutStrLn, withFile)

import DirTree

data CommitInfo = CommitInfo
  { getMessage :: String
  , getDate :: String
  , getHash :: String
  , getParentHash :: String
  } deriving (Show, Read)

hagitDir :: FilePath -> FilePath
hagitDir dir = combine dir ".hagit"

commitsDir :: FilePath -> FilePath
commitsDir dir = hagitDir dir </> "commits"

objectsDir :: FilePath -> FilePath
objectsDir dir = hagitDir dir </> "objects"

headPath :: FilePath -> FilePath
headPath dir = hagitDir dir </> "HEAD"

readCommitHead :: FilePath -> IO String
readCommitHead base = do
  fileExist <- doesFileExist $ headPath base
  if not fileExist
    then return ""
    else withFile (headPath base) ReadMode hGetLine

-- | Updates the HEAD file with specified commit    
storeCommitHead :: FilePath -> String -> IO ()
storeCommitHead base hash =
  withFile (headPath base) WriteMode (`hPutStrLn` hash)

loadCommit :: FilePath -> IO (DirTree String)
loadCommit path = do
  contents <- readFile path
  let line = lines contents !! 1
  return $ read line

printStoreDirError :: IO ()
printStoreDirError =
  putStrLn "Unable to perform operation: hagit directory (.hagit) not found."

hasStoreDir :: FilePath -> IO Bool
hasStoreDir dir = do
  let dirList =
        [hagitDir dir, hagitDir dir </> "commits", hagitDir dir </> "objects"]
  dirsExist <- forM dirList doesDirectoryExist
  headExists <- doesFileExist $ hagitDir dir </> "HEAD"
  return (and $ headExists : dirsExist)

execIfStore :: FilePath -> IO () -> IO ()
execIfStore dir comp = do
  hagitEnabled <- hasStoreDir dir
  if hagitEnabled
    then comp
    else printStoreDirError
