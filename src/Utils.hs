module Utils
  ( workingDir
  , hagitDir
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

workingDir :: FilePath
workingDir = "."

hagitDir :: FilePath
hagitDir = combine workingDir ".hagit"

commitsDir :: FilePath
commitsDir = hagitDir </> "commits"

objectsDir :: FilePath
objectsDir = hagitDir </> "objects"

headPath :: FilePath
headPath = hagitDir </> "HEAD"

readCommitHead :: IO String
readCommitHead = do
  fileExist <- doesFileExist headPath
  if not fileExist
    then return ""
    else withFile headPath ReadMode hGetLine

-- | Updates the HEAD file with specified commit    
storeCommitHead :: String -> IO ()
storeCommitHead hash = withFile headPath WriteMode (`hPutStrLn` hash)

loadCommit :: FilePath -> IO (DirTree String)
loadCommit path = do
  contents <- readFile path
  let line = lines contents !! 1
  return $ read line

printStoreDirError :: IO ()
printStoreDirError =
  putStrLn "Unable to perform operation: hagit directory (.hagit) not found."

hasStoreDir :: IO Bool
hasStoreDir = do
  let dirList = [hagitDir, hagitDir </> "commits", hagitDir </> "objects"]
  dirsExist <- forM dirList doesDirectoryExist
  headExists <- doesFileExist $ hagitDir </> "HEAD"
  return (and $ headExists : dirsExist)

execIfStore :: IO () -> IO ()
execIfStore comp = do
  hagitEnabled <- hasStoreDir
  if hagitEnabled
    then comp
    else printStoreDirError
