module Utils
  ( workingDir
  , hagitDir
  , commitsDir
  , objectsDir
  , headPath
  , readWorkingTree
  , printStoreDirError
  , readCommitHead
  , storeCommitHead
  , execIfStore
  , loadCommit
  , CommitInfo(..)
  , ObjectHash
  , FileWithHash(..)
  ) where

import Conduit ((.|), filterC, runConduitRes, sinkList, sourceDirectoryDeep)
import Control.Monad (forM)
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), combine)
import System.IO (IOMode(..), hGetLine, hPutStrLn, withFile)

import Hashing

data CommitInfo = CommitInfo
  { getMessage :: String
  , getDate :: String
  , getHash :: String
  , getParentHash :: String
  } deriving (Show, Read)

data FileWithHash = FileWithHash
  { getPath :: FilePath
  , getContentHash :: ObjectHash
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
storeCommitHead :: ObjectHash -> IO ()
storeCommitHead hash = withFile headPath WriteMode (`hPutStrLn` hash)

loadCommit :: FilePath -> IO [FileWithHash]
loadCommit path = do
  contents <- readFile path
  let line = lines contents !! 1
  return $ read line

readWorkingTree :: IO [FilePath]
readWorkingTree =
  runConduitRes $
  sourceDirectoryDeep False workingDir .| filterC isValidPath .| sinkList

isValidPath :: FilePath -> Bool
isValidPath path =
  not $ any (`isPrefixOf` path) ["./.hagit", "./.git", "./.stack-work"]

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
