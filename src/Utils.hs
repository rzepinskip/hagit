{-|
Module      : Utils
Description : Utility functions - mostly directories handling.
Copyright   : (c) Paweł Rzepiński 2018
License     :  BSD 3
Maintainer  : rzepinski.pawel@email.com
Stability   : experimental
Portability : POSIX
-}
module Utils
  ( workingDir
  , hagitDir
  , refsDir
  , commitsDir
  , objectsDir
  , headPath
  , indexPath
  , readDirectoryRec
  , printStoreDirError
  , executeIfInitialized
  , CommitInfo(..)
  , ShaHash
  ) where

import Conduit ((.|), filterC, runConduitRes, sinkList, sourceDirectoryDeep)
import Control.Monad (forM)
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Hashing (ShaHash)

-- | Commit details
data CommitInfo = CommitInfo
  { getMessage :: String
  , getDate :: String
  , getParentHash :: ShaHash
  } deriving (Show, Read)

-- | Returns working directory.
workingDir :: FilePath
workingDir = "."

-- | Returns hagit directory.
hagitDir :: FilePath
hagitDir = workingDir </> ".hagit"

-- | Returns references directory.
refsDir :: FilePath
refsDir = hagitDir </> "refs"

-- | Returns commits directory.
commitsDir :: FilePath
commitsDir = hagitDir </> "commits"

-- | Returns objects directory.
objectsDir :: FilePath
objectsDir = hagitDir </> "objects"

-- | Returns path to HEAD file.
headPath :: FilePath
headPath = hagitDir </> "HEAD"

-- | Returns path to INDEX file. Also know as staging area.
indexPath :: FilePath
indexPath = hagitDir </> "INDEX"

-- | Recursively reads directory to retrieve all files.
readDirectoryRec :: FilePath -> IO [FilePath]
readDirectoryRec dir =
  runConduitRes $
  sourceDirectoryDeep False dir .| filterC isValidPath .| sinkList

isValidPath :: FilePath -> Bool
isValidPath path =
  not $ any (`isPrefixOf` path) ["./.hagit", "./.git", "./.stack-work"]

-- | Prints out hagit directory errors.
printStoreDirError :: IO ()
printStoreDirError =
  putStrLn "Unable to perform operation: hagit directory (.hagit) not found."

hasStoreDir :: IO Bool
hasStoreDir = do
  let dirList = [hagitDir, hagitDir </> "commits", hagitDir </> "objects"]
  dirsExist <- forM dirList doesDirectoryExist
  headExists <- doesFileExist $ hagitDir </> "HEAD"
  return (and $ headExists : dirsExist)

-- | Executes specified action only if hagit directory exists.
executeIfInitialized :: IO () -> IO ()
executeIfInitialized comp = do
  hagitEnabled <- hasStoreDir
  if hagitEnabled
    then comp
    else printStoreDirError
