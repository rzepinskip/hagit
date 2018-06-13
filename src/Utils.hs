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
  , execIfStore
  , CommitInfo(..)
  , ObjectHash
  ) where

import Conduit ((.|), filterC, runConduitRes, sinkList, sourceDirectoryDeep)
import Control.Monad (forM)
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Hashing

data CommitInfo = CommitInfo
  { getMessage :: String
  , getDate :: String
  , getHash :: String
  , getParentHash :: String
  } deriving (Show, Read)

workingDir :: FilePath
workingDir = "."

hagitDir :: FilePath
hagitDir = workingDir </> ".hagit"

refsDir :: FilePath
refsDir = hagitDir </> "refs"

commitsDir :: FilePath
commitsDir = hagitDir </> "commits"

objectsDir :: FilePath
objectsDir = hagitDir </> "objects"

headPath :: FilePath
headPath = hagitDir </> "HEAD"

indexPath :: FilePath
indexPath = hagitDir </> "INDEX"

readDirectoryRec :: FilePath -> IO [FilePath]
readDirectoryRec dir =
  runConduitRes $
  sourceDirectoryDeep False dir .| filterC isValidPath .| sinkList

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
