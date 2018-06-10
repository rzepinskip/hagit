module Utils
  ( hagitDir
  , commitsDir
  , objectsDir
  , headPath
  , printHvcDirError
  , storeCommitHead
  , readCommitHead
  , execIfHvc
  , loadCommit
  , CommitSummary(..)
  ) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), combine)
import System.IO (IOMode(..), hGetLine, hPutStrLn, withFile)

import DirTree

data CommitSummary =
  CommitSummary String
                String
                String
  deriving (Show, Read)

hagitDir :: FilePath -> FilePath
hagitDir dir = combine dir ".hagit"

commitsDir :: FilePath -> FilePath
commitsDir dir = hagitDir dir </> "commits"

objectsDir :: FilePath -> FilePath
objectsDir dir = hagitDir dir </> "objects"

headPath :: FilePath -> FilePath
headPath dir = hagitDir dir </> "HEAD"

storeCommitHead :: FilePath -> String -> IO ()
storeCommitHead base hash =
  withFile (headPath base) WriteMode (`hPutStrLn` hash)

readCommitHead :: FilePath -> IO String
readCommitHead base = withFile (headPath base) ReadMode hGetLine

loadCommit :: FilePath -> IO (DirTree String)
loadCommit path = do
  contents <- readFile path
  let line = lines contents !! 1
  return $ read line

printHvcDirError :: IO ()
printHvcDirError =
  putStrLn "Unable to perform operation: hagit directory (.hagit) not found."

hasHvcDir :: FilePath -> IO Bool
hasHvcDir dir = do
  let dirList =
        [hagitDir dir, hagitDir dir </> "commits", hagitDir dir </> "objects"]
  dirsExist <- forM dirList doesDirectoryExist
  headExists <- doesFileExist $ hagitDir dir </> "HEAD"
  return (and $ headExists : dirsExist)

execIfHvc :: FilePath -> IO () -> IO ()
execIfHvc dir comp = do
  hagitEnabled <- hasHvcDir dir
  if hagitEnabled
    then comp
    else printHvcDirError
