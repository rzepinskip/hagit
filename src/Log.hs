module Log
  ( logCommand
  ) where

import Control.Monad (forM_)
import System.FilePath ((</>))
import System.IO (IOMode(..), hGetLine, withFile)
import qualified System.IO.Strict as S (readFile)

import Branch (readHeadCommit)
import Utils

-- | Prints log - list of all commits of the repository in specified directory
logCommand :: IO ()
logCommand = execIfStore execLog

execLog :: IO ()
execLog = do
  headRef <- S.readFile headPath
  putStrLn $ "On: " ++ headRef
  headCommit <- readHeadCommit
  parents <- traverseParents headCommit []
  forM_ (reverse $ concat $ parents) $ \(CommitInfo msg date hash parentHash) -> do
    putStrLn $ "commit " ++ hash
    putStrLn $ ">>= parentHash: " ++ parentHash
    putStrLn $ ">>= date: " ++ date
    putStrLn $ ">>= message: " ++ msg
    putStrLn ""

traverseParents :: ShaHash -> [CommitInfo] -> IO (ShaHash, [CommitInfo])
traverseParents "" parents = do
  putStrLn ""
  return ("", parents)
traverseParents hash parents = do
  currentParent <- loadCommitInfo $ commitsDir </> hash
  traverseParents (getParentHash currentParent) $ currentParent : parents

loadCommitInfo :: FilePath -> IO CommitInfo
loadCommitInfo path = withFile path ReadMode (fmap read . hGetLine)
