module Log
  ( logCommand
  ) where

import Control.Monad (forM, forM_)
import Data.List
import Data.Time.Clock (UTCTime)
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import System.IO (IOMode(..), hGetLine, withFile)

import Utils

-- | Prints log - list of all commits of the repository in specified directory
logCommand :: IO ()
logCommand = execIfStore execLog

execLog :: IO ()
execLog = do
  paths <- getCommitPaths
  sortedCommits <- loadSortedCommits paths
  forM_ sortedCommits $ \(CommitInfo msg date hash parentHash) -> do
    putStrLn $ "commit " ++ hash
    putStrLn $ ">>= parentHash: " ++ parentHash
    putStrLn $ ">>= date: " ++ date
    putStrLn $ ">>= message: " ++ msg
    putStrLn ""

getCommitPaths :: IO [FilePath]
getCommitPaths = do
  let commitDir = commitsDir
  commits <- getDirectoryContents commitDir
  return $ map (combine commitDir) $ filter (`notElem` [".", ".."]) commits

loadSortedCommits :: [FilePath] -> IO [CommitInfo]
loadSortedCommits paths = do
  summaries <- forM paths loadCommitInfo
  return $
    reverse $
    sortOn (\(CommitInfo _ date _ _) -> read date :: UTCTime) summaries

loadCommitInfo :: FilePath -> IO CommitInfo
loadCommitInfo path = withFile path ReadMode (fmap read . hGetLine)
