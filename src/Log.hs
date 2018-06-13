module Log
  ( logCommand
  ) where

import Control.Monad (forM, forM_)
import Data.List
import Data.Time.Clock (UTCTime)
import System.Directory (listDirectory)
import System.FilePath (combine)
import System.IO (IOMode(..), hGetLine, withFile)
import qualified System.IO.Strict as S (readFile)

import Utils

-- | Prints log - list of all commits of the repository in specified directory
logCommand :: IO ()
logCommand = execIfStore execLog

execLog :: IO ()
execLog = do
  headRef <- S.readFile headPath
  putStrLn $ "On: " ++ headRef ++ "\n"
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
  commits <- listDirectory commitDir
  return $ map (combine commitDir) commits

loadSortedCommits :: [FilePath] -> IO [CommitInfo]
loadSortedCommits paths = do
  summaries <- forM paths loadCommitInfo
  return $
    reverse $
    sortOn (\(CommitInfo _ date _ _) -> read date :: UTCTime) summaries

loadCommitInfo :: FilePath -> IO CommitInfo
loadCommitInfo path = withFile path ReadMode (fmap read . hGetLine)
