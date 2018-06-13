module Log
  ( logCommand
  ) where

import Control.Monad (forM_)
import System.FilePath ((</>))
import System.IO (IOMode(..), hGetLine, withFile)
import qualified System.IO.Strict as S (readFile)

import Branch (readHeadCommit)
import Utils

-- | Prints log - list of all commits of the repository in working directory
logCommand :: IO ()
logCommand = executeIfInitialized execLog

execLog :: IO ()
execLog = do
  headRef <- S.readFile headPath
  putStrLn $ "On: " ++ headRef ++ "\n"
  headCommit <- readHeadCommit
  parents <- traverseParents headCommit []
  forM_ (reverse $ concat $ parents) printInfo

printInfo :: (ShaHash, CommitInfo) -> IO ()
printInfo (hash, info) = do
  putStrLn $ "commit " ++ hash
  putStrLn $ ">>= parentHash: " ++ (getParentHash info)
  putStrLn $ ">>= date: " ++ (getDate info)
  putStrLn $ ">>= message: " ++ (getMessage info)
  putStrLn ""

traverseParents ::
     ShaHash -> [(ShaHash, CommitInfo)] -> IO (ShaHash, [(ShaHash, CommitInfo)])
traverseParents "" acc = return ("", acc)
traverseParents hash acc = do
  currentParent <- loadCommitInfo $ commitsDir </> hash
  let parentHash = getParentHash currentParent
  traverseParents (parentHash) $ (hash, currentParent) : acc

loadCommitInfo :: FilePath -> IO CommitInfo
loadCommitInfo path = withFile path ReadMode (fmap read . hGetLine)
