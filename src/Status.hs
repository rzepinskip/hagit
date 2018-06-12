module Status
  ( statusCommand
  ) where

import Control.Monad (unless, when)
import qualified Data.Map as M
import System.FilePath ((</>))

import Hashing
import Index (loadIndex)
import Utils

-- | Prints status of work repository - new/modified/deleted files in comparison to latest commit
statusCommand :: IO ()
statusCommand = execIfStore execStatus

execStatus :: IO ()
execStatus = do
  files <- readDirectoryRec workingDir
  workFiles <- mapM toFileWithHash files
  commitHead <- readCommitHead
  putStrLn $ "HEAD: " ++ commitHead
  indexFiles <- loadIndex
  headFile <- readCommitHead
  committedFiles <- loadCommit $ commitsDir </> headFile
  compareFiles workFiles indexFiles committedFiles

compareFiles :: [FileWithHash] -> [FileWithHash] -> [FileWithHash] -> IO ()
compareFiles work index commit = do
  let workMap = M.fromList (map toFileWithHashTuple work)
  let indexMap = M.fromList (map toFileWithHashTuple index)
  let commitMap = M.fromList (map toFileWithHashTuple commit)
  printChanges "\nChanges to be committed:\n" "added:" indexMap commitMap
  printChanges "\nChanges not staged for commit:\n" "deleted:" indexMap workMap
  let unstaged = workMap `M.difference` indexMap
  printMap "\nUntracked files:\n" unstaged

printChanges ::
     String
  -> String
  -> M.Map FilePath ObjectHash
  -> M.Map FilePath ObjectHash
  -> IO ()
printChanges header differentLabel biggerMap smallerMap = do
  let different = M.difference biggerMap smallerMap
  let modified =
        M.difference
          (M.differenceWith handleEqualPaths biggerMap smallerMap)
          different
  when (not (null different) || not (null modified)) $ putStrLn header
  printMapInlineHeader differentLabel different
  printMapInlineHeader "modified:" modified

toFileWithHashTuple :: FileWithHash -> (FilePath, ObjectHash)
toFileWithHashTuple file = (getPath file, getContentHash file)

handleEqualPaths :: ObjectHash -> ObjectHash -> Maybe ObjectHash
handleEqualPaths a b =
  if a == b
    then Nothing
    else Just a

printMapInlineHeader :: String -> M.Map FilePath ObjectHash -> IO ()
printMapInlineHeader header filesMap = do
  let paths =
        map (\(path, _) -> "\t" ++ header ++ "\t" ++ path) $ M.toList filesMap
  unless (null paths) $ putStr $ unlines paths

printMap :: String -> M.Map FilePath ObjectHash -> IO ()
printMap header filesMap = do
  let paths = map (\(path, _) -> "\t" ++ path) $ M.toList filesMap
  unless (null paths) $ putStrLn $ ((header ++ "\n") ++) $ unlines paths
