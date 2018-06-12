module Status
  ( statusCommand
  ) where

import Control.Monad (unless, when)
import qualified Data.Map as M

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
  compareIndexAndWorkingDir workFiles indexFiles

compareIndexAndWorkingDir :: [FileWithHash] -> [FileWithHash] -> IO ()
compareIndexAndWorkingDir work index = do
  let workMap = M.fromList (map toFileWithHashTuple work)
  let indexMap = M.fromList (map toFileWithHashTuple index)
  let staged = indexMap `M.intersection` workMap
  printMap "Changes to be committed:\n" staged
  let deleted = indexMap `M.difference` workMap
  let modified = M.differenceWith cmpEqual indexMap workMap
  when (not (null deleted) || not (null modified)) $
    putStrLn "Changes not staged for commit:\n"
  printMapInlineHeader "deleted:" deleted
  printMapInlineHeader "modified:" modified
  let unstaged = workMap `M.difference` indexMap
  printMap "Untracked files:\n" unstaged

toFileWithHashTuple :: FileWithHash -> (FilePath, ObjectHash)
toFileWithHashTuple file = (getPath file, getContentHash file)

cmpEqual :: ObjectHash -> ObjectHash -> Maybe ObjectHash
cmpEqual a b =
  if a == b
    then Nothing
    else Just a

printMapInlineHeader :: String -> M.Map FilePath ObjectHash -> IO ()
printMapInlineHeader header filesMap = do
  let paths =
        map (\(path, _) -> "\t" ++ header ++ "\t" ++ path) $ M.toList filesMap
  unless (null paths) $ putStrLn $ unlines paths

printMap :: String -> M.Map FilePath ObjectHash -> IO ()
printMap header filesMap = do
  let paths = map (\(path, _) -> "\t" ++ path) $ M.toList filesMap
  unless (null paths) $ putStrLn $ ((header ++ "\n") ++) $ unlines paths
