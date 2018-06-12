module Status
  ( statusCommand
  ) where

import qualified Data.Map.Strict as Map

import Hashing
import Index (loadIndex)
import Utils

-- | Prints status of current repository - new/modified/deleted files in comparison to latest commit
statusCommand :: IO ()
statusCommand = execIfStore execStatus

execStatus :: IO ()
execStatus = do
  files <- readDirectoryRec workingDir
  filesWithHashes <- mapM toFileWithHash files
  commitHead <- readCommitHead
  putStrLn $ "Status: commit HEAD is: " ++ commitHead
  indexedTree <- loadIndex
  let cmpLines = compareTrees filesWithHashes indexedTree
  putStrLn $
    if not (null cmpLines)
      then unlines cmpLines
      else "No changes have been made since last commit."

compareTrees :: [FileWithHash] -> [FileWithHash] -> [String]
compareTrees current stored =
  concatMap (\f -> f currentMap storedMap) cmpFunctions
  where
    currentMap = Map.fromList (map toFileWithHashTuple current)
    storedMap = Map.fromList (map toFileWithHashTuple stored)
    cmpFunctions = [listNewFiles, listDeletedFiles, listChangedFiles]

toFileWithHashTuple :: FileWithHash -> (FilePath, ObjectHash)
toFileWithHashTuple file = (getPath file, getContentHash file)

listNewFiles ::
     Map.Map FilePath ObjectHash -> Map.Map FilePath ObjectHash -> [String]
listNewFiles = mapDifferencesInfo "file not staged: "

listDeletedFiles ::
     Map.Map FilePath ObjectHash -> Map.Map FilePath ObjectHash -> [String]
listDeletedFiles = flip $ mapDifferencesInfo "file deleted: "

mapDifferencesInfo ::
     String
  -> Map.Map FilePath ObjectHash
  -> Map.Map FilePath ObjectHash
  -> [String]
mapDifferencesInfo msg base other = map (\path -> msg ++ path) paths
  where
    paths = map fst (Map.toList (Map.difference base other))

listChangedFiles ::
     Map.Map FilePath ObjectHash -> Map.Map FilePath ObjectHash -> [String]
listChangedFiles base other =
  foldr
    (\(path, same) xs ->
       if same
         then xs
         else ("file modified: " ++ path) : xs)
    []
    changedList
  where
    changedList = Map.toList $ Map.intersectionWith (==) base other
