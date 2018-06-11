module Status
  ( statusCommand
  ) where

import qualified Data.Map.Strict as Map
import System.FilePath ((</>))

import DirTree
import DirTreeUtils
import Utils

-- | Prints status of current repository - new/modified/deleted files in comparison to latest commit
statusCommand :: IO ()
statusCommand = execIfStore execStatus

execStatus :: IO ()
execStatus = do
  currentTree <- treeFromDir workingDir
  commitHead <- readCommitHead
  let currentTreeHashes = removeByteStrings currentTree
  putStrLn $ "Status: commit HEAD is: " ++ commitHead
  let commitPath = commitsDir </> commitHead
  commitedTree <- loadCommit commitPath
  let cmpLines = compareTrees currentTreeHashes commitedTree
  putStrLn $
    if not (null cmpLines)
      then unlines cmpLines
      else "No changes have been made since last commit."

compareTrees :: DirTree String -> DirTree String -> [String]
compareTrees current stored =
  concatMap (\f -> f currentMap storedMap) cmpFunctions
  where
    currentMap = Map.fromList (pathExtrasFromTree current)
    storedMap = Map.fromList (pathExtrasFromTree stored)
    cmpFunctions = [listNewFiles, listDeletedFiles, listChangedFiles]

listNewFiles :: Map.Map FilePath String -> Map.Map FilePath String -> [String]
listNewFiles = mapDifferencesInfo "new file created: "

listDeletedFiles ::
     Map.Map FilePath String -> Map.Map FilePath String -> [String]
listDeletedFiles = flip $ mapDifferencesInfo "file deleted: "

mapDifferencesInfo ::
     String -> Map.Map FilePath String -> Map.Map FilePath String -> [String]
mapDifferencesInfo msg base other = map (\path -> msg ++ path) paths
  where
    paths = map fst (Map.toList (Map.difference base other))

listChangedFiles ::
     Map.Map FilePath String -> Map.Map FilePath String -> [String]
listChangedFiles base other =
  foldr
    (\(path, same) xs ->
       if same
         then xs
         else ("file changed: " ++ path) : xs)
    []
    changedList
  where
    changedList = Map.toList $ Map.intersectionWith (==) base other
