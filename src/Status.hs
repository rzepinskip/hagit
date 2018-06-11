module Status
  ( statusCommand
  ) where

import qualified Data.Map.Strict as Map
import System.FilePath ((</>))

import Hashing
import Utils

-- | Prints status of current repository - new/modified/deleted files in comparison to latest commit
statusCommand :: IO ()
statusCommand = execIfStore execStatus

execStatus :: IO ()
execStatus = do
  files <- readWorkingTree
  filesWithHashes <- mapM toFileWithHash files
  commitHead <- readCommitHead
  putStrLn $ "Status: commit HEAD is: " ++ commitHead
  let commitPath = commitsDir </> commitHead
  commitedTree <- loadCommit commitPath
  let cmpLines = compareTrees filesWithHashes commitedTree
  putStrLn $
    if not (null cmpLines)
      then unlines cmpLines
      else "No changes have been made since last commit."

toFileWithHash :: FilePath -> IO FileWithHash
toFileWithHash path = do
  hash <- hashFile path
  return $ FileWithHash path hash

toFileWithHashTuple :: FileWithHash -> (FilePath, ObjectHash)
toFileWithHashTuple file = (getPath file, getContentHash file)

compareTrees :: [FileWithHash] -> [FileWithHash] -> [String]
compareTrees current stored =
  concatMap (\f -> f currentMap storedMap) cmpFunctions
  where
    currentMap = Map.fromList (map toFileWithHashTuple current)
    storedMap = Map.fromList (map toFileWithHashTuple stored)
    cmpFunctions = [listNewFiles, listDeletedFiles, listChangedFiles]

listNewFiles ::
     Map.Map FilePath ObjectHash -> Map.Map FilePath ObjectHash -> [String]
listNewFiles = mapDifferencesInfo "new file created: "

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
         else ("file changed: " ++ path) : xs)
    []
    changedList
  where
    changedList = Map.toList $ Map.intersectionWith (==) base other
