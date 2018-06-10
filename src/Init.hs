module Init
  ( initCommand
  ) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import Commit (storeCommit)
import DirTreeUtils (hcEmptyTreeDir)
import Utils

-- | Initializes hagit repository in specified directory
initCommand :: FilePath -> IO ()
initCommand dir = do
  exists <- doesDirectoryExist $ hagitDir dir
  if exists
    then putStrLn "Directory is already initialized."
    else do
      createDirectoryIfMissing True $ objectsDir dir
      createDirectoryIfMissing True $ commitsDir dir
      _ <- storeCommit dir "Initial commit." hcEmptyTreeDir
      putStrLn $ "Init: directory " ++ dir ++ " initialized."
