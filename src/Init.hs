module Init
  ( initCommand
  ) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import Commit (storeCommit)
import Utils

-- | Initializes hagit repository in specified directory
initCommand :: IO ()
initCommand = do
  exists <- doesDirectoryExist hagitDir
  if exists
    then putStrLn "Directory is already initialized."
    else do
      createDirectoryIfMissing True objectsDir
      createDirectoryIfMissing True commitsDir
      writeFile indexPath ""
      _ <- storeCommit "Initial commit." []
      putStrLn "Init: directory initialized."
