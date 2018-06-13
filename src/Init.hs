module Init
  ( initCommand
  ) where

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import Branch
import Commit (storeCommit)
import qualified Data.Map as M
import Utils

-- | Initializes hagit repository in specified directory
initCommand :: IO ()
initCommand = do
  exists <- doesDirectoryExist hagitDir
  if exists
    then putStrLn "Directory is already initialized."
    else do
      createDirectoryIfMissing True refsDir
      createDirectoryIfMissing True commitsDir
      createDirectoryIfMissing True objectsDir
      writeFile indexPath ""
      initHead
      _ <- storeCommit "Initial commit." M.empty
      putStrLn "Init: directory initialized."
