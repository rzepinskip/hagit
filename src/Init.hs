{-|
Module      : Log
Description : Initializes hagit repository in working directory
Copyright   : (c) Paweł Rzepiński 2018
License     :  BSD 3
Maintainer  : rzepinski.pawel@email.com
Stability   : experimental
Portability : POSIX
-}
module Init
  ( initCommand
  ) where

import qualified Data.Map as M
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)

import Branch (initHead)
import Commit (storeCommit)
import Utils

-- | Initializes hagit repository in working directory
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
