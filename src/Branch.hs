{-|
Module      : Branch
Description : Handles branching - creating a branch, synchronizing refs with HEAD
Copyright   : (c) Paweł Rzepiński 2018
License     :  BSD 3
Maintainer  : rzepinski.pawel@email.com
Stability   : experimental
Portability : POSIX
-}
module Branch
  ( readHeadCommit
  , storeHeadCommit
  , createBranch
  , initHead
  , branchCommand
  ) where

import Control.Monad (forM_)
import Data.List (isPrefixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified System.IO.Strict as S (readFile)

import Utils

type BranchName = String

-- | Lists existing branches
branchCommand :: IO ()
branchCommand = executeIfInitialized listBranches

listBranches :: IO ()
listBranches = do
  branches <- listDirectory refsDir
  headRef <- S.readFile headPath
  forM_ branches $
    (\name -> do
       if ("refs" </> name == headRef)
         then putStrLn $ "* " ++ name
         else putStrLn $ "  " ++ name)

initBranchName :: BranchName
initBranchName = "master"

-- | Creates branch ref.
createBranch :: BranchName -> ShaHash -> IO ()
createBranch name hash = writeFile (refsDir </> name) hash

-- |Initializes HEAD file with master branch.
initHead :: IO ()
initHead = do
  createBranch initBranchName ""
  writeFile headPath $ "refs" </> initBranchName

-- | Reads commit ref from HEAD file. 
-- Reference types:
--    0ccd3a1dacc3aa196c00b847d0134dba9adc9094
--    refs/master
readHeadCommit :: IO ShaHash
readHeadCommit = do
  headRef <- S.readFile headPath
  if "refs" `isPrefixOf` headRef
    then S.readFile $ hagitDir </> headRef
    else return headRef

-- | Stores ref to newest commit in HEAD file.
storeHeadCommit :: ShaHash -> IO ()
storeHeadCommit hash = do
  headRef <- S.readFile headPath
  if "refs" `isPrefixOf` headRef
    then writeFile (hagitDir </> headRef) hash
    else writeFile headPath hash
