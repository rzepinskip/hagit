module Branch
  ( readHeadCommit
  , storeHeadCommit
  , createBranch
  , initHead
  , branchCommand
  ) where

import Data.List (isPrefixOf)
import System.FilePath ((</>))
import qualified System.IO.Strict as S (readFile)

import Utils

type BranchName = String

branchCommand :: String -> IO ()
branchCommand name = execIfStore $ execBranch name

execBranch :: BranchName -> IO ()
execBranch name = do
  hash <- readHeadCommit
  createBranch name hash

initBranchName :: BranchName
initBranchName = "master"

createBranch :: BranchName -> ShaHash -> IO ()
createBranch name hash = do
  writeFile (refsDir </> name) hash

initHead :: IO ()
initHead = do
  createBranch initBranchName ""
  writeFile headPath $ "refs" </> initBranchName

readHeadCommit :: IO ShaHash
readHeadCommit = do
  headRef <- S.readFile headPath
  if "refs" `isPrefixOf` headRef
    then S.readFile $ hagitDir </> headRef
    else return headRef

storeHeadCommit :: ShaHash -> IO ()
storeHeadCommit hash = do
  headRef <- S.readFile headPath
  if "refs" `isPrefixOf` headRef
    then writeFile (hagitDir </> headRef) hash
    else writeFile headPath hash
