module Branch
  ( readHeadCommit
  , storeHeadCommit
  , createBranch
  , initHead
  , branchCommand
  , createBranch
  ) where

import Control.Monad (forM_)
import Data.List (isPrefixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import qualified System.IO.Strict as S (readFile)

import Utils

type BranchName = String

branchCommand :: IO ()
branchCommand = execIfStore listBranches

listBranches :: IO ()
listBranches = do
  branches <- listDirectory refsDir
  headRef <- S.readFile headPath
  forM_ branches $
    (\name -> do
       if ("refs" </> name == headRef)
         then putStrLn $ "* " ++ name
         else putStrLn $ " " ++ name)

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
