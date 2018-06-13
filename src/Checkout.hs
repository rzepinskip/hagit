module Checkout
  ( checkoutCommand
  ) where

import Conduit ((.|), liftIO, mapM_C, runConduit, yieldMany)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as Lazy
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>), takeDirectory)
import qualified System.IO.Strict as S (readFile)

import Branch (createBranch, readHeadCommit, storeHeadCommit)
import Commit (loadCommit)
import Hashing (FileWithHash(..))
import Utils

-- | Checkouts commit with specified hash to particular directory
checkoutCommand :: String -> IO ()
checkoutCommand param = execIfStore $ execCheckout param

execCheckout :: String -> IO ()
execCheckout param = do
  isCommit <- isExistingCommit param
  isBranch <- isExistingBranch param
  if isCommit
    then do
      hash <- checkoutCommit param
      putStrLn $ "Checked out commit: " ++ hash
    else if isBranch
           then checkoutBranch param
           else checkoutNewBranch param

isExistingCommit :: String -> IO Bool
isExistingCommit name = doesFileExist $ commitsDir </> name

isExistingBranch :: String -> IO Bool
isExistingBranch name = do
  branches <- listDirectory refsDir
  return $ name `elem` branches

checkoutBranch :: String -> IO ()
checkoutBranch name = do
  lastCommit <- S.readFile $ refsDir </> name
  writeFile headPath $ "refs" </> name
  _ <- checkoutCommit lastCommit
  putStrLn $ "Checked out branch: " ++ name

checkoutNewBranch :: String -> IO ()
checkoutNewBranch name = do
  hash <- readHeadCommit
  createBranch name hash
  writeFile headPath $ "refs" </> name

checkoutCommit :: ShaHash -> IO ShaHash
checkoutCommit hash = do
  commitFiles <- loadCommit $ commitsDir </> hash
  hasObjects <- doesAllObjectsExist $ map getContentHash commitFiles
  if hasObjects
    then do
      runConduit $ yieldMany commitFiles .| mapM_C (liftIO . restoreObject)
      restoreIndex commitFiles
      storeHeadCommit hash
      return hash
    else return "Checkout: unable to checkout: missing objects."

doesAllObjectsExist :: [ShaHash] -> IO Bool
doesAllObjectsExist hashes = do
  filesExist <- forM hashes $ \hash -> doesFileExist $ objectsDir </> hash
  return $ and filesExist

-- | Reads and restores specified object
restoreObject :: FileWithHash -> IO ()
restoreObject obj = do
  let path = getPath obj
  let hash = getContentHash obj
  let targetPath = workingDir </> path
  content <- Lazy.readFile (objectsDir </> hash)
  createDirectoryIfMissing True (takeDirectory targetPath)
  Lazy.writeFile targetPath content

restoreIndex :: [FileWithHash] -> IO ()
restoreIndex files = writeFile indexPath $ show files
