module Checkout
  ( checkoutCommand
  ) where

import Conduit ((.|), liftIO, mapM_C, runConduit, yieldMany)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as Lazy
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

import Branch (storeHeadCommit)
import Commit (loadCommit)
import Hashing (FileWithHash(..))
import Utils

-- | Checkouts commit with specified hash to particular directory
checkoutCommand :: ObjectHash -> IO ()
checkoutCommand hash = execIfStore $ execCheckout hash

execCheckout :: ObjectHash -> IO ()
execCheckout hash = do
  let commitFile = commitsDir </> hash
  commitExists <- doesFileExist commitFile
  if commitExists
    then do
      commitFiles <- loadCommit commitFile
      hasObjects <- doesAllObjectsExist $ map getContentHash commitFiles
      if hasObjects
        then do
          runConduit $ yieldMany commitFiles .| mapM_C (liftIO . restoreObject)
          restoreIndex commitFiles
          storeHeadCommit hash
          putStrLn $ "Checkout: checked out commit " ++ hash
        else putStrLn "Checkout: unable to checkout commit: missing objects."
    else putStrLn "Checkout: unable to checkout commit: commit not found."

doesAllObjectsExist :: [ObjectHash] -> IO Bool
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
