module Checkout
  ( checkoutCommand
  ) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString.Lazy as Lazy
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

import Utils

-- | Checkouts commit with specified hash to particular directory
checkoutCommand :: ObjectHash -> IO ()
checkoutCommand = execCheckout

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
          restoreObjects workingDir commitFiles
          storeCommitHead hash
          putStrLn $ "Checkout: checked out commit " ++ hash
        else putStrLn "Checkout: unable to checkout commit: missing objects."
    else putStrLn "Checkout: unable to checkout commit: commit not found."

doesAllObjectsExist :: [ObjectHash] -> IO Bool
doesAllObjectsExist hashes = do
  filesExist <- forM hashes $ \hash -> doesFileExist $ objectsDir </> hash
  return $ and filesExist

restoreObjects :: String -> [FileWithHash] -> IO ()
restoreObjects targetDir objs = forM_ objs (restoreObject targetDir)

-- | Reads and restores specified object
restoreObject :: String -> FileWithHash -> IO ()
restoreObject targetDir obj = do
  let path = getPath obj
  let hash = getContentHash obj
  let targetPath = targetDir </> path
  content <- Lazy.readFile (objectsDir </> hash)
  createDirectoryIfMissing True (takeDirectory targetPath)
  Lazy.writeFile targetPath content
