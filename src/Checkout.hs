module Checkout
  ( checkoutCommand
  ) where

import Codec.Compression.GZip
import Control.Monad (forM, forM_)
import qualified Data.ByteString.Lazy as Lazy
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

import DirTreeUtils
import Utils

-- | Checkouts commit with specified hash to particular directory
checkoutCommand :: String -> IO ()
checkoutCommand hash = execIfStore (execCheckout hash)

execCheckout :: String -> IO ()
execCheckout hash = do
  let commitFile = commitsDir </> hash
  commitExists <- doesFileExist commitFile
  if commitExists
    then do
      commit <- loadCommit commitFile
      hasObjects <- objectsExist $ extrasFromTree commit
      if hasObjects
        then do
          restoreObjects workingDir (pathExtrasFromTree commit)
          storeCommitHead hash
          putStrLn $ "Checkout: checked out commit " ++ hash
        else putStrLn "Checkout: unable to checkout commit: missing objects."
    else putStrLn "Checkout: unable to checkout commit: commit not found."

objectsExist :: [String] -> IO Bool
objectsExist hashes = do
  filesExist <- forM hashes $ \hash -> doesFileExist $ objectsDir </> hash
  return $ and filesExist

restoreObjects :: String -> [(FilePath, String)] -> IO ()
restoreObjects targetDir objs = forM_ objs (restoreObject targetDir)

-- | Reads and restores specified object
restoreObject :: String -> (FilePath, String) -> IO ()
restoreObject targetDir (filepath, hash) = do
  let targetPath = targetDir </> filepath
  compressedContents <- Lazy.readFile (objectsDir </> hash)
  createDirectoryIfMissing True (takeDirectory targetPath)
  Lazy.writeFile targetPath (decompress compressedContents)
