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
checkoutCommand :: FilePath -> String -> IO ()
checkoutCommand dir hash = execIfStore dir (execCheckout dir hash)

execCheckout :: FilePath -> String -> IO ()
execCheckout dir hash = do
  let commitFile = commitsDir dir </> hash
  commitExists <- doesFileExist commitFile
  if commitExists
    then do
      commit <- loadCommit commitFile
      hasObjects <- objectsExist (objectsDir dir) (extrasFromTree commit)
      if hasObjects
        then do
          restoreObjects dir (pathExtrasFromTree commit)
          storeCommitHead dir hash
          putStrLn $ "Checkout: checked out commit " ++ hash
        else putStrLn "Checkout: unable to checkout commit: missing objects."
    else putStrLn "Checkout: unable to checkout commit: commit not found."

objectsExist :: String -> [String] -> IO Bool
objectsExist dir hashes = do
  filesExist <- forM hashes $ \hash -> doesFileExist $ dir </> hash
  return $ and filesExist

restoreObjects :: String -> [(FilePath, String)] -> IO ()
restoreObjects dir objs = forM_ objs (restoreObject dir)

-- | Reads and restores specified object
restoreObject :: String -> (FilePath, String) -> IO ()
restoreObject dir (filepath, hash) = do
  let targetPath = dir </> filepath
  compressedContents <- Lazy.readFile (objectsDir dir </> hash)
  createDirectoryIfMissing True (takeDirectory targetPath)
  Lazy.writeFile targetPath (decompress compressedContents)
