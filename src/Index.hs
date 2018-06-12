module Index
  ( indexAddCommand
  ) where

import Data.List (nub)
import Hashing (FileWithHash(..), hashFile)
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified System.IO.Strict as S
import Utils (execIfStore, indexPath)

indexAddCommand :: FilePath -> IO ()
indexAddCommand path = execIfStore (addPathToIndex path)

addPathToIndex :: FilePath -> IO ()
addPathToIndex path = do
  index <- loadIndex indexPath
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  print isFile

addFileToIndex :: FilePath -> [FileWithHash] -> IO [FileWithHash]
addFileToIndex path index = do
  hash <- hashFile path
  let file = FileWithHash path hash
  let indexUpdated = nub $ file : index
  return indexUpdated

loadIndex :: FilePath -> IO [FileWithHash]
loadIndex path = do
  contents <- S.readFile path
  if not $ null contents
    then return $ read $ head (lines contents)
    else return []
