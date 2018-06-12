module Index
  ( indexAddCommand
  , indexRemoveCommand
  ) where

import Data.List (nub)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (IOMode(..), hPutStrLn, withFile)
import qualified System.IO.Strict as S

import Hashing (FileWithHash(..), toFileWithHash)
import Utils (execIfStore, indexPath, readDirectoryRec)

indexAddCommand :: FilePath -> IO ()
indexAddCommand path = execIfStore (addPathToIndex path)

indexRemoveCommand :: FilePath -> IO ()
indexRemoveCommand path = execIfStore (removePathFromIndex path)

addPathToIndex :: FilePath -> IO ()
addPathToIndex path = do
  (index, filesWithHashes) <- loadIndexAndFiles path
  let updatedIndex = nub $ filesWithHashes ++ index
  withFile indexPath WriteMode (`hPutStrLn` show updatedIndex)

removePathFromIndex :: FilePath -> IO ()
removePathFromIndex path = do
  (index, filesWithHashes) <- loadIndexAndFiles path
  let updatedIndex = filter (`notElem` filesWithHashes) index
  withFile indexPath WriteMode (`hPutStrLn` show updatedIndex)

loadIndexAndFiles :: FilePath -> IO ([FileWithHash], [FileWithHash])
loadIndexAndFiles path = do
  index <- loadIndex indexPath
  filesPaths <- readFilesFromPath path
  filesWithHashes <- mapM toFileWithHash filesPaths
  return (index, filesWithHashes)

loadIndex :: FilePath -> IO [FileWithHash]
loadIndex path = do
  contents <- S.readFile path
  if not (null contents)
    then return $ read $ head (lines contents)
    else return []

readFilesFromPath :: FilePath -> IO [FilePath]
readFilesFromPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
    then readDirectoryRec path
    else if isFile
           then return [path]
           else return []
