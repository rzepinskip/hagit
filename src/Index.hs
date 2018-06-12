module Index
  ( indexAddCommand
  , indexRemoveCommand
  , loadIndex
  , formatPath
  ) where

import Data.List (nub)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), joinPath, splitPath)
import System.IO (IOMode(..), hPutStrLn, withFile)
import qualified System.IO.Strict as S

import Hashing (FileWithHash(..), toFileWithHash)
import Utils (execIfStore, indexPath, readDirectoryRec, workingDir)

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
  index <- loadIndex
  filesPaths <- readFilesFromPath path
  filesWithHashes <- mapM toFileWithHash filesPaths
  return (index, filesWithHashes)

loadIndex :: IO [FileWithHash]
loadIndex = do
  contents <- S.readFile indexPath
  if not (null contents)
    then return $ read $ head (lines contents)
    else return []

readFilesFromPath :: FilePath -> IO [FilePath]
readFilesFromPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  if isDir
    then map (formatPath path) <$> readDirectoryRec path
    else if isFile
           then return [formatPath workingDir path]
           else return []

formatPath :: FilePath -> FilePath -> FilePath
formatPath dir filePath = do
  let tailedDir = joinPath $ tail $ splitPath dir
  workingDir </> tailedDir </> filePath
