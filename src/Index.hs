module Index
  ( indexAddCommand
  , indexRemoveCommand
  , loadIndex
  ) where

import Conduit ((.|), liftIO, mapMC, mapM_C, runConduit, sinkList, yieldMany)
import qualified Data.ByteString.Lazy as Lazy (readFile, writeFile)
import Data.List ((\\), nub)
import System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath ((</>), joinPath, splitPath)
import qualified System.IO.Strict as S (readFile)

import Hashing (FileWithHash(..), hashFile)
import Utils (execIfStore, indexPath, objectsDir, readDirectoryRec, workingDir)

indexAddCommand :: FilePath -> IO ()
indexAddCommand path = execIfStore (addPathToIndex path)

addPathToIndex :: FilePath -> IO ()
addPathToIndex path = do
  index <- loadIndex
  filesPaths <- readFilesFromPath path
  filesWithHashes <- storeObjects filesPaths
  let updatedIndex = nub $ filesWithHashes ++ index
  writeFile indexPath $ show updatedIndex

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

storeObjects :: [FilePath] -> IO [FileWithHash]
storeObjects files =
  runConduit $ yieldMany files .| mapMC (liftIO . storeObject) .| sinkList

storeObject :: FilePath -> IO FileWithHash
storeObject path = do
  hash <- hashFile path
  let res = FileWithHash path hash
  let finalName = objectsDir </> hash
  exists <- doesFileExist finalName
  if exists
    then return res
    else do
      content <- Lazy.readFile path
      Lazy.writeFile finalName content
      return res

indexRemoveCommand :: FilePath -> IO ()
indexRemoveCommand path = execIfStore (removePathFromIndex path)

removePathFromIndex :: FilePath -> IO ()
removePathFromIndex path = do
  index <- loadIndex
  removedFiles <- readFilesFromPath path
  let objectsToRemove = filter (\file -> getPath file `elem` removedFiles) index
  runConduit $ yieldMany objectsToRemove .| mapM_C removeObject
  let updatedIndex = index \\ objectsToRemove
  writeFile indexPath $ show updatedIndex

removeObject :: FileWithHash -> IO ()
removeObject file = removeFile $ objectsDir </> getContentHash file
