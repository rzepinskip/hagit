module Index
  ( indexAddCommand
  , indexRemoveCommand
  , loadIndex
  , mergeIndexWith
  , removeFromIndex
  ) where

import Conduit ((.|), liftIO, mapMC, mapM_C, runConduit, sinkList, yieldMany)
import qualified Data.ByteString.Lazy as Lazy (readFile, writeFile)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import System.FilePath ((</>), joinPath, splitPath)
import qualified System.IO.Strict as S (readFile)

import Hashing (ShaHash, hashFile)
import Utils (execIfStore, indexPath, objectsDir, readDirectoryRec, workingDir)

indexAddCommand :: FilePath -> IO ()
indexAddCommand path = execIfStore (addPathToIndex path)

addPathToIndex :: FilePath -> IO ()
addPathToIndex path = do
  index <- loadIndex
  workFilesPaths <- readFilesFromPath path
  work <- storeObjects workFilesPaths
  let updatedIndex = mergeIndexWith index work
  writeFile indexPath $ show updatedIndex

mergeIndexWith ::
     M.Map FilePath ShaHash -> [(FilePath, ShaHash)] -> M.Map FilePath ShaHash
mergeIndexWith index work = M.union workMap index
  where
    workMap = M.fromList work

loadIndex :: IO (M.Map FilePath ShaHash)
loadIndex = do
  contents <- S.readFile indexPath
  if not (null contents)
    then return $ read $ head (lines contents)
    else return M.empty

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

storeObjects :: [FilePath] -> IO [(FilePath, ShaHash)]
storeObjects files =
  runConduit $ yieldMany files .| mapMC (liftIO . storeObject) .| sinkList

storeObject :: FilePath -> IO (FilePath, ShaHash)
storeObject path = do
  hash <- hashFile path
  let res = (path, hash)
  let finalName = objectsDir </> hash
  exists <- doesFileExist finalName
  if exists
    then return res
    else do
      content <- Lazy.readFile path
      Lazy.writeFile finalName content
      return res

indexRemoveCommand :: FilePath -> IO ()
indexRemoveCommand path = execIfStore (removePathsFromIndex path)

removePathsFromIndex :: FilePath -> IO ()
removePathsFromIndex path = do
  index <- loadIndex
  removedFiles <- readFilesFromPath path
  let removedHashes = catMaybes $ map (`M.lookup` index) removedFiles
  runConduit $ yieldMany removedHashes .| mapM_C removeObject
  let updatedIndex = removeFromIndex index removedFiles
  writeFile indexPath $ show $ updatedIndex

removeFromIndex ::
     M.Map FilePath ShaHash -> [FilePath] -> M.Map FilePath ShaHash
removeFromIndex index paths = M.difference index pathsMap
  where
    pathsMap = M.fromList $ zip paths [[] | _ <- [1 ..]]

removeObject :: ShaHash -> IO ()
removeObject hash = removeFile $ objectsDir </> hash
