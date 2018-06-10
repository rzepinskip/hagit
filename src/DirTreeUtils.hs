module DirTreeUtils
  ( treeFromDir
  , emptyTreeDir
  , hcEmptyTreeDir
  , extrasFromTree
  , pathExtrasFromTree
  , treeHash
  , removeByteStrings
  , HashContents
  , hcHash
  , hcContents
  ) where

import Control.Monad (forM)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.List
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), makeRelative, normalise)

import DirTree
import Hash

type HashContents = (String, Lazy.ByteString)

hcHash :: HashContents -> String
hcHash = fst

hcContents :: HashContents -> Lazy.ByteString
hcContents = snd

removeByteStrings :: DirTree HashContents -> DirTree String
removeByteStrings = foldDirTree DirNode id (\p hc -> FileNode p (hcHash hc))

treeHash :: DirTree String -> String
treeHash tree = bsToHex $ hashBS (foldr Strict.append Strict.empty pairHashes)
  where
    pairHashes =
      foldDirTree
        (\_ x -> x)
        concat
        (\p h -> [hashString (h ++ bsToHex (hashString p))])
        tree

extrasFromTree :: DirTree a -> [a]
extrasFromTree = foldDirTree (\_ x -> x) concat (\_ e -> [e])

pathExtrasFromTree :: DirTree a -> [(FilePath, a)]
pathExtrasFromTree = foldDirTree (\_ x -> x) concat (\p e -> [(p, e)])

-- TODO: ignore files based on extension
ignoredPath :: FilePath -> Bool
ignoredPath path = path `elem` [".", "..", ".hagit", ".git", ".stack-work"]

makeTreeRelative :: FilePath -> DirTree a -> DirTree a
makeTreeRelative path =
  foldDirTree (DirNode . makeRelative path) id (FileNode . makeRelative path)

treeFromDir :: FilePath -> IO (DirTree HashContents)
treeFromDir dir = makeTreeRelative dir <$> treeFromDirAbs dir

loadFileData :: FilePath -> IO HashContents
loadFileData path = do
  contents <- Lazy.readFile path
  let hash = bsToHex $ hashLazyBS contents
  return (hash, contents)

treeFromDirAbs :: FilePath -> IO (DirTree HashContents)
treeFromDirAbs dir = do
  let normDir = normalise dir
  contents <- getDirectoryContents normDir
  let filteredContents = filter (not . ignoredPath) contents
  nodes <-
    forM (sort filteredContents) $ \name -> do
      let path = normDir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then treeFromDirAbs path
        else FileNode path <$> loadFileData path
  return (DirNode normDir nodes)

emptyTreeDir :: DirTree String
emptyTreeDir = DirNode "." []

hcEmptyTreeDir :: DirTree HashContents
hcEmptyTreeDir = DirNode "." []
