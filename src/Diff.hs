-----------------------------------------------------------------------------
-- Based on Data.Algorithm.DiffOutput module
--  ((c) Sterling Clover 2008-2011, Kevin Charter 2011) 
-- available at https://hub.darcs.net/sterlingclover/Diff/browse/src/Data/Algorithm/DiffOutput.hs
-----------------------------------------------------------------------------
module Diff
  ( mergeFiles
  , diffCommand
  ) where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput (ppDiff)
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.FilePath (takeFileName)
import System.FilePath ((</>))
import qualified System.IO.Strict as S (readFile)

import Index (loadIndex)
import Utils

diffCommand :: [FilePath] -> IO ()
diffCommand paths = execIfStore $ execDiff paths

execDiff :: [String] -> IO ()
execDiff paths = do
  mapM_ diffFile paths

diffFile :: FilePath -> IO ()
diffFile path = do
  currentContent <- S.readFile path
  index <- loadIndex
  let indexedHash = fromJust $ M.lookup (workingDir </> path) index
  indexedContent <- S.readFile $ objectsDir </> indexedHash
  putStrLn $ "FILE: " ++ path
  putStrLn $
    ppDiff $ getGroupedDiff (lines currentContent) (lines indexedContent)

mergeFiles :: FilePath -> FilePath -> IO ()
mergeFiles current merged = do
  newContent <- getFileDiffs current merged
  writeFile current newContent

getFileDiffs :: FilePath -> FilePath -> IO String
getFileDiffs current merged = do
  currentContent <- S.readFile current
  mergedContent <- S.readFile merged
  let currentName = takeFileName current
  let mergedName = takeFileName merged
  let lineDiffs =
        diffToLineRanges $
        getGroupedDiff (lines currentContent) (lines mergedContent)
  let res =
        intercalate
          "\n"
          (concatMap (concatDiffs currentName mergedName) lineDiffs)
  return res

-- | Line number alias
type LineNo = Int

-- | Line Range: start, end and contents
data LineRange = LineRange
  { lrNumbers :: (LineNo, LineNo)
  , lrContents :: [String]
  } deriving (Show, Read, Eq, Ord)

-- | Diff Operation representing changes to apply
data DiffOperation a
  = Deletion a
             LineNo
  | Addition a
             LineNo
  | Change a
           a
  | Same a
  deriving (Show, Read, Eq, Ord)

concatDiffs :: String -> String -> DiffOperation LineRange -> [String]
concatDiffs currentName otherName operation =
  case operation of
    (Same range) -> lrContents range
    (Addition range _) -> prefix ++ separator ++ lrContents range ++ suffix
    (Deletion range _) -> prefix ++ lrContents range ++ separator ++ suffix
    (Change range1 range2) ->
      prefix ++ lrContents range1 ++ separator ++ lrContents range2 ++ suffix
  where
    prefix = ["<<<<<<< " ++ currentName]
    separator = ["======="]
    suffix = [">>>>>>> " ++ otherName]

diffToLineRanges :: [Diff [String]] -> [DiffOperation LineRange]
diffToLineRanges = toLineRange 1 1
  where
    toLineRange :: Int -> Int -> [Diff [String]] -> [DiffOperation LineRange]
    toLineRange _ _ [] = []
    toLineRange leftLine rightLine (Both ls _:rs) =
      let lins = length ls
          diff = Same (LineRange (leftLine, leftLine + lins - 1) ls)
       in diff : toLineRange (leftLine + lins) (rightLine + lins) rs
    toLineRange leftLine rightLine (Second lsS:First lsF:rs) =
      toChange leftLine rightLine lsF lsS rs
    toLineRange leftLine rightLine (First lsF:Second lsS:rs) =
      toChange leftLine rightLine lsF lsS rs
    toLineRange leftLine rightLine (Second lsS:rs) =
      let linesS = length lsS
          diff =
            Addition
              (LineRange (rightLine, rightLine + linesS - 1) lsS)
              (leftLine - 1)
       in diff : toLineRange leftLine (rightLine + linesS) rs
    toLineRange leftLine rightLine (First lsF:rs) =
      let linesF = length lsF
          diff =
            Deletion
              (LineRange (leftLine, leftLine + linesF - 1) lsF)
              (rightLine - 1)
       in diff : toLineRange (leftLine + linesF) rightLine rs
    toChange leftLine rightLine lsF lsS rs =
      let linesS = length lsS
          linesF = length lsF
       in Change
            (LineRange (leftLine, leftLine + linesF - 1) lsF)
            (LineRange (rightLine, rightLine + linesS - 1) lsS) :
          toLineRange (leftLine + linesF) (rightLine + linesS) rs
