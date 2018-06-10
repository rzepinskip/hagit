module Main where

import System.Environment

import Args
import Checkout
import Commit
import Init
import Log
import Status

main :: IO ()
main = do
  args <- getArgs
  action <- parseArgs args
  runAction action

runAction :: HvcArgsResult -> IO ()
runAction (HvcError e) = printError e
runAction (HvcOperation op) = runOperation op

runOperation :: HvcOperationType -> IO ()
runOperation (Init dir) = initCommand dir
runOperation (Commit dir msg) = commitCommand dir msg
runOperation (Log dir) = logCommand dir
runOperation (Checkout dir commit) = checkoutCommand dir commit
runOperation (Status dir) = statusCommand dir
runOperation Help = printHelp

printError :: HvcErrorType -> IO ()
printError (DirError dir) = putStrLn $ "Invalid directory: " ++ dir

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: hagit <directory> <operation> [options]"
  putStrLn "Valid operations are: init, commit, checkout, log, status, help"
  putStrLn "See README.md for more details."
