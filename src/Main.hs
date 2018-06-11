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
runOperation Init = initCommand
runOperation (Commit msg) = commitCommand msg
runOperation Log = logCommand
runOperation (Checkout commit) = checkoutCommand commit
runOperation Status = statusCommand
runOperation Help = printHelp

printError :: HvcErrorType -> IO ()
printError _ = putStrLn "There was an error"

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: hagit <operation> [options]"
  putStrLn "Valid operations are: init, commit, checkout, log, status, help"
  putStrLn "See README.md for more details."
