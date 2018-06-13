{-|
Module      : Main
Description : Entry point module
Copyright   : (c) Paweł Rzepiński 2018
License     :  BSD 3
Maintainer  : rzepinski.pawel@email.com
Stability   : experimental
Portability : POSIX
-}
module Main where

import System.Environment

import Args
import Branch
import Checkout
import Commit
import Diff
import Index
import Init
import Log
import Status

main :: IO ()
main = do
  args <- getArgs
  action <- parseArgs args
  runAction action

runAction :: ArgsResult -> IO ()
runAction (Error e) = printError e
runAction (Operation op) = runOperation op

runOperation :: OperationType -> IO ()
runOperation Init = initCommand
runOperation (IndexAdd path) = indexAddCommand path
runOperation (IndexRemove path) = indexRemoveCommand path
runOperation (Commit msg) = commitCommand msg
runOperation (Checkout commit) = checkoutCommand commit
runOperation (Diff paths) = diffCommand paths
runOperation Branch = branchCommand
runOperation Log = logCommand
runOperation Status = statusCommand
runOperation Help = printHelp

printError :: ErrorType -> IO ()
printError _ = putStrLn "There was an error"

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: hagit <operation> [params]"
  putStrLn
    "Valid operations are: init, commit, checkout, log, add, remove, status, branch, diff, help"
  putStrLn "See README.md for more details."
