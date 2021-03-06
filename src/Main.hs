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
main = getArgs >>= parseArgs >>= runAction

runAction :: ArgsResult -> IO ()
runAction (Error e) = printError e
runAction (Operation op) = runOperation op

runOperation :: OperationType -> IO ()
runOperation Branch = branchCommand
runOperation (Checkout commit) = checkoutCommand commit
runOperation (Commit msg) = commitCommand msg
runOperation (Diff paths) = diffCommand paths
runOperation Help = printHelp
runOperation (IndexAdd path) = indexAddCommand path
runOperation (IndexRemove path) = indexRemoveCommand path
runOperation Init = initCommand
runOperation Log = logCommand
runOperation Status = statusCommand

printError :: ErrorType -> IO ()
printError _ = putStrLn "There was an error"

printHelp :: IO ()
printHelp = do
  putStrLn "Usage: hagit <operation> [params]"
  putStrLn
    "Valid operations are: branch, checkout, commit, diff, help, add, remove, init, log, status"
  putStrLn "See README.md for more details."
