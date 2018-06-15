{-|
Module      : Args
Description : Handles command line arguments parsing
Copyright   : (c) Paweł Rzepiński 2018
License     :  BSD 3
Maintainer  : rzepinski.pawel@email.com
Stability   : experimental
Portability : POSIX
-}
module Args
  ( ArgsResult(..)
  , ErrorType(..)
  , OperationType(..)
  , parseArgs
  ) where

import System.Directory (doesDirectoryExist, getPermissions, readable, writable)
import Utils

-- | Result of parsing supplied arguments.
data ArgsResult
  = Error ErrorType
  | Operation OperationType

-- | Encountered error type.
newtype ErrorType =
  DirError String

-- | Type of program command.
data OperationType
  = Init
  | IndexAdd FilePath
  | IndexRemove FilePath
  | Commit String
  | Checkout String
  | Diff [String]
  | Branch
  | Log
  | Status
  | Help

-- | Parses arguments supplied to program.
parseArgs :: [String] -> IO ArgsResult
parseArgs [] = return (Operation Help)
parseArgs ["help"] = return (Operation Help)
parseArgs [command] = validateCommand command []
parseArgs [command, param] = validateCommand command [param]
parseArgs manyParams = validateCommand (head manyParams) (tail manyParams)

validateCommand :: String -> [String] -> IO ArgsResult
validateCommand command params =
  case operation of
    Help -> return (Operation Help)
    _ -> do
      valid <- validDir workingDir
      if valid
        then return (Operation operation)
        else return
               (Error $ DirError "Wrong permissions in the working directory")
  where
    operation = parseOp command params

validDir :: FilePath -> IO Bool
validDir fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      permissions <- getPermissions fp
      return (readable permissions && writable permissions)
    else return False

parseOp :: String -> [String] -> OperationType
parseOp command [] = parseSimpleOp command
parseOp command [param] = parseParamOp command param
parseOp command params = parseMultipleParamsOp command params

parseSimpleOp :: String -> OperationType
parseSimpleOp "branch" = Branch
parseSimpleOp "init" = Init
parseSimpleOp "log" = Log
parseSimpleOp "status" = Status
parseSimpleOp _ = Help

parseParamOp :: String -> String -> OperationType
parseParamOp "add" param = IndexAdd param
parseParamOp "checkout" param = Checkout param
parseParamOp "commit" param = Commit param
parseParamOp "diff" param = Diff [param]
parseParamOp "remove" param = IndexRemove param
parseParamOp _ _ = Help

parseMultipleParamsOp :: String -> [String] -> OperationType
parseMultipleParamsOp "diff" params = Diff params
parseMultipleParamsOp _ _ = Help
