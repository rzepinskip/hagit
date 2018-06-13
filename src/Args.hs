module Args
  ( ArgsResult(..)
  , ErrorType(..)
  , OperationType(..)
  , parseArgs
  ) where

import System.Directory (doesDirectoryExist, getPermissions, readable, writable)
import Utils

data ArgsResult
  = Error ErrorType
  | Operation OperationType

newtype ErrorType =
  DirError String

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
        else return (Error $ DirError "Invalid directory")
  where
    operation = strToOp command params

validDir :: FilePath -> IO Bool
validDir fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      permissions <- getPermissions fp
      return (readable permissions && writable permissions)
    else return False

strToOp :: String -> [String] -> OperationType
strToOp command [] = strToSimpleOp command
strToOp command [param] = strToParamOp command param
strToOp command params = strToParamsOp command params

strToSimpleOp :: String -> OperationType
strToSimpleOp "log" = Log
strToSimpleOp "init" = Init
strToSimpleOp "status" = Status
strToSimpleOp "branch" = Branch
strToSimpleOp _ = Help

strToParamOp :: String -> String -> OperationType
strToParamOp "checkout" param = Checkout param
strToParamOp "add" param = IndexAdd param
strToParamOp "remove" param = IndexRemove param
strToParamOp "commit" param = Commit param
strToParamOp "diff" param = Diff [param]
strToParamOp _ _ = Help

strToParamsOp :: String -> [String] -> OperationType
strToParamsOp "diff" params = Diff params
strToParamsOp _ _ = Help
