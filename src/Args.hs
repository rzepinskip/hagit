module Args
  ( HvcArgsResult(..)
  , HvcErrorType(..)
  , HvcOperationType(..)
  , parseArgs
  ) where

import System.Directory
import Utils

data HvcArgsResult
  = HvcError HvcErrorType
  | HvcOperation HvcOperationType

newtype HvcErrorType =
  DirError String

data HvcOperationType
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

strToSimpleOp :: String -> HvcOperationType
strToSimpleOp "log" = Log
strToSimpleOp "init" = Init
strToSimpleOp "status" = Status
strToSimpleOp "branch" = Branch
strToSimpleOp _ = Help

strToParamOp :: String -> String -> HvcOperationType
strToParamOp "checkout" param = Checkout param
strToParamOp "add" param = IndexAdd param
strToParamOp "remove" param = IndexRemove param
strToParamOp "commit" param = Commit param
strToParamOp "diff" param = Diff [param]
strToParamOp _ _ = Help

strToParamsOp :: String -> [String] -> HvcOperationType
strToParamsOp "diff" params = Diff params
strToParamsOp _ _ = Help

strToOp :: String -> [String] -> HvcOperationType
strToOp command [] = strToSimpleOp command
strToOp command [param] = strToParamOp command param
strToOp command params = strToParamsOp command params

validateCommand :: String -> [String] -> IO HvcArgsResult
validateCommand command params =
  case operation of
    Help -> return (HvcOperation Help)
    _ -> do
      valid <- validDir workingDir
      if valid
        then return (HvcOperation operation)
        else return (HvcError $ DirError "Invalid directory")
  where
    operation = strToOp command params

parseArgs :: [String] -> IO HvcArgsResult
parseArgs [] = return (HvcOperation Help)
parseArgs ["help"] = return (HvcOperation Help)
parseArgs [command] = validateCommand command []
parseArgs [command, param] = validateCommand command [param]
parseArgs manyParams = validateCommand (head manyParams) (tail manyParams)

validDir :: FilePath -> IO Bool
validDir fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      permissions <- getPermissions fp
      return (readable permissions && writable permissions)
    else return False
