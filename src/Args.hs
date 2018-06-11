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
  | Commit String
  | Help
  | Checkout String
  | Log
  | Status

strToSimpleOp :: String -> HvcOperationType
strToSimpleOp "log" = Log
strToSimpleOp "init" = Init
strToSimpleOp "status" = Status
strToSimpleOp _ = Help

strToCompoundOp :: String -> String -> HvcOperationType
strToCompoundOp "checkout" param = Checkout param
strToCompoundOp "commit" param = Commit param
strToCompoundOp _ _ = Help

strToOp :: String -> Maybe String -> HvcOperationType
strToOp command Nothing = strToSimpleOp command
strToOp command (Just param) = strToCompoundOp command param

validateCommand :: String -> Maybe String -> IO HvcArgsResult
validateCommand command param =
  case operation of
    Help -> return (HvcOperation Help)
    _ -> do
      valid <- validDir workingDir
      if valid
        then return (HvcOperation operation)
        else return (HvcError $ DirError "Invalid directory")
  where
    operation = strToOp command param

parseArgs :: [String] -> IO HvcArgsResult
parseArgs ["help"] = return (HvcOperation Help)
parseArgs [command] = validateCommand command Nothing
parseArgs [command, param] = validateCommand command (Just param)
parseArgs _ = return (HvcOperation Help)

validDir :: FilePath -> IO Bool
validDir fp = do
  exists <- doesDirectoryExist fp
  if exists
    then do
      permissions <- getPermissions fp
      return (readable permissions && writable permissions)
    else return False
