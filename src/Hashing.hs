module Hashing
  ( hashFile
  , toFileWithHash
  , hashString
  , hashLazyBS
  , hashBS
  , bsToHex
  , ShaHash
  , FileWithHash(..)
  , toFileWithHashTuple
  ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Strict8
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)

type ShaHash = String

data FileWithHash = FileWithHash
  { getPath :: FilePath
  , getContentHash :: ShaHash
  } deriving (Show, Read, Eq)

toFileWithHashTuple :: FileWithHash -> (FilePath, ShaHash)
toFileWithHashTuple file = (getPath file, getContentHash file)

bsToHex :: Strict.ByteString -> String
bsToHex bytes = concatMap (printf "%02x") (Strict.unpack bytes)

hashString :: String -> Strict.ByteString
hashString s = SHA1.hash $ Strict8.pack s

hashLazyBS :: Lazy.ByteString -> Strict.ByteString
hashLazyBS = SHA1.hashlazy

hashBS :: Strict.ByteString -> Strict.ByteString
hashBS = SHA1.hash

hashFile :: FilePath -> IO ShaHash
hashFile path = do
  content <- Lazy.readFile path
  let hash = bsToHex $ hashLazyBS content
  return hash

toFileWithHash :: FilePath -> IO FileWithHash
toFileWithHash path = FileWithHash path <$> hashFile path
