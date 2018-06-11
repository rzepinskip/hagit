module Hashing
  ( hashFile
  , hashString
  , hashLazyBS
  , hashBS
  , bsToHex
  , ObjectHash
  ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Strict8
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)

type ObjectHash = String

bsToHex :: Strict.ByteString -> String
bsToHex bytes = concatMap (printf "%02x") (Strict.unpack bytes)

hashString :: String -> Strict.ByteString
hashString s = SHA1.hash $ Strict8.pack s

hashLazyBS :: Lazy.ByteString -> Strict.ByteString
hashLazyBS = SHA1.hashlazy

hashBS :: Strict.ByteString -> Strict.ByteString
hashBS = SHA1.hash

hashFile :: FilePath -> IO ObjectHash
hashFile path = do
  content <- Lazy.readFile path
  let hash = bsToHex $ hashLazyBS content
  return hash
