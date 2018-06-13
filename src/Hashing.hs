{-|
Module      : Hashing
Description : Exports functions for hashing with SHA1
Copyright   : (c) Paweł Rzepiński 2018
License     :  BSD 3
Maintainer  : rzepinski.pawel@email.com
Stability   : experimental
Portability : POSIX
-}
module Hashing
  ( hashFile
  , hashString
  , bsToHex
  , ShaHash
  ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Strict8
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)

-- | SHA1 hash
type ShaHash = String

-- | Converts ByteString to its' hexadecimal representation.
bsToHex :: Strict.ByteString -> String
bsToHex bytes = concatMap (printf "%02x") (Strict.unpack bytes)

-- | Hashes specified string using SHA1.
hashString :: String -> Strict.ByteString
hashString s = SHA1.hash $ Strict8.pack s

-- | Hashes specified file using SHA1.
hashFile :: FilePath -> IO ShaHash
hashFile path = do
  content <- Lazy.readFile path
  let hash = bsToHex $ hashLazyBS content
  return hash

hashLazyBS :: Lazy.ByteString -> Strict.ByteString
hashLazyBS = SHA1.hashlazy
