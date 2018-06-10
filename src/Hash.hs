module Hash
  ( fileSHA1
  , strSHA1
  , lbstrSHA1
  , bstrSHA1
  , bstrToHex
  ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Strict8
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)

bstrToHex :: Strict.ByteString -> String
bstrToHex bytes = concatMap (printf "%02x") (Strict.unpack bytes)

strSHA1 :: String -> Strict.ByteString
strSHA1 s = SHA1.hash $ Strict8.pack s

lbstrSHA1 :: Lazy.ByteString -> Strict.ByteString
lbstrSHA1 = SHA1.hashlazy

bstrSHA1 :: Strict.ByteString -> Strict.ByteString
bstrSHA1 = SHA1.hash

fileSHA1 :: FilePath -> IO Strict.ByteString
fileSHA1 file = lbstrSHA1 <$> Lazy.readFile file
