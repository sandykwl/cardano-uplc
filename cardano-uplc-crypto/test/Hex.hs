module Hex (hex) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt)

-- | Bytes from a string of hex digits.
hex :: String -> ByteString
hex = BS.pack . go
  where
    go (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go [] = []
    go [_] = error "hex: odd number of digits"
