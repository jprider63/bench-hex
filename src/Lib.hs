module Lib where

import Data.Word

-- hexNaive :: Word8 -> Word16
-- hexNaive c
--   | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
--   | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A'
--   | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a'
--   | otherwise            = 0xff


hexMatching :: Word8 -> Word8
hexMatching 48  = 0  -- '0'
hexMatching 49  = 1  -- '1'
hexMatching 50  = 2  -- '2'
hexMatching 51  = 3  -- '3'
hexMatching 52  = 4  -- '4'
hexMatching 53  = 5  -- '5'
hexMatching 54  = 6  -- '6'
hexMatching 55  = 7  -- '7'
hexMatching 56  = 8  -- '8'
hexMatching 57  = 9  -- '9'
hexMatching 65  = 10 -- 'A'
hexMatching 97  = 10 -- 'a'
hexMatching 66  = 11 -- 'B'
hexMatching 98  = 11 -- 'b'
hexMatching 67  = 12 -- 'C'
hexMatching 99  = 12 -- 'c'
hexMatching 68  = 13 -- 'D'
hexMatching 100 = 13 -- 'd'
hexMatching 69  = 14 -- 'E'
hexMatching 101 = 14 -- 'e'
hexMatching 70  = 15 -- 'F'
hexMatching 102 = 15 -- 'f'
hexMatching _ = 255

hexNaive :: Word8 -> Word8
hexNaive x
  | 48 <= x && x <=  57 = fromIntegral x - 48  -- 0-9
  | 65 <= x && x <=  70 = fromIntegral x - 55  -- A-F
  | 97 <= x && x <= 102 = fromIntegral x - 87  -- a-f
  | otherwise = 255

