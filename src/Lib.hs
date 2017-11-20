{-# LANGUAGE MagicHash #-}

module Lib where

import Data.Bits
import qualified Data.Vector.Unboxed as V
import Data.Word
import GHC.Prim
import GHC.Word

-- http://tab.snarc.org/posts/haskell/2011-11-15-lookup-tables.html
--
-- hexNaive :: Word8 -> Word16
-- hexNaive c
--   | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
--   | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A'
--   | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a'
--   | otherwise            = 0xff

{-# INLINE hexMatching #-}
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
  | 48 <= x && x <=  57 = x - 48  -- 0-9
  | 65 <= x && x <=  70 = x - 55  -- A-F
  | 97 <= x && x <= 102 = x - 87  -- a-f
  | otherwise = 255

hexBranchQuick :: Word8 -> Word8
hexBranchQuick x
  | num < 10 = num
  | alpha < 6 = alpha + 10
  | otherwise = 255
  where
    num = x - 48
    alpha = (x .&. 0xdf) - 65

vtable = V.fromList table

hexUnsafeVector :: Word8 -> Word8
hexUnsafeVector c = let w = vtable `V.unsafeIndex` fromIntegral c in if w == 0xff then error ("bad: " ++ show c) else w

__ = 0xff
table :: [Word8]
table = [
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
         0, 1, 2, 3, 4, 5, 6, 7,  8, 9,__,__,__,__,__,__,
        __,10,11,12,13,14,15,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,10,11,12,13,14,15,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,

        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__,
        __,__,__,__,__,__,__,__, __,__,__,__,__,__,__,__ ]

hexBranchFree :: Word8 -> Word8
hexBranchFree (W8# w#) =
  let num# = word2Int# w# -# 0x30#
      isNum# = ltWord# (int2Word# num#) (int2Word# 10#)
      alpha# = (andI# 0xdf# (word2Int# w#)) -# 0x41#
      isAlpha# = ltWord# (int2Word# alpha#) (int2Word# 6#)
  in
   W8#
    (narrow8Word# (int2Word# (orI#
      (orI#
        (isNum# *# num#)
        (isAlpha# *# (10# +# alpha#)))
      ((orI# isAlpha# isNum#) -# 1#))))
  where
    x# *# y# = andI# (negateInt# x#) y#
{-# INLINE hexBranchFree #-}
