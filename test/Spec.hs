module Main where

import Data.Word
import qualified Data.ByteString as BS
import Criterion.Main
-- import qualified N -- naive computing
import Lib

import Data.List (foldl')

-- simple benchmark function
dataList, dataList2 :: [Word8]
dataList = take 100000 $ cycle [0 .. 255] -- ['\x1'..'\xf0']
dataList2 = take 100000 $ cycle $ BS.unpack "D34dBe3F"
dataList3 = take 100000 $ cycle $ [48 .. 102]
process hexF = foldl' (\acc c -> acc + hexF c) 0

fs =
  [ ("baseline", process id)
  , ("match"   , process hexMatching)
  , ("sub"     , process hexNaive)
  , ("bq"      , process hexBranchQuick)
  , ("bf"      , process hexBranchFree)
  ]

mkBench name x = bgroup name
  [ bench fname $ whnf f x | (fname, f) <- fs]


main = defaultMain
        [ mkBench "dataList" dataList
        , mkBench "dataList2" dataList2
        , mkBench "dataList3" dataList3
        ]
