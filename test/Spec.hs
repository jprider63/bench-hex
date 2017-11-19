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
process hexF = foldl' (\acc c -> acc + hexF c) 0

main = defaultMain [
          bgroup "dataList"
                [ bench "baseline" $ whnf (process id) dataList
                , bench "match" $ whnf (process hexMatching) dataList
                , bench "sub" $ whnf (process hexNaive) dataList
                , bench "bq" $ whnf (process hexBranchQuick) dataList
                , bench "bf" $ whnf (process hexBranchFree) dataList
                ]
        , bgroup "dataList2"
                [ bench "baseline" $ whnf (process id) dataList2
                , bench "match" $ whnf (process hexMatching) dataList2
                , bench "sub" $ whnf (process hexNaive) dataList2
                , bench "bq" $ whnf (process hexBranchQuick) dataList2
                , bench "bf" $ whnf (process hexBranchFree) dataList2
                ]
        ]
