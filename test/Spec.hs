module Main where

import Data.Word
import qualified Data.ByteString as BS
import Criterion.Main
-- import qualified N -- naive computing
import Lib

import Data.List (foldl', nub)

-- simple benchmark function
dataList, dataList2 :: [Word8]
dataList = take 100000 $ cycle [0 .. 255] -- ['\x1'..'\xf0']
dataList2 = take 100000 $ cycle $ BS.unpack "D34dBe3F"
dataList3 = take 100000 $ cycle $ [48 .. 102]
dataList4 = replicate 100000 0

process :: (a -> Word8) -> [a] -> Word8
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

sanityCheck =
  let results = (fmap . fmap) (\f -> f dataList) (drop 1 fs)
  in if length (nub (fmap snd results)) /= 1 then
    fail (show results)
  else return ()

main = do
  sanityCheck
  defaultMain
    [ mkBench "dataList" dataList
    , mkBench "dataList2" dataList2
    , mkBench "dataList3" dataList3
    , mkBench "dataList4" dataList4
    ]
