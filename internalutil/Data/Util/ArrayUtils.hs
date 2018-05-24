{-# LANGUAGE BangPatterns        #-}
module Data.Util.ArrayUtils
  where

import qualified Data.Array.Repa.Eval.Gang as RG

nstart  :: Int -> Int -> Int
nstart !len !c
 = let  chunks          = RG.gangSize RG.theGang
        chunkLen        = len `quot` chunks
        chunkLeftover   = len `rem`  chunks

        getStart c'
         | c' < chunkLeftover  = c' * (chunkLen + 1)
         | otherwise           = c' * chunkLen  + chunkLeftover

  in    getStart c
