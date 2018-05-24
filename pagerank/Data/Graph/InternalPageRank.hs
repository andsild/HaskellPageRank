{-# LANGUAGE BangPatterns        #-}

module Data.Graph.InternalPageRank
  where

import Data.Graph.NeighborGraph (NodeID)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Control.Monad.ST
import Control.Concurrent.MVar (MVar)

type PageRankArray  = MU.IOVector PRType
type PageRankArrayLocked  = V.Vector (MVar PRType)
type PageRankResult  = U.Vector PRType
type SafePageRankArray s = MU.STVector s PRType
type PRType = Double

defaultPrValue :: PRType
defaultPrValue = 1.0

defaultDampingFactor :: PRType
defaultDampingFactor = 0.15

minimumParallelizableWorkload :: Int
minimumParallelizableWorkload = 7000

{-# INLINE writeMutexLocked #-}
writeMutexLocked :: U.Vector PRType
  -> U.Vector NodeID
  -> V.Vector PRType
  -> PRType
  -> PRType
  -> IO PRType
writeMutexLocked array nbrs lenNbrs dampingFactor dConst = do
  rhs <- U.foldM' (\acc (i, pageRankValue) ->
    let numNbrs = lenNbrs V.! i in
    return $ acc + (pageRankValue / numNbrs))
      0 (U.zip nbrs array)
  return $ dConst + (dampingFactor * rhs)


{-# INLINE writeMutexIO #-}
writeMutexIO :: PageRankArray
  -> U.Vector NodeID
  -> U.Vector PRType
  -> PRType
  -> PRType
  -> IO PRType
writeMutexIO array nbrs lenNbrs dampingFactor dConst = do
  rhs <- U.foldM' (\acc i -> do
    pageRankValue <- MU.read array i
    let numNbrs = lenNbrs U.! i
    return $ acc + (pageRankValue / numNbrs))
      0 nbrs
  return $ dConst + (dampingFactor * rhs)

{-# INLINE writeMutexST #-}
writeMutexST ::
     SafePageRankArray s
  -> U.Vector NodeID
  -> U.Vector PRType
  -> PRType
  -> PRType
  -> ST s (PRType)
writeMutexST array nbrs lenNbrs dampingFactor dConst = do
  rhs <- U.foldM' (\acc i -> do
    pageRankValue <- MU.read array i
    let numNbrs = lenNbrs U.! i
    return $ acc + (pageRankValue / numNbrs))
      0 nbrs
  return $ dConst + (dampingFactor * rhs)
