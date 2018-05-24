{-# LANGUAGE BangPatterns, CPP #-}

module Data.Graph.PageRankStep (
  pageRank,
)
  where

import           Data.Graph.InternalPageRank
import           Control.Monad
import           Data.Graph.NeighborGraph
import Data.Util.ArrayUtils

import           Data.IORef
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import           Debug.Trace

import qualified Data.Array.Repa.Eval.Gang   as RG


{-# INLINE vertexLoop #-}
vertexLoop ::
     PageRankArray
  -> PageRankArray
  -> Int
  -> U.Vector Int
  -> U.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> IO ()
  -- TODO: could it be that this method is so much better because it has a non-monadic fold to query its neighbors?
vertexLoop !prevPageRankArray !array !vertexId !nbrs !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst
  | lenNbrs U.! vertexId == 0 = MU.write array vertexId 0
  | otherwise = do
  newVertexValue <- ((+dConst) . (*dampingFactor)) <$> 
        U.foldM' (\acc i ->
          MU.read prevPageRankArray i >>= \pageRankValue ->
          return $ acc + (pageRankValue / lenNbrs U.! i)
          ) 0 nbrs

  oldVal <- MU.read prevPageRankArray vertexId
  MU.write array vertexId newVertexValue

  let vDiff = abs $ oldVal - newVertexValue
    in when (vDiff > threshold) $
      -- I ..think.. this is safe - since we only write during a graphLoop.
      writeIORef isAboveThreshold True

graphLoopIO :: (NeighborGraph g)
  => g
  -> PageRankArray
  -> PageRankArray
  -> U.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> Word
  -> Word
  -> IO (Word,PageRankResult)
  -- The bangpatterns are tested, following combination should be good
graphLoopIO !g' !prevPageRankArray !pageRankArray !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst !loopCounter !numIters 
  | numIters > 0 && numIters == loopCounter = (,) loopCounter <$> U.unsafeFreeze prevPageRankArray
  | otherwise = do
  writeIORef isAboveThreshold False
  let numberOfVertices = numVerts g'

  if numberOfVertices < minimumParallelizableWorkload
    then V.imapM_ (\vertexId nbrs ->
      vertexLoop prevPageRankArray pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
      ) (neighborsArray g')
    else RG.gangIO RG.theGang
      (\threadId ->
        let chunkStart = nstart numberOfVertices threadId
            chunkSize = (nstart numberOfVertices (succ threadId)) - chunkStart
        in V.imapM_ (\vertexId nbrs ->
              vertexLoop prevPageRankArray pageRankArray (chunkStart+vertexId) nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
              ) (V.slice chunkStart chunkSize (neighborsArray g'))
      )

  isAboveThreshold' <- readIORef isAboveThreshold

  if (isAboveThreshold' || (numIters > 0 && numIters > loopCounter))
    then do
      newPageRankArray <- MU.new (numVerts g')
      graphLoopIO g' pageRankArray newPageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst (succ loopCounter) numIters
    else (,) loopCounter <$> U.unsafeFreeze prevPageRankArray

pageRank :: (NeighborGraph g) => g -> g -> PRType -> PRType -> PRType -> Word -> IO PageRankResult
pageRank !g !g' !dampingFactor !threshold !prValue !numIters = do
  initPageRankArray <- MU.replicate (numVerts g) prValue

  pageRankArray <- MU.new (numVerts g)
  let !lenNbrs = U.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      !n = fromIntegral $
          U.foldl' (\acc value -> if value == 0 then acc else succ acc) 0 lenNbrs

  isAboveThreshold <- newIORef False

  (loops, result) <- graphLoopIO g' initPageRankArray pageRankArray lenNbrs isAboveThreshold threshold dampingFactor (dConst n) 0 numIters

#ifdef DEBUG
  traceM $ "looped (PageRankStep.hs) " ++ show loops
#endif
  return result
    where
      dConst n = (1-dampingFactor) / n
