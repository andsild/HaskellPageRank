{-# LANGUAGE BangPatterns, CPP        #-}

module Data.Graph.PageRankNonDet (
  pageRank, PRType, defaultDampingFactor, PageRankResult, PageRankArray, SafePageRankArray, defaultPrValue
)
  where

import           Data.Graph.InternalPageRank
import           Data.Graph.NeighborGraph
import Data.Util.ArrayUtils

import           Control.Monad
import           Data.IORef
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable         as MU
import qualified Data.Array.Repa.Eval.Gang as RG

import           Debug.Trace

{-# INLINE vertexLoop #-}
vertexLoop ::
  PageRankArray ->
  Int ->
  U.Vector Int ->
  U.Vector PRType ->
  IORef Bool ->
  PRType ->
  PRType ->
  PRType ->
  IO ()
vertexLoop !array !vertexId !nbrs !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst
  | lenNbrs U.! vertexId == 0 = MU.write array vertexId 0
  | otherwise = do
  newVertexValue <- (+dConst) . (*dampingFactor) <$> 
    U.foldM' (\acc i ->
      MU.read array i >>= \pageRankValue ->
      return $ acc + (pageRankValue / lenNbrs U.! i))
        0 nbrs

  oldVal <- MU.read array vertexId
  MU.write array vertexId newVertexValue
  let vDiff = abs $ oldVal - newVertexValue
    in when (vDiff > threshold) $
      -- I ..think.. this is safe - since we only write during a graphLoop.
      writeIORef isAboveThreshold True


graphLoopIO :: (NeighborGraph g)
  => g
  -> PageRankArray
  -> U.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> Word
  -> Word
  -> IO Word
graphLoopIO g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst loopCounter numIters 
  | numIters > 0 && numIters == loopCounter = return loopCounter
  | otherwise = do
  writeIORef isAboveThreshold False
  let numberOfVertices = numVerts g'

  if numberOfVertices < 9000000--minimumParallelizableWorkload
    then V.imapM_ (\vertexId nbrs ->
      vertexLoop pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
      ) (neighborsArray g')
    else RG.gangIO RG.theGang
      (\threadId ->
        let chunkStart = nstart numberOfVertices threadId
            chunkSize = nstart numberOfVertices (succ threadId) - chunkStart
        in V.imapM_ (\vertexId nbrs ->
              vertexLoop pageRankArray (chunkStart+vertexId) nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
            ) (V.slice chunkStart chunkSize (neighborsArray g'))
      )

  isAboveThreshold' <- readIORef isAboveThreshold

  if isAboveThreshold' || (numIters > 0 && numIters > loopCounter)
    then graphLoopIO g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst (succ loopCounter) numIters
    else return loopCounter

pageRank :: (NeighborGraph g) => g -> g -> PRType -> PRType -> PRType -> Word -> IO PageRankResult
pageRank !g !g' !dampingFactor !threshold !prValue !numIters = do
  pageRankArray <- MU.replicate (numVerts g) prValue
  let !lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      !n = fromIntegral $
          U.foldl' (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs

  isAboveThreshold <- newIORef False

  loops <- graphLoopIO g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor (dConst n) 0 numIters

#ifdef DEBUG
  traceM $ "looped (PageRank.hs) " ++ show loops
#endif

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n
