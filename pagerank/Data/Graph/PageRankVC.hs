{-# LANGUAGE BangPatterns, CPP        #-}

module Data.Graph.PageRankVC (
  pageRank
)
  where

import           Data.Graph.InternalPageRank
import           Data.Graph.NeighborGraph
import Data.Util.ArrayUtils

import Control.Concurrent.MVar
import           Control.Monad
import           Data.Maybe
import           Data.IORef
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Array.Repa.Eval.Gang as RG

import           Debug.Trace

{-# INLINE vertexLoop #-}
--TODO: is this really the best way?
vertexLoop ::
     PageRankArrayLocked
  -> Int
  -> U.Vector Int
  -> V.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> IO ()
vertexLoop !array !vertexId !nbrs !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst
  | lenNbrs V.! vertexId == 0 = takeMVar (array V.! vertexId) >>
                               putMVar (array V.! vertexId) 0
  | otherwise = do
  !nbrsValues <- U.mapM (\x -> readMVar $ V.unsafeIndex array x) nbrs
  !oldVal <- takeMVar (array V.! vertexId)
  !newVertexValue <- writeMutexLocked nbrsValues nbrs lenNbrs dampingFactor dConst

  let !vDiff = abs $ oldVal - newVertexValue
    in when (vDiff > threshold) $
      -- I ..think.. this is safe - since we only write during a graphLoop.
      -- TODO: this is safe because any modification to isAboveThreshold respects the lattice -
      -- unlike complex types
      writeIORef isAboveThreshold True

  putMVar (array V.! vertexId) newVertexValue

graphLoop :: (NeighborGraph g)
  => g
  -> PageRankArrayLocked
  -> V.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> Word
  -> Word
  -> IO Word
graphLoop !g' !pageRankArray !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst !loopCounter !numIters
  | numIters > 0 && numIters == loopCounter = return loopCounter
  | otherwise = do
  writeIORef isAboveThreshold False
  let !numberOfVertices = numVerts g'

  if numberOfVertices < minimumParallelizableWorkload
    then V.imapM_ (\vertexId nbrs ->
      vertexLoop pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
      ) (neighborsArray g')
    else RG.gangIO RG.theGang
      (\threadId ->
        let chunkStart = nstart numberOfVertices threadId
            chunkSize = (nstart numberOfVertices (succ threadId)) - chunkStart
        in V.imapM_ (\vertexId nbrs ->
              vertexLoop pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
              ) (V.slice chunkStart chunkSize (neighborsArray g'))
      )

  isAboveThreshold' <- readIORef isAboveThreshold

  if (isAboveThreshold' || (numIters > 0 && numIters > loopCounter))
    then graphLoop g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst (succ loopCounter) numIters
    else return loopCounter

pageRank :: (NeighborGraph g) => g -> g -> PRType -> PRType -> PRType -> Word -> IO (V.Vector PRType)
pageRank g g' dampingFactor threshold prValue numIters = do
  pageRankArray <- V.replicateM (numVerts g) (newMVar prValue)
  let lenNbrs = V.map (fromIntegral . U.length) (neighborsArray g)
      n = fromIntegral $
          V.foldl (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs

  isAboveThreshold <- newIORef False
  loops <- graphLoop g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor (dConst n) 0 numIters

#ifdef DEBUG
  traceM $ "looped (PageRankVC.hs) " ++ show loops
#endif

  theJusts <- V.mapM (\x -> tryTakeMVar x) pageRankArray
  return $ V.map (fromMaybe 0.0) theJusts
    where
      dConst n = (1-dampingFactor) / n
