{-# LANGUAGE BangPatterns, CPP        #-}

module Data.Graph.PageRankEC (
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
  -> MVar Bool
  -> Int
  -> U.Vector Int
  -> V.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> IO ()
vertexLoop !array !arrayLock !vertexId !nbrs !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst
  | lenNbrs V.! vertexId == 0 = takeMVar arrayLock >>
                               takeMVar (array V.! vertexId) >>
                               putMVar arrayLock True >>
                               putMVar (array V.! vertexId) 0
  | otherwise = do
  void $ takeMVar arrayLock
  !nbrsValues <- U.mapM (\x -> takeMVar $ V.unsafeIndex array x) nbrs
  -- The difference from edge and full consistency is ordering - full consistency would put this line in bottom
  !oldVal <- takeMVar (array V.! vertexId)
  putMVar arrayLock True
  !newVertexValue <- writeMutexLocked nbrsValues nbrs lenNbrs dampingFactor dConst

  let !vDiff = abs $ oldVal - newVertexValue
    in when (vDiff > threshold) $
      -- I ..think.. this is safe - since we only write during a graphLoop.
      -- TODO: this is safe because any modification to isAboveThreshold respects the lattice -
      -- unlike complex types
      writeIORef isAboveThreshold True

  U.mapM_ (\(i,val) -> putMVar (array V.! i) val) (U.zip nbrs nbrsValues)
  putMVar (array V.! vertexId) newVertexValue

graphLoop :: (NeighborGraph g)
  => g
  -> PageRankArrayLocked
  -> MVar Bool
  -> V.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> Word
  -> Word
  -> IO Word
graphLoop !g' !pageRankArray !arrayLock !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst !loopCounter !numIters
  | numIters > 0 && numIters == loopCounter = return loopCounter
  | otherwise = do
  writeIORef isAboveThreshold False
  let !numberOfVertices = numVerts g'

  if numberOfVertices < minimumParallelizableWorkload
    then V.imapM_ (\vertexId nbrs ->
      vertexLoop pageRankArray arrayLock vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
      ) (neighborsArray g')
    else RG.gangIO RG.theGang
      (\threadId ->
        let chunkStart = nstart numberOfVertices threadId
            chunkSize = (nstart numberOfVertices (succ threadId)) - chunkStart
        in V.imapM_ (\vertexId nbrs ->
              vertexLoop pageRankArray arrayLock vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
              ) (V.slice chunkStart chunkSize (neighborsArray g'))
      )


  isAboveThreshold' <- readIORef isAboveThreshold

  if (isAboveThreshold' || (numIters > 0 && numIters > loopCounter))
    then graphLoop g' pageRankArray arrayLock lenNbrs isAboveThreshold threshold dampingFactor dConst (succ loopCounter) numIters
    else return loopCounter

pageRank :: (NeighborGraph g) => g -> g -> PRType -> PRType -> PRType -> Word -> IO (V.Vector PRType)
pageRank g g' dampingFactor threshold prValue numIters = do
  pageRankArray <- V.replicateM (numVerts g) (newMVar prValue)
  let lenNbrs = V.map (fromIntegral . U.length) (neighborsArray g)
      n = fromIntegral $
          V.foldl (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs

  isAboveThreshold <- newIORef False
  arrayLock <- newMVar False
  loops <- graphLoop g' pageRankArray arrayLock lenNbrs isAboveThreshold threshold dampingFactor (dConst n) 0 numIters

#ifdef DEBUG
  traceM $ "looped  (PageRankEC.hs)" ++ show loops
#endif

  theJusts <- V.mapM (\x -> tryTakeMVar x) pageRankArray
  return $ V.map (fromMaybe 0.0) theJusts
    where
      dConst n = (1-dampingFactor) / n
