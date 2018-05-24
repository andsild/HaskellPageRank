{-# LANGUAGE BangPatterns, CPP        #-}

module Data.Graph.PageRankWithPreprocessing (
  pageRankIO, PRType, defaultDampingFactor, PageRankResult
  -- , pageRankWith1D
)
  where

import           Data.Graph.InternalPageRank
import Data.Util.ArrayUtils

import           Data.Graph.GraphColoring
import           Control.Monad
import           Data.IORef
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed.Mutable         as MU
import qualified Data.Vector.Unboxed         as U
import           Data.Graph.NeighborGraph

-- import Control.Concurrent.MVar (tryPutMVar, readMVar, newMVar, MVar)
-- import           Control.LVish               (Par, isDet, runPar, runParNonDet, runParPoly)
-- import           Data.IVar.Simple            (IVar, new, tryRead, tryWrite)
import qualified Data.Array.Repa.Eval.Gang as RG

import           Debug.Trace

-- defaultStrategy = VS.parVector (2 :: Int)
--
--

-- vertexLoopMVar ::
--      PageRankArray
--   -> Int
--   -> U.Vector Int
--   -> V.Vector PRType
--   -> MVar Bool
--   -> PRType
--   -> PRType
--   -> PRType
--   -> IO ()
-- vertexLoopMVar array vertexId nbrs lenNbrs isThresholdReached threshold dampingFactor dConst  = do
--   oldVal <- M.read array vertexId
--   newVertexValue <- writeMutexIO array nbrs lenNbrs dampingFactor dConst
--
--   M.write array vertexId newVertexValue
--
--   let vDiff = abs $ oldVal - newVertexValue
--     in when (vDiff > threshold) $
--         void $ tryPutMVar isThresholdReached True

-- vertexLoopIvar ::
--      PageRankArray
--   -> Int
--   -> U.Vector Int
--   -> U.Vector PRType
--   -> IVar Bool
--   -> PRType
--   -> PRType
--   -> PRType
--   -> IO ()
-- vertexLoopIvar array vertexId nbrs lenNbrs isThresholdReached threshold dampingFactor dConst  = do
--   oldVal <- MU.read array vertexId
--   newVertexValue <- ((+dConst) . (*dampingFactor)) <$> 
--     U.foldM' (\acc i ->
--       MU.read array i >>= \pageRankValue ->
--       return $ acc + (pageRankValue / lenNbrs U.! i))
--         0 nbrs
--
--   MU.write array vertexId newVertexValue
--
--   let vDiff = abs $ oldVal - newVertexValue
--     in when (vDiff > threshold) $
--         void $ tryWrite isThresholdReached True

{-# INLINE vertexLoopIO #-}
vertexLoopIO ::
     PageRankArray
  -> Int
  -> U.Vector Int
  -> U.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> IO ()
vertexLoopIO !array !vertexId !nbrs !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst
  | lenNbrs U.! vertexId == 0 = MU.write array vertexId 0
  | otherwise = do
  oldVal <- MU.read array vertexId
    -- TODO: could I save time by lazily reading neighbors, then executing some other function? If there is an overlap, maybe all chunks could be evaluated just once
  newVertexValue <- (+dConst) . (*dampingFactor) <$> 
    U.foldM' (\acc i ->
      MU.read array i >>= \pageRankValue ->
      return $ acc + (pageRankValue / lenNbrs U.! i))
        0 nbrs

  MU.write array vertexId newVertexValue

  let vDiff = abs $ oldVal - newVertexValue
    in when (vDiff > threshold) $
      -- I ..think.. this is safe - since we only write during a graphLoop.
      writeIORef isAboveThreshold True

-- {-# INLINE vertexLoop #-}
-- vertexLoop ::
--      SafePageRankArray s
--   -> Int
--   -> U.Vector Int
--   -> V.Vector PRType
--   -> STRef s Bool
--   -> PRType
--   -> PRType
--   -> PRType
--   -> ST s ()
-- vertexLoop array vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst  = do
--   oldVal <- M.read array vertexId
--   newVertexValue <- writeMutexST array nbrs lenNbrs dampingFactor dConst
--
--   M.write array vertexId newVertexValue
--
--   isAboveThreshold' <- readSTRef isAboveThreshold
--
--   let vDiff = abs $ oldVal - newVertexValue
--     in when (vDiff > threshold) $
--       -- I ..think.. this is safe - since we only write during a graphLoop.
--       writeSTRef isAboveThreshold True
--       -- ticket <- readForCAS isAboveThreshold
--       -- void $ casIORef isAboveThreshold ticket True

-- graphLoopWith1D ::
--   (NeighborGraph g)
--   => g
--   -> PageRankArray
--   -> V.Vector PRType
--   -> IORef Bool
--   -> PRType
--   -> PRType
--   -> PRType
--   -> (V.Vector (Int, Int), V.Vector NodeID)
--   -> IORef Int
--   -> IO ()
-- graphLoopWith1D g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst (colorIndexes, vNodeID) loopCounter = do
--   return ()
  -- modifyIORef loopCounter (+1)
  -- -- isAboveThreshold <- new :: IO (IVar Bool)
  -- writeIORef isAboveThreshold False
  --
  -- V.mapM_ (\(li,ri) ->
  --   V.mapM_ (\vertexId ->
  --     let nbrs = neighbors vertexId g'
  --         numNbrsForVertex = lenNbrs V.! vertexId
  --       in if U.null nbrs && numNbrsForVertex == 0
  --           then M.write pageRankArray vertexId 0.0
  --           else vertexLoop pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
  --     ) (V.slice li ri vNodeID)
  --   ) colorIndexes
  --
  -- isAboveThreshold' <- readIORef isAboveThreshold
  --
  -- when (isAboveThreshold') $
  --   graphLoopWith1D g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst (colorIndexes,vNodeID) loopCounter

  -- tryRead isAboveThreshold >>= \isAboveThreshold' ->
  --   case isAboveThreshold' of
  --     Just bool -> when bool $
  --       graphLoopWith1D g' pageRankArray lenNbrs threshold dampingFactor dConst (colorIndexes,vNodeID) loopCounter
  --     Nothing -> return ()


graphLoopIO ::
  (NeighborGraph g)
  => g
  -> PageRankArray
  -> U.Vector PRType
  -> IORef Bool
  -> PRType
  -> PRType
  -> PRType
  -> GraphColoring
  -> Word
  -> Word
  -> IO Word
-- I dont know why, but coloring should _not_ be strict
graphLoopIO !g' !pageRankArray !lenNbrs !isAboveThreshold !threshold !dampingFactor !dConst coloring !loopCounter !numIters
  | numIters > 0 && numIters == loopCounter = return loopCounter
  | otherwise =
  writeIORef isAboveThreshold False >>

  V.mapM_ (\colorGroup -> let lenColors = V.length colorGroup in
    if lenColors < minimumParallelizableWorkload
      then V.mapM_ (\vertexId ->
            let nbrs = neighbors vertexId g' in
            vertexLoopIO pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
            ) colorGroup
      else RG.gangIO RG.theGang 
        (\threadId -> 
          let chunkStart = nstart lenColors threadId
              chunkSize = nstart lenColors (succ threadId) - chunkStart
          in V.mapM_ (\vertexId ->
            let nbrs = neighbors vertexId g' in
            vertexLoopIO pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
            ) (V.slice chunkStart chunkSize colorGroup)
        )
      ) coloring >>

  readIORef isAboveThreshold >>= \isAboveThreshold' ->
  if isAboveThreshold' || (numIters > 0 && numIters > loopCounter)
    then graphLoopIO g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst coloring (succ loopCounter) numIters
    else return loopCounter

-- We keep this code because it shows the nice property that we can operate on
-- pagerank with language-level determinism
-- graphLoop ::
--   (NeighborGraph g)
--   => g
--   -> SafePageRankArray s
--   -> V.Vector PRType
--   -> STRef s (Bool)
--   -> PRType
--   -> PRType
--   -> PRType
--   -> GraphColoring
--   -> Int
--   -> ST s (Int)
-- graphLoop g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst coloring loopCounter = do
--   writeSTRef isAboveThreshold False
--
--   V.mapM_ (\colorGroup ->
--     (\vec -> do 
--       mapped <- V.mapM (\vertexId ->
--         let nbrs = neighbors vertexId g'
--             numNbrsForVertex = lenNbrs V.! vertexId
--           in if U.null nbrs && numNbrsForVertex == 0
--               then M.write pageRankArray vertexId 0.0
--               else vertexLoop pageRankArray vertexId nbrs lenNbrs isAboveThreshold threshold dampingFactor dConst
--           ) vec
--       return $ myParVector 4 mapped 
--       -- return (mapped `VS.using` (VS.parVector 256))
--     ) colorGroup
--         -- ) colorGroup) >>= \vertexLoops -> ((V.toList vertexLoops  :: ST s [()])) `using` parList r0
--     ) coloring
--
--   isAboveThreshold' <- readSTRef isAboveThreshold
--
--   if isAboveThreshold'
--     then graphLoop g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor dConst coloring (succ loopCounter)
--     else return loopCounter

-- pageRankWith1D :: (NeighborGraph g) => g -> g -> PRType -> PRType -> (V.Vector (Int,Int), V.Vector NodeID) -> IO PageRankResult
-- pageRankWith1D g g' dampingFactor threshold comb = do
--   pageRankArray <- M.replicate (numVerts g) defaultPrValue
--   loopCounter <- newIORef 0
--
--   let lenNbrs = V.map (fromIntegral . U.length) (neighborsArray g)
--       n = fromIntegral $
--           V.foldl (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs
--   isAboveThreshold <- newIORef False
--
--   -- graphLoopWith1D g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor (dConst n) comb loopCounter
--
--   loops <- readIORef loopCounter
--   traceM $ "looped " ++ show loops
--
--   V.unsafeFreeze pageRankArray
--     where
--       dConst n = (1-dampingFactor) / n
--

pageRankIO :: (NeighborGraph g) => 
  g
  -> g
  -> PRType
  -> PRType
  -> GraphColoring
  -> PRType
  -> Word
  -> IO (U.Vector PRType)
pageRankIO !g !g' !dampingFactor !threshold !coloring !prValue !numIters = do
  pageRankArray <- MU.replicate (numVerts g) prValue

  let !lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      !n = fromIntegral $
          U.foldl' (\acc value -> if value == 0 then acc else succ acc) 0 lenNbrs
  isAboveThreshold <- newIORef False

  numLoops <- graphLoopIO g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor (dConst n) coloring 0 numIters

#ifdef DEBUG
  traceM $ "looped (pp) " ++ show numLoops
#endif

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n

-- pageRank :: (NeighborGraph g) => 
--   g
--   -> g
--   -> PRType
--   -> PRType
--   -> GraphColoring
--   -> PRType
--   -> PageRankResult
-- pageRank g g' dampingFactor threshold coloring prValue = V.create $ do
--   pageRankArray <- M.replicate (numVerts g) prValue
--
--   let lenNbrs = V.map (fromIntegral . U.length) (neighborsArray g)
--       n = fromIntegral $
--           V.foldl (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs
--   isAboveThreshold <- newSTRef False
--
--   loops <- graphLoop g' pageRankArray lenNbrs isAboveThreshold threshold dampingFactor (dConst n) coloring 0
--
-- #ifdef DEBUG
--   traceM $ "looped " ++ show loops
-- #endif
--
--   return pageRankArray
--     where
--       dConst n = (1-dampingFactor) / n
