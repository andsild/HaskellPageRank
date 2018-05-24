{-# LANGUAGE BangPatterns        #-} {-# LANGUAGE CPP                 #-} {-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.GraphColoring
  ( graphColoring
  , graphColoringGreedyInputOrder
  , graphColoring2Distance
  , jonesPlassmanSeq
  , genRandomNumbers
  , getGraphColoring
  , getGraphColoring2
  , getGraphColoring4
  , getGraphColoring1D'
  , genRandomNumbersWithGen
  , graphColoringNonDeterministic
  , Color
  , GraphColoring
  ) where

import           Data.Graph.NeighborGraph

import           Control.LVish
import           Control.Monad
import           Control.Monad.Primitive      (PrimState)
import           Control.Monad.Random         (evalRandIO, getRandomRs, RandomGen(..), randomRs)
import           Control.Monad.ST
import           Data.IORef
import           Data.Maybe
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as M
import qualified Data.Vector.Unboxed          as U
import qualified Data.Vector.Unboxed.Mutable          as MU
import           Data.Word
import qualified Data.Array.Repa.Eval.Gang as RG

import           Data.STRef                   (modifySTRef, newSTRef, readSTRef, writeSTRef)
-- TODO: experiment with different sorts man.. I think small sort is better
import qualified Data.Vector.Algorithms.Intro as VSort

nstart  :: Int -> Int -> Int
nstart !len !c
 = let  chunks          = RG.gangSize RG.theGang
        chunkLen        = len `quot` chunks
        chunkLeftover   = len `rem`  chunks

        getStart c'
         | c' < chunkLeftover  = c' * (chunkLen + 1)
         | otherwise           = c' * chunkLen  + chunkLeftover

  in    getStart c

type Color = Word16
uncolored :: Color
uncolored = minBound :: Color

type GraphColoring = V.Vector (V.Vector NodeID)

-- |
--   Return buckets for k colors, where each k colorbucket has x vertices
getGraphColoring :: V.Vector Color -> GraphColoring
getGraphColoring v =
  V.create $ do
    let maxElem = V.maximum v
    graphColoringByColor <- M.replicate (fromIntegral maxElem) V.empty
    V.imapM_
      (\index color -> do
         let colorIndex = fromIntegral $ color - 1

         colorVec <- M.read graphColoringByColor colorIndex
         let newColorVec = V.snoc colorVec (fromIntegral index)
          in M.write graphColoringByColor colorIndex newColorVec)
      v
    return graphColoringByColor

getGraphColoring2 :: V.Vector Color -> GraphColoring
getGraphColoring2 v =
  V.unsafeAccumulate V.snoc retList (V.imap (\i e -> (fromIntegral $ e - 1, i)) v)
  where
    retList = V.replicate numberOfColors V.empty
    numberOfColors = fromIntegral $ V.maximum v

getGraphColoring4 :: U.Vector Color -> GraphColoring
getGraphColoring4 v = runST $ do
  let c = getCounts numberOfColors v
  retList <- V.mapM (runST $ return M.new) (V.convert c)
  indexCounter <- U.unsafeThaw c
  U.imapM_
    (\i e' ->
      let e = fromIntegral $ pred e' in
        MU.modify indexCounter pred e >>
        MU.read indexCounter e >>= \index ->
        M.write (retList V.! e) index i
        )
    v
  V.mapM V.unsafeFreeze retList
    where
      numberOfColors = fromIntegral $ U.maximum v

getGraphColoring1D' :: V.Vector Color -> (V.Vector (Int, Int), V.Vector NodeID)
getGraphColoring1D' v = runST $ do
  indexedV <- V.thaw (V.indexed v)

  VSort.sortBy (\(_,lc) (_,rc) -> lc `compare` rc) indexedV

  colorIndexes <- M.unsafeNew $ succ numberOfColors
  frznSorted <- V.freeze indexedV

  -- M.write colorIndexes 0 (0,0)
  lastIndex <- newSTRef (0 :: Int)
  lastColor <- newSTRef (0 :: Color)

  nodeIndexes <- V.imapM (\iPos (i,c) -> do
    readSTRef lastColor >>= \ lastColor' ->
      when (lastColor' < c) $ do
        lastIndex' <- readSTRef lastIndex
        M.write colorIndexes (fromIntegral (c-1)) (lastIndex', iPos-lastIndex')
        modifySTRef lastColor succ
        writeSTRef lastIndex iPos
    return i
    ) frznSorted

  M.read colorIndexes (numberOfColors-1) >>= \(f,s) ->
    M.write colorIndexes numberOfColors (f+s, V.length v - (f+s))

  V.freeze colorIndexes >>= \frznColorIndexes ->
    return (V.drop 1 frznColorIndexes, nodeIndexes)
    where
      numberOfColors = fromIntegral $ V.maximum v

getCounts :: Int -> U.Vector Color -> U.Vector Int
getCounts numberOfColors =
  -- This actually beats its imperative counterpart below, yay
  U.unsafeAccumulate (+) (U.replicate numberOfColors 0) . U.map (\v -> ((fromIntegral . pred) v, 1))
  -- U.create $ do
  -- mli <- MU.replicate numberOfColors 0
  -- U.mapM_ ((MU.modify mli succ) . pred . fromIntegral) li
  -- return mli

genRandomNumbersWithGen :: (RandomGen g) => g -> Int -> Int -> Int -> U.Vector Int
genRandomNumbersWithGen g howManyToTake lo hi = U.fromList
  (take howManyToTake $ randomRs (lo, hi) g)

genRandomNumbers :: Int -> Int -> Int -> IO (U.Vector Int)
genRandomNumbers howManyToTake lo hi = evalRandIO $
  U.fromList . take howManyToTake <$> getRandomRs (lo, hi)

-- assignPredAndSucc
--   :: (NeighborGraph g)
--   => g -> g -> U.Vector Int -> V.Vector (U.Vector Int, U.Vector Int)
-- assignPredAndSucc g g' randomNumbers = V.fromList $
--   map (\i ->
--        let vertexValue = randomNumbers U.! i
--        in U.partition
--             (\nbrId ->
--                let nbrValue = randomNumbers U.! nbrId
--                in nbrValue < vertexValue || (nbrValue == vertexValue && nbrId < i)
--                ) (neighbors i g U.++ neighbors i g')
--     ) [0..pred (numVerts g)]

numPreds
  :: (NeighborGraph g)
  => g -> g ->  U.Vector Int -> U.Vector Int
numPreds g g' randomNumbers = U.fromList $
      map (\i -> 
        let vertexValue = randomNumbers U.! i
        in U.foldl'
            (\acc nbrId ->
                let nbrValue = randomNumbers U.! nbrId
                    boolVal = nbrValue < vertexValue || (nbrValue == vertexValue && nbrId < i)
                in if boolVal then succ acc else acc
            ) 0 (neighbors i g U.++ neighbors i g')
          ) [0..pred (numVerts g)]

-- assignPredecessors
--   :: (NeighborGraph g)
--   => g -> V.Vector Int -> EdgeArray
-- assignPredecessors g randomNumbers = EdgeArray {vectorEdges = filtered}
--   where
--     filtered =
--       V.imap
--         (\vertexId nbrs ->
--            let vertexValue = randomNumbers V.! vertexId
--            in U.filter
--                 (\nbrId ->
--                    let nbrValue = randomNumbers V.! nbrId
--                    in nbrValue < vertexValue || (nbrValue == vertexValue && nbrId < vertexId))
--                 nbrs)
--         (neighborsArray g)
--
-- assignSuccessors
--   :: (NeighborGraph g)
--   => g -> g -> U.Vector Int -> EdgeArray
-- assignSuccessors g g' randomNumbers = EdgeArray {vectorEdges = filtered}
--   where
--     filtered = V.fromList $
--       map (\i ->
--         let vertexValue = randomNumbers U.! i
--         in U.filter (\nbrId ->
--             let nbrValue = randomNumbers U.! nbrId
--             in nbrValue > vertexValue || (nbrValue == vertexValue && nbrId > i)
--             ) (neighbors i g U.++ neighbors i g')
--         ) [0..pred (numVerts g)]

-- Am I losing performance because I do so much IO when all I have to do is iterate over sets?
-- Do I need succOfNode or just a count?
-- color2
--   :: (PrimMonad m)
--   => Int -> V.Vector (U.Vector Int, U.Vector Int) -> V.MVector (PrimState m) Color -> m ()
-- color2 nodeId g' colorArray = do
--   colors <- U.mapM (M.read colorArray . fromIntegral) predOfNode
--   let nextColor =
--         if U.null predOfNode
--           then uncolored + 1
--           else U.maximum colors
--     in M.write colorArray nodeId nextColor
--   U.mapM_
--     (\nbrId ->
--       let (predOfNbr, _) = g' V.! nbrId
--       in when (not (U.null predOfNbr) && U.last predOfNbr == nodeId) $
--         color2 nbrId g' colorArray)
--     succOfNode
--   where
--     (predOfNode, succOfNode) = g' V.! nodeId

--TODO: Binary tournament could be solved by creating a thread for each coloring thread.

findSmallestPossibleColor :: U.Vector Color -> Color
findSmallestPossibleColor !li
  | U.null li = 1
  | otherwise = runST $ do
      mli <- MU.replicate lilen False
      U.mapM_ (\l -> let l' = fromIntegral l in when (l' < succ lilen) $ MU.write mli (pred l') True) li
      U.unsafeFreeze mli >>= return . ((fromIntegral . succ) . (fromMaybe lilen) . (U.elemIndex False))

    -- Pure version given below. Its performance sucks.
    -- fromIntegral . succ $ fromMaybe lilen 
    --   (U.elemIndex False $ U.update (U.replicate lilen False) (U.zip (U.filter (< lilen) $ U.map (fromIntegral . pred) li) (U.replicate lilen True)))
    where
      lilen = U.length li

-- color3 :: Int -> V.Vector (U.Vector Int, U.Vector Int) -> V.MVector (PrimState (ST s)) Color -> ST s ()
-- color3 nodeId g' colorArray = do
--   this <- newSTRef 0
--   U.mapM_ (M.read colorArray >=> (\val -> readSTRef this >>= \max' -> when (fromIntegral val > max') $ writeSTRef this val)) predOfNode
--   readSTRef this >>= \val -> M.write colorArray nodeId (succ val)
--
--   U.mapM_
--     (\nbrId ->
--       let predOfNbr = fst $ g' V.! nbrId
--       in when (not (U.null predOfNbr) && U.last predOfNbr == nodeId) $
--         color3 nbrId g' colorArray)
--     succOfNode
--   where
--     (predOfNode, succOfNode) = g' V.! nodeId

-- color4 :: Int -> V.Vector (U.Vector Int, U.Vector Int) -> U.MVector (PrimState (ST s)) Int -> U.MVector (PrimState (ST s)) Color -> ST s ()
-- color4 !nodeId g' !counts !colorArray = do
--   nextColor <- findSmallestPossibleColor <$> U.mapM (MU.read colorArray) predOfNode
--
--   MU.write colorArray nodeId nextColor
--   U.mapM_ (MU.modify counts pred) succOfNode
--   -- To prevent rescheduling a node, we reduce our own count for nodeId below 0
--   MU.modify counts pred nodeId
--
--   U.mapM_ (\nbrId ->
--     MU.read counts nbrId >>= \count -> 
--     when (count == 0) $
--       color4 nbrId g' counts colorArray
--     ) succOfNode
--   where
--     (!predOfNode, !succOfNode) = g' V.! nodeId

-- color5 is color4 without the explicit list of successor and predecessors
-- the bangpatterns in the signature are tested: the following combination should be optimal
color5 :: (NeighborGraph g) => NodeID -> g -> g -> U.Vector Int -> U.MVector (PrimState (ST s)) Int -> U.MVector (PrimState (ST s)) Color -> ST s ()
color5 !nodeId g g' rands !counts !colorArray =
  -- To prevent rescheduling a node, we reduce our own count for nodeId below 0
  MU.modify counts pred nodeId >>

  findSmallestPossibleColor <$> U.mapM (MU.read colorArray) predOfNode 
  >>= MU.write colorArray nodeId >>

  U.mapM_ (\nbrId ->
    MU.modify counts pred nbrId >>
    (== 0) <$> MU.read counts nbrId
    >>= flip when (color5 nbrId g g' rands counts colorArray)
    ) succOfNode
  where
    (predOfNode, succOfNode) = 
      let vertexValue = rands U.! nodeId
      in U.partition (\nbrId ->
               let nbrValue = rands U.! nbrId
               in nbrValue < vertexValue || (nbrValue == vertexValue && nbrId < nodeId)
               ) (neighbors nodeId g U.++ neighbors nodeId g')


-- Steps: give each node a random number
-- Ask each node if they have the biggest/lowest weight
-- If they do, they are in the new set I'
jonesPlassmanSeq
  :: (NeighborGraph g)
  => g -> g -> U.Vector Int -> U.Vector Color
--TODO: since the greedy vastly outperforms this one, I should try to optimize away a few things - like the "assignPredAndSucc"
jonesPlassmanSeq !g !g' !randomNumbers
  | U.length randomNumbers /= numVerts g =
    error $ "Mismatch between number of randvals (" ++ show (U.length randomNumbers) ++ ") and vertices " ++ show (numVerts g) ++ ")"
  | otherwise =
    U.create $ do
      colorArray <- MU.replicate (numVerts g) uncolored
      counts' <- MU.new (numVerts g)
      let predCounts = numPreds g g' randomNumbers
      U.imapM_ (MU.write counts') predCounts
      let nodesWithNoPreds = U.findIndices (==0) predCounts
      U.mapM_ (\vertexId -> color5 vertexId g g' randomNumbers counts' colorArray) nodesWithNoPreds
      return colorArray

graphColoring
  :: (NeighborGraph g)
  => Color -> g -> V.Vector Color
graphColoring numColors g =
  V.create $ do
    let colors = [0 .. numColors] :: [Color]
    flagsArr <- M.replicate (numVerts g) uncolored
    let loop !numNbrs !nbrs !selfInd !i triedColors
          | i == numNbrs = M.write flagsArr (fromIntegral selfInd) $ head triedColors
          | otherwise = do
            let nbrInd = nbrs U.! i
            nbrFlag <- M.read flagsArr nbrInd
            loop numNbrs nbrs selfInd (i + 1) (filter (/= nbrFlag) triedColors)
    for_ (0, numVerts g) $ \ndIx -> do
      let nds = neighbors ndIx g
      let ofLowerOrder = U.filter (< ndIx) nds
      loop (U.length ofLowerOrder) ofLowerOrder ndIx 0 (drop 1 colors)
    return flagsArr

graphColoringGreedyInputOrder
  :: (NeighborGraph g)
  => g -> g -> U.Vector Color
graphColoringGreedyInputOrder g g' = U.create $
  MU.replicate (numVerts g) 9001 >>= \mli ->
  mapM_ (\i ->
    U.mapM (MU.read mli) (neighbors i g) >>= \valNbrsG ->
    U.mapM (MU.read mli) (neighbors i g') >>= \valNbrsG' ->
    MU.write mli i $ (fromIntegral . findSmallestPossibleColor) (valNbrsG U.++ valNbrsG')
    ) [0..pred (numVerts g)] >>
  return mli


graphColoring2Distance
  :: (NeighborGraph g)
  => g -> U.Vector Color
--TODO: implement
graphColoring2Distance g = U.create $ do
  mli <- MU.replicate (numVerts g) 9001

  V.imapM_ (\i nbrs -> do
    valNbr <- U.mapM (MU.read mli) nbrs
    valNbrsOfNbrs <- U.concat <$> mapM (\nbrId -> U.mapM (MU.read mli) (neighbors nbrId g)) (U.toList nbrs)
    let lowestForAllNbrs = findSmallestPossibleColor ((U.++) valNbr valNbrsOfNbrs)
    MU.write mli i lowestForAllNbrs
    ) (neighborsArray g)
  return mli

graphColoringNonDeterministic 
  :: (NeighborGraph g)
  => g
  -> g
  -> IO (U.Vector Color)
graphColoringNonDeterministic g g' =
  MU.replicate numVertices 9001
  >>= \mli ->
  RG.gangIO RG.theGang
    (\threadId -> 
      let chunkStart = nstart numVertices threadId
          chunkSize = nstart numVertices (succ threadId) - chunkStart
      in mapM_ (\i -> 
          fromIntegral . findSmallestPossibleColor <$> U.mapM (MU.read mli) (neighbors i g U.++ neighbors i g')
          >>= MU.write mli i
          ) [chunkStart..pred (chunkStart+chunkSize)]
    ) >>
  iterateOverFalse mli 0 (U.enumFromN 0 numVertices) >>
  U.unsafeFreeze mli
    where 
      numVertices = numVerts g
      -- I dont know why, but it seems that having Nothing in the ioref is better than initial U.empty
      iterateOverFalse :: MU.IOVector Color -> Int -> U.Vector Int -> IO ()
      iterateOverFalse mli roundNum activationSet
        | U.null activationSet = return ()
        | otherwise = 
        V.replicateM (RG.gangSize RG.theGang) (newIORef Nothing) >>= \newAs ->
        RG.gangIO RG.theGang
          (\threadId -> 
            let chunkStart = nstart (U.length activationSet) threadId
                chunkSize = nstart (U.length activationSet) (succ threadId) - chunkStart
            in Just <$> U.filterM (\i -> do
                   disNode <- MU.read mli i
                   relevantNbrsValues <- U.mapM (MU.read mli) (U.filter (>i) (neighbors i g U.++ neighbors i g'))
                   if U.elem disNode relevantNbrsValues
                     then let newColor = findSmallestPossibleColor relevantNbrsValues
                          in MU.write mli i newColor >> return True
                     else return False
                   ) (U.slice chunkStart chunkSize activationSet) 
               >>= writeIORef (newAs V.! threadId)
          ) >>
        (U.force . U.concat . catMaybes . V.toList) <$> V.mapM readIORef newAs
        >>= iterateOverFalse mli (succ roundNum)
