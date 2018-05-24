{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.Graph.Prism
  (
    prism
    , prism2
    , prism3
    , prism4
    , prism5
    , prism6
  ) where

--TODO: there should be prism without coloring!
--TODO: there should be a coloring algorithm which yields a more balanced set of colors, without the current "tail" from jonesPlassmanSeq.

import           Data.Graph.GraphColoring    (Color)
import           Data.Graph.EdgeArray             (EdgeArray)
import           Data.Graph.NeighborGraph         (NeighborGraph (..), NodeID)

import           Control.DeepSeq             (deepseq)
import           Control.Monad               (foldM, unless, when)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.Vector                 as V
import Data.Util.ArrayUtils
import qualified Data.Maybe
import qualified Data.Vector.Mutable         as M
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.IntSet                 as Set
-- import           Control.LVish
-- import           Control.LVish.DeepFrz       (DeepFrz, FrzType, Frzn, NonFrzn, Trvrsbl)
-- import qualified Data.Concurrent.SkipListMap as SLM
-- import           Data.LVar.Generic
-- import qualified Data.LVar.IStructure        as IS
-- import qualified Data.LVar.SLSet             as SL
import qualified Data.Array.Repa.Eval.Gang   as RG
import           Debug.Trace
import qualified Data.HashTable.IO as H

type PRType = Double
type PageRankArray  = MU.IOVector PRType
type PageRankResult  = U.Vector PRType
minimumParallelizableWorkload :: Int
minimumParallelizableWorkload = 7000

type HashTable k v = H.CuckooHashTable k v

-- type SPALog s = SL.ISet s NodeID
-- type SPABag s = V.Vector (IS.IStructure s NodeID)
-- type SPALog2 m = V.MVector (PrimState m) NodeID
-- type SPALog2 = Set Color
type SPABag2 m = V.MVector (PrimState m) (V.Vector NodeID)
-- type SPABag3 s = V.MVector (PrimState (ST s)) Set.IntSet
type SPABag3 = M.IOVector Set.IntSet

-- Could Use LVar.Counter instead to avoid length checks and potential races
-- mbInsert :: (HasPut e)
--   => SPALog s
--   -> SPABag s
--   -> NodeID
--   -> Int
--   -> Par e s ()
-- mbInsert spaLog spaBag vertexId color =
--   SL.insert color spaLog >>
--   let resultBag = spaBag V.! color in
--   IS.getLength resultBag >>= \newSlot ->
--   IS.put resultBag newSlot vertexId

-- mbInsert2 :: (PrimMonad m)
--   => SPALog2
--   -> SPABag2 m
--   -> NodeID
--   -> Color
--   -> m (Set Color)
-- mbInsert2 spaLog spaBag vertexId color = do
  --   let amended = Set.insert color spaLog
--
--   mybag <- M.read spaBag color'
--   let amendedBag = V.snoc mybag vertexId
--   M.write spaBag color' amendedBag
--
--   return amended
--     where
--       color' = fromIntegral color

mbInsert3 :: (PrimMonad m)
  => SPABag2 m
  -> NodeID
  -> Color
  -> m ()
mbInsert3 spaBag vertexId color = do
  mybag <- M.read spaBag color'
  let amendedBag = V.snoc mybag vertexId
  M.write spaBag color' amendedBag
    where
      color' = fromIntegral (color-1)

mbInsert4 :: (PrimMonad m)
  => SPABag2 m
  -> V.MVector (PrimState m) Int
  -> NodeID
  -> Color
  -> m ()
mbInsert4 spaBag counts vertexId color = do
  nextIndex <- M.read counts color'
  mybag <- M.read spaBag color'
  unfrzn <- V.unsafeThaw mybag
  M.write unfrzn nextIndex vertexId
  frzn <- V.unsafeFreeze unfrzn

  M.write spaBag color' frzn
  M.modify counts succ color'
    where
      color' = fromIntegral (color-1)

mbInsert5 ::
     SPABag3
  -> NodeID
  -> Color
  -> IO ()
mbInsert5 spaBag vertexId color =
  M.read spaBag color' >>= \set ->
  unless (Set.member vertexId set) $
    M.modify spaBag (Set.insert vertexId) color'
      where
    color' = (fromIntegral . pred) color

-- mbCollect2 ::
--      SPALog2
--   -> V.Vector (V.Vector NodeID)
--   -> V.Vector (V.Vector NodeID)
-- mbCollect2 spaLog spaBag =
--   Set.foldr (\e acc -> V.snoc acc (spaBag V.! fromIntegral e)) V.empty spaLog

mbCollect3 ::
     V.Vector (V.Vector NodeID)
  -> V.Vector (V.Vector NodeID)
mbCollect3 =
  V.filter $ not . V.null

mbCollect4 ::
     V.Vector (V.Vector NodeID)
  -> V.Vector (V.Vector NodeID)
mbCollect4 input =
  V.map (V.takeWhile (> (-1))) $ V.filter (not . V.null) input

mbCollect5 ::
     V.Vector Set.IntSet
  -> V.Vector (V.Vector NodeID)
mbCollect5 input =
  V.filter (not . V.null) $ V.map (V.fromList . Set.toList) input

type PrFunction = (NodeID -> V.Vector Set.IntSet -> EdgeArray -> U.Vector PRType -> PageRankArray -> PRType -> PRType -> PRType -> IO Set.IntSet)
-- type PrFunctionRetardSet = (NodeID -> EdgeArray -> EdgeArray -> V.Vector (MU.IOVector PRType) -> U.Vector PRType -> MU.IOVector PRType -> PRType -> PRType -> PRType -> IO (U.Vector NodeID))
-- type PrFunctionRetardSet2 = (NodeID -> EdgeArray -> EdgeArray -> U.Vector PRType -> PageRankArray -> PRType -> PRType -> PRType -> IO (U.Vector NodeID))
type PrFunctionRetardSet3 = (NodeID -> EdgeArray -> EdgeArray -> U.Vector PRType -> PageRankArray -> PRType -> PRType -> PRType -> IO Bool)

prFunction :: (NeighborGraph g)
  => NodeID
  -> g
  -> g
  -> U.Vector PRType
  -> MU.IOVector PRType
  -> PRType
  -> PRType
  -> PRType
  -> IO (U.Vector NodeID)
prFunction nodeID g g' lenNbrs pageRankArray dampingFactor dConst threshold
  | U.null nbrs && lenNbrs U.! nodeID == 0 =
    MU.write pageRankArray nodeID 0 >>
    return U.empty
  | otherwise = do
  oldVal <- MU.read pageRankArray nodeID

  rhs <- U.foldM' (\acc i -> do
    let i' = fromIntegral i
        numNbrs = lenNbrs U.! i'
    pageRankValue <- MU.unsafeRead pageRankArray i'
    return $ acc + (pageRankValue / numNbrs))
      0 nbrs

  let newVal = dConst + (dampingFactor * rhs)

  MU.write pageRankArray nodeID newVal

  if abs (oldVal - newVal) < threshold
    then return U.empty
    else return $ neighbors nodeID g
      where
    nbrs = neighbors nodeID g'


prFunctionIO :: PrFunction
prFunctionIO nodeID g g' lenNbrs pageRankArray dampingFactor dConst threshold
  | U.null nbrs && lenNbrs U.! nodeID == 0 = do
    MU.write pageRankArray nodeID 0
    return Set.empty
      | otherwise = do
  oldVal <- MU.read pageRankArray nodeID

  rhs <- U.foldM' (\acc i -> do
    let i' = fromIntegral i
        numNbrs = lenNbrs U.! i'
    pageRankValue <- MU.unsafeRead pageRankArray i'
    return $ acc + (pageRankValue / numNbrs))
      0 nbrs

  let newVal = dConst + (dampingFactor * rhs)

  MU.write pageRankArray nodeID newVal

  if abs (oldVal - newVal) < threshold
    then return Set.empty
    else return $ g V.! nodeID
      where
    nbrs = neighbors nodeID g'

-- prFunctionIORetardSet :: PrFunctionRetardSet
-- prFunctionIORetardSet nodeID g g' edgeData lenNbrs pageRankArray dampingFactor dConst threshold
--   | U.null nbrsInbound && lenNbrs U.! nodeID == 0 = MU.write pageRankArray nodeID 0 >> return U.empty
--   | otherwise = do
--
--   rhs <- U.foldM' (\acc i -> do
--     let i' = fromIntegral i
--         numNbrs = lenNbrs U.! i'
--     pageRankValue <- MU.unsafeRead pageRankArray i'
--     Just myIndex <- return $ U.findIndex (==nodeID) (neighbors i' g)
--     MU.write (edgeData V.! i') myIndex pageRankValue
--     return $ acc + (pageRankValue / numNbrs)
--     ) 0 nbrsInbound
--
--   let newVal = dConst + (dampingFactor * rhs)
--   MU.write pageRankArray nodeID newVal
--
--   -- traceM $ "lets do " ++ show (neighbors nodeID g) ++ "\t, keeping in mind " ++ show (neighbors nodeID g')
--   let terminal_threshold = 0.5
--   U.filterM (\nbrId -> do
--     Just thatOtherNbrId <- return $ U.findIndex (==nbrId) (neighbors nodeID g)
--     -- traceM $ "trying to read from " ++ show nodeID ++ " index  " ++ show thatOtherNbrId
--     oldValFromNbr <- MU.read (edgeData V.! nodeID) thatOtherNbrId
--     let diff = abs (newVal - oldValFromNbr)
--         edgeWeight = 1 / (fromIntegral $ U.length (neighbors nbrId g))
--     return $ ((diff * edgeWeight) > (threshold*terminal_threshold))
--     ) (neighbors nodeID g)
--     where
--       nbrsInbound = neighbors nodeID g'

-- prFunctionIORetardSet2 :: PrFunctionRetardSet2
-- prFunctionIORetardSet2 nodeID g g' lenNbrs pageRankArray dampingFactor dConst threshold
--   | U.null nbrsInbound && lenNbrs U.! nodeID == 0 = do
--     MU.write pageRankArray nodeID 0
--     return U.empty
--       | otherwise = do
--   oldVal <- MU.read pageRankArray nodeID
--
--   rhs <- U.foldM' (\acc i ->
--     let i' = fromIntegral i
--         numNbrs = lenNbrs U.! i'
--     in MU.unsafeRead pageRankArray i' >>= \pageRankValue ->
--     return $ acc + (pageRankValue / numNbrs)
--     ) 0 nbrsInbound
--
--   let newVal = dConst + (dampingFactor * rhs)
--   MU.write pageRankArray nodeID newVal
--
--   let diff = abs (oldVal - newVal)
--   if diff < threshold
--     then return U.empty
--     else return $ neighbors nodeID g
--     -- else return $ neighbors nodeID g
--       where
--     nbrsInbound = neighbors nodeID g'

prFunctionIORetardSet3 :: PrFunctionRetardSet3
prFunctionIORetardSet3 nodeID _ g' lenNbrs pageRankArray dampingFactor dConst threshold
  | U.null nbrsInbound && lenNbrs U.! nodeID == 0 = do
    MU.write pageRankArray nodeID 0
    return False
      | otherwise = do
  oldVal <- MU.read pageRankArray nodeID

  newVal <- (+dConst) . (*dampingFactor) <$>
    U.foldM' (\acc i ->
      MU.read pageRankArray i >>= \pageRankValue ->
      return $ acc + (pageRankValue / lenNbrs U.! i))
        0 (neighbors nodeID g')
  MU.write pageRankArray nodeID newVal

  let diff = abs (oldVal - newVal)
  if diff < threshold
    then return False
    else return True
      where
    nbrsInbound = neighbors nodeID g'

-- why was this bad?
prism :: (NeighborGraph g)
  => g
  -> g
  -> U.Vector Color
  -> PRType
  -> PRType
  -> V.Vector (V.Vector NodeID)
  -> PRType
  -> IO (PageRankResult)
prism g g' graphColoring dampingFactor threshold activationSet prValue = do
  pageRankArray <- MU.replicate (numVerts g) prValue
  let lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      n = fromIntegral $
                U.foldl' (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs
  prism' g g' pageRankArray lenNbrs prFunction activationSet graphColoring threshold dampingFactor (dConst n)

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n


-- this method will not use snoc (prism1), but rather filter out big lists...
-- why does it lag so much?
prism2 :: (NeighborGraph g)
  => g
  -> g
  -> U.Vector Color
  -> PRType
  -> PRType
  -> V.Vector (V.Vector NodeID)
  -> PRType
  -> IO PageRankResult
prism2 g g' graphColoring dampingFactor threshold activationSet prValue = do
  pageRankArray <- MU.replicate (numVerts g) prValue
  let !lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      !n = fromIntegral $
                       U.foldl' (\acc value -> if value == 0 then acc else succ acc) 0 lenNbrs
  let colorCounts = V.map (V.foldl' (\node acc -> acc + floor (lenNbrs U.! node)) 0) activationSet
  prism2' g g' pageRankArray lenNbrs prFunction activationSet graphColoring threshold dampingFactor (dConst n) colorCounts

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n


prism3 :: (NeighborGraph g)
  => g
  -> g
  -> U.Vector Color
  -> PRType
  -> PRType
  -> V.Vector (V.Vector NodeID)
  -> PRType
  -> IO (U.Vector PRType)
prism3 g g' graphColoring dampingFactor threshold activationSet prValue = do
  pageRankArray <- MU.replicate (numVerts g) prValue
  let lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      n = U.foldl' (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs

  loopCounter <- prism3' g g' pageRankArray lenNbrs prFunction activationSet graphColoring threshold dampingFactor (dConst n) 0
  traceM $ show loopCounter

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n

removeRedundant :: V.Vector (V.Vector NodeID) -> V.Vector (V.Vector NodeID)
removeRedundant = V.map (V.fromList . Set.toList . Set.fromList . V.toList)

-- Mutable sets. Slower than prism5
prism6 ::
     EdgeArray
  -> EdgeArray
  -> U.Vector Color
  -> PRType
  -> PRType
  -> V.Vector (V.Vector NodeID)
  -> PRType
  -> IO PageRankResult
prism6 !g !g' graphColoring !dampingFactor !threshold activationSet !prValue = do
  pageRankArray <- MU.replicate (numVerts g) prValue
  let !lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      !n = fromIntegral $
          U.foldl' (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs

  numLoops <- prism6' g g' pageRankArray lenNbrs prFunctionIORetardSet3 activationSet graphColoring threshold dampingFactor (dConst n) 0

#ifdef DEBUG
  traceM $ "looped (prism6)" ++ show numLoops ++ " times in prism"
#endif

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n

prism6' ::
     EdgeArray
  -> EdgeArray
  -> PageRankArray
  -> U.Vector PRType
  -> PrFunctionRetardSet3
  -> V.Vector (V.Vector NodeID)
  -> U.Vector Color
  -> PRType
  -> PRType
  -> PRType
  -> Int
  -> IO Int
prism6' !g !g' !graphArray !lenNbrs !vertexFunc !activationSet !graphColoring !threshold !dampingFactor !dConst !numLoops
  | V.all V.null activationSet = return numLoops
  | otherwise = do
#ifdef DEBUG
  traceM $ "I have " ++ (show $ V.sum (V.map V.length activationSet)) ++ " nodes to process"
#endif

  -- code below: Segfaults?!
  -- V.replicateM (RG.gangSize RG.theGang)
  --           (V.replicateM (V.length activationSet) (H.new :: IO (HashTable NodeID Bool)))
  -- V.replicateM (V.length activationSet) (H.new :: IO (HashTable NodeID Bool))
  V.mapM (\li -> H.newSized (V.length li) :: IO (HashTable NodeID Bool)) activationSet
  >>= \vli ->
  V.mapM_ (\colorSet ->
    let numVertices = V.length colorSet in
    (H.newSized (U.length graphColoring) :: IO (HashTable NodeID Bool)) >>= \processed ->

    if numVertices < minimumParallelizableWorkload
      then V.mapM_ (\nodeId -> do
            isProcessed <- Data.Maybe.fromMaybe False <$> H.lookup processed nodeId
            unless isProcessed $
              H.insert processed nodeId True >>
              vertexFunc nodeId g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
              when afflicted $
                U.mapM_ (\a ->
                    let set = vli V.! (fromIntegral . pred $ graphColoring U.! a)
                    in H.insert set a True
                  ) (neighbors nodeId g)
            ) colorSet
      else RG.gangIO RG.theGang
      (\threadId ->
          let chunkStart = nstart numVertices threadId
              chunkSize = nstart numVertices (succ threadId) - chunkStart
          in V.mapM_ (\nodeId -> do
            isProcessed <- Data.Maybe.fromMaybe False <$> H.lookup processed nodeId
            unless isProcessed $
              H.insert processed nodeId True >>
              vertexFunc nodeId g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
              when afflicted $
                U.mapM_ (\a ->
                    let set = vli V.! (fromIntegral . pred $ graphColoring U.! a)
                    in H.insert set a True
                  ) (neighbors nodeId g)
            ) (V.slice chunkStart chunkSize colorSet)
      )
    ) activationSet >>
  V.mapM (\colorId ->
      let set = vli V.! colorId
      in V.fromList . map fst <$> H.toList set
      ) (V.enumFromN 0 (length activationSet))
  >>= \newSpaBag ->
  prism6' g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst (succ numLoops) 

-- LVars thread their structures with state. Trying to mix the state of
-- my mutable vectors with their monotonically growing lists is not possible,
-- at least not without some unsafePerformIO or unsafeDupablePerformIO.
-- An alternative could be to combine lvars with BSP.
-- -- prism6 ::
--      EdgeArray
--   -> EdgeArray
--   -> U.Vector Color
--   -> PRType
--   -> PRType
--   -> V.Vector (V.Vector NodeID)
--   -> IO PageRankResult
-- prism6 g g' graphColoring dampingFactor threshold activationSet = do
  --   pageRankArray <- M.replicate (numVerts g) defaultPrValue
--   edgeData <- V.mapM (\e -> M.replicate (U.length e) defaultPrValue) (neighborsArray g)
--   let lenNbrs = V.map (fromIntegral . U.length) (neighborsArray g)
--       n = fromIntegral $
             --           V.foldl (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs
--
--   sets <- mapM (\vec -> SL.newFromList (V.toList vec)) (V.toList $ activationSet)
--   numLoops <- prism6' g g' pageRankArray lenNbrs prFunctionIORetardSet sets graphColoring threshold dampingFactor (dConst n) edgeData 0
--
--   traceM $ "looped " ++ show numLoops ++ " times in prism"
--
--   V.unsafeFreeze pageRankArray
--     where
--       dConst n = (1-dampingFactor) / n

-- Prism5 is faster than its predecessors, but even now it doesnt manage to outperform a plain and simple jacobi computation.
prism5 ::
     EdgeArray
  -> EdgeArray
  -> U.Vector Color
  -> PRType
  -> PRType
  -> V.Vector (V.Vector NodeID)
  -> PRType
  -> IO PageRankResult
prism5 g g' graphColoring dampingFactor threshold activationSet prValue = do
  pageRankArray <- MU.replicate (numVerts g) prValue
  let !lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      !n = fromIntegral $
          U.foldl' (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs

  -- edgeData <- V.mapM (\e -> MU.replicate (U.length e) defaultPrValue) (neighborsArray g)
  -- numLoops <- prism5' g g' pageRankArray lenNbrs prFunctionIORetardSet activationSet graphColoring threshold dampingFactor (dConst n) edgeData 0
  -- numLoops <- prism5NothingFancy' g g' pageRankArray lenNbrs prFunctionIORetardSet2 activationSet graphColoring threshold dampingFactor (dConst n) 0
  numLoops <- prism5JustTheVertexFancy g g' pageRankArray lenNbrs prFunctionIORetardSet3 activationSet graphColoring threshold dampingFactor (dConst n) 0

#ifdef DEBUG
  traceM $ "looped (prism5)" ++ show numLoops ++ " times in prism"
#endif

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n

-- NOTE: prism4 was faster when I did not pre-compute the sets, but rather just called fromList and insert.
-- This was still 3min ish compared to prism3's 1 min ish, suggesting that union is not fast enough
prism4 ::
     EdgeArray
  -> EdgeArray
  -> U.Vector Color
  -> PRType
  -> PRType
  -> V.Vector (V.Vector NodeID)
  -> PRType
  -> IO PageRankResult
prism4 g g' graphColoring dampingFactor threshold _ prValue = do
  pageRankArray <- MU.replicate (numVerts g) prValue
  let lenNbrs = V.convert $ V.map (fromIntegral . U.length) (neighborsArray g)
      n = fromIntegral $
          U.foldl' (\acc value -> if value == 0 then acc else acc + 1) 0 lenNbrs

  let nbrsAsSets = V.map (Set.fromList . U.toList) (neighborsArray g)
  numLoops <- nbrsAsSets `deepseq` prism4' nbrsAsSets g' pageRankArray lenNbrs prFunctionIO (V.toList nbrsAsSets) graphColoring threshold dampingFactor (dConst n) 0

#ifdef DEBUG
  traceM $ "looped (prism4) " ++ show numLoops ++ " times in prism"
#endif

  U.unsafeFreeze pageRankArray
    where
      dConst n = (1-dampingFactor) / n

sumTogetherVectors :: V.Vector (V.Vector Int) -> U.Vector Int
sumTogetherVectors li = U.map (\i -> V.sum (V.map (V.! i) li)) (U.enumFromN 0 (length (V.head li)))

prism5JustTheVertexFancy ::
     EdgeArray
  -> EdgeArray
  -> PageRankArray
  -> U.Vector PRType
  -> PrFunctionRetardSet3
  -> V.Vector (V.Vector NodeID)
  -> U.Vector Color
  -> PRType
  -> PRType
  -> PRType
  -> Int
  -> IO Int
-- TODO: optimize more: don't have to use boolean flags, can just write the iteration number to save time of allocation.
prism5JustTheVertexFancy !g !g' !graphArray !lenNbrs !vertexFunc !activationSet !graphColoring !threshold !dampingFactor !dConst !numLoops
  | V.all V.null activationSet = return numLoops
  | otherwise = do
#ifdef DEBUG
  traceM $ "I have " ++ (show $ V.sum (V.map V.length activationSet)) ++ " nodes to process"
#endif
  newSpaBag <- do
    mli <- MU.replicate (numVerts g) False
    V.mapM_ (\colorSet -> let numVertices = V.length colorSet in
      if numVertices < minimumParallelizableWorkload
        then V.mapM_ (\nodeId ->
          vertexFunc nodeId g g' lenNbrs graphArray dampingFactor dConst threshold >>=
          \afflicted -> when afflicted $
            U.mapM_ (\a -> -- TODO: which is faster?
              MU.write mli a True
              ) (neighbors nodeId g)
          ) colorSet
        else
          RG.gangIO RG.theGang
            (\threadId ->
              let chunkStart = nstart numVertices threadId
                  chunkSize = nstart numVertices (succ threadId) - chunkStart
              in V.mapM_ (\nodeId ->
                    vertexFunc nodeId g g' lenNbrs graphArray dampingFactor dConst threshold >>=
                    \afflicted -> when afflicted $
                    U.mapM_ (\a -> -- TODO: which is faster?
                      -- isWritten <- MU.read mli a
                      -- unless isWritten $
                      MU.write mli a True
                      ) (neighbors nodeId g)
                    ) (V.slice chunkStart chunkSize colorSet)
            )
      ) activationSet

    frzn <- U.unsafeFreeze mli
    newColorCounts <- M.replicate (V.length activationSet) 0
    let numVertices = U.length frzn
    if numVertices < minimumParallelizableWorkload
      then U.imapM_ (\i a -> when a $ M.modify newColorCounts succ (pred . fromIntegral $ graphColoring U.! i)) frzn
      else do
        mthis <- V.replicateM (RG.gangSize RG.theGang) (M.replicate (V.length activationSet) 0)
        RG.gangIO RG.theGang
            (\threadId ->
              let chunkStart = nstart numVertices threadId
                  chunkSize = nstart numVertices (succ threadId) - chunkStart
              in U.imapM_ (\i a -> when a $ M.modify (mthis V.! threadId) succ (pred . fromIntegral $ graphColoring U.! (i+chunkStart))) (U.slice chunkStart chunkSize frzn)
            )
        mfrzn <- V.mapM V.unsafeFreeze mthis
        U.imapM_ (M.write newColorCounts) (sumTogetherVectors mfrzn)

    frznCounts <- V.unsafeFreeze newColorCounts
    refinedCounts <- V.mapM M.new frznCounts
    counts <- V.unsafeThaw frznCounts
    U.mapM_ (\i -> do
      let color = (pred . fromIntegral) (graphColoring U.! i)
      M.modify counts pred color
      index <- M.read counts color
      M.write (refinedCounts V.! color) index i
      ) (U.findIndices (==True) frzn)
    V.mapM V.unsafeFreeze refinedCounts

  prism5JustTheVertexFancy g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst (succ numLoops)

-- prism5NothingFancy' ::
--      EdgeArray
--   -> EdgeArray
--   -> PageRankArray
--   -> U.Vector PRType
--   -> PrFunctionRetardSet2
--   -> V.Vector (V.Vector NodeID)
--   -> U.Vector Color
--   -> PRType
--   -> PRType
--   -> PRType
--   -> Int
--   -> IO Int
-- prism5NothingFancy' !g !g' !graphArray !lenNbrs !vertexFunc !activationSet !graphColoring !threshold !dampingFactor !dConst !numLoops
--   | V.all V.null activationSet = return numLoops
--   | otherwise = do
--   -- traceM $ "I have " ++ (show $ V.sum (V.map V.length activationSet)) ++ " nodes to process"
--   newSpaBag <- do
--     mli <- MU.replicate (numVerts g) False
--     V.mapM_ (\colorSet -> let numVertices = V.length colorSet in
--       if numVertices < minimumParallelizableWorkload
--         then V.mapM_ (\nodeId ->
--           vertexFunc nodeId g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
--           U.mapM_ (\a -> MU.write mli a True) afflicted
--           ) colorSet
--         else
--           RG.gangIO RG.theGang
--             (\threadId ->
--               let chunkStart = nstart numVertices threadId
--                   chunkSize = nstart numVertices (succ threadId) - chunkStart
--               in V.mapM_ (\nodeId ->
--                     vertexFunc nodeId g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
--                     U.mapM_ (\a -> MU.write mli a True) afflicted
--                     ) (V.slice chunkStart chunkSize colorSet)
--               )
--       ) activationSet
--
--     frzn <- U.unsafeFreeze mli
--     newColorCounts <- M.replicate (V.length activationSet) 0
--     let numVertices = U.length frzn
--     if (numVertices < minimumParallelizableWorkload)
--       then U.imapM_ (\i a -> when a $ M.modify newColorCounts succ (pred . fromIntegral $ graphColoring U.! i)) frzn
--       else do
--         mthis <- V.replicateM (RG.gangSize RG.theGang) (M.replicate (V.length activationSet) 0)
--         RG.gangIO RG.theGang
--             (\threadId ->
--               let chunkStart = nstart numVertices threadId
--                   chunkSize = nstart numVertices (succ threadId) - chunkStart
--               in U.imapM_ (\i a -> when a $ M.modify (mthis V.! threadId) succ (pred . fromIntegral $ graphColoring U.! (i+chunkStart))) (U.slice chunkStart chunkSize frzn)
--             )
--         mfrzn <- V.mapM V.unsafeFreeze mthis
--         U.imapM_ (M.write newColorCounts) (sumTogetherVectors mfrzn)
--
--     frznCounts <- V.unsafeFreeze newColorCounts
--     refinedCounts <- V.mapM (M.new) frznCounts
--     counts <- V.unsafeThaw frznCounts
--     U.mapM_ (\i -> do
--       let color = (pred . fromIntegral) (graphColoring U.! i)
--       M.modify counts pred color
--       index <- M.read counts color
--       M.write (refinedCounts V.! color) index i
--       ) (U.findIndices (==True) frzn)
--     V.mapM V.unsafeFreeze refinedCounts
--
--   prism5NothingFancy' g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst (succ numLoops) --(V.sum (V.map V.length activationSet) + numLoops)

-- prism5' ::
--      EdgeArray
--   -> EdgeArray
--   -> PageRankArray
--   -> U.Vector PRType
--   -> PrFunctionRetardSet
--   -> V.Vector (V.Vector NodeID)
--   -> U.Vector Color
--   -> PRType
--   -> PRType
--   -> PRType
--   -> V.Vector (MU.IOVector PRType)
--   -> Int
--   -> IO Int
-- prism5' !g !g' !graphArray !lenNbrs !vertexFunc !activationSet !graphColoring !threshold !dampingFactor !dConst !edgeData !numLoops
--   | V.all V.null activationSet = return numLoops
--   | otherwise = do
--   -- traceM $ "I have " ++ (show $ V.sum (V.map V.length activationSet)) ++ " nodes to process"
--   newSpaBag <- do
--     mli <- MU.replicate (numVerts g) False
--     newColorCounts <- M.replicate (V.length activationSet) 0
--     V.imapM_ (\_ colorSet -> do
--       V.mapM_ (\nodeId ->
--         vertexFunc nodeId g g' edgeData lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
--         U.mapM_ (\a -> do
--           isWritten <- MU.read mli a
--           unless isWritten $
--             MU.write mli a True >>
--             M.modify newColorCounts succ (pred . fromIntegral $ graphColoring U.! a)
--           ) afflicted
--         ) colorSet
--       ) activationSet
--     frzn <- U.unsafeFreeze mli
--     frznCounts <- V.unsafeFreeze newColorCounts
--     refinedCounts <- V.mapM (M.new) frznCounts
--     counts <- V.unsafeThaw frznCounts
--     U.mapM_ (\i -> do
--       let color = (pred . fromIntegral) (graphColoring U.! i)
--       M.modify counts pred color
--       index <- M.read counts color
--       M.write (refinedCounts V.! color) index i
--       ) (U.findIndices (==True) frzn)
--     V.mapM V.unsafeFreeze refinedCounts
--
--   prism5' g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst edgeData (succ numLoops) --(V.sum (V.map V.length activationSet) + numLoops)

prism4' ::
     V.Vector Set.IntSet
  -> EdgeArray
  -> PageRankArray
  -> U.Vector PRType
  -> PrFunction
  -> [Set.IntSet]
  -> U.Vector Color
  -> PRType
  -> PRType
  -> PRType
  -> Int
  -> IO Int
prism4' !g !g' !graphArray !lenNbrs !vertexFunc !activationSet !graphColoring !threshold !dampingFactor !dConst !numLoops
  | all Set.null activationSet = return numLoops
  | otherwise = do
  traceM $ "rolling: " ++ show numLoops
  newSpaBag <-
    mapM (\colorSet' -> do
      let !colorSet = Set.toList colorSet'
      foldM (\acc nodeId ->
        vertexFunc nodeId g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
        return $ Set.union acc afflicted
        ) Set.empty colorSet
      ) activationSet

  prism4' g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst (succ numLoops)

prism3' :: (NeighborGraph g)
  => g
  -> g
  -> MU.IOVector PRType
  -> U.Vector PRType
  -> (NodeID -> g -> g -> U.Vector PRType -> MU.IOVector PRType -> PRType -> PRType -> PRType -> IO (U.Vector NodeID))
  -> V.Vector (V.Vector NodeID)
  -> U.Vector Color
  -> PRType
  -> PRType
  -> PRType
  -> Int
  -> IO Int
prism3' !g !g' !graphArray !lenNbrs !vertexFunc !activationSet !graphColoring !threshold !dampingFactor !dConst !loopCounter
  | V.null activationSet = return loopCounter
  | otherwise = do
  newSpaBag <- do
    mli <- M.replicate numberOfColors Set.empty

    activationSet `deepseq` V.imapM_ (\_ colorGroup -> let lenColors = V.length colorGroup in
      if lenColors < minimumParallelizableWorkload
        then V.mapM_ (\node ->
              vertexFunc node g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
              U.mapM_ (\a -> mbInsert5 mli a (graphColoring U.! fromIntegral a)) afflicted
              ) colorGroup
        else RG.gangIO RG.theGang
        (\threadId ->
          let chunkStart = nstart lenColors threadId
              chunkSize = nstart lenColors (succ threadId) - chunkStart
          in V.mapM_ (\node ->
              vertexFunc node g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
              U.mapM_ (\a -> mbInsert5 mli a (graphColoring U.! fromIntegral a)) afflicted
              ) (V.slice chunkStart chunkSize colorGroup)
        )
      ) activationSet
    frzn <- V.unsafeFreeze mli
    return $ mbCollect5 frzn

  prism3' g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst (succ loopCounter)
    where
      numberOfColors = U.length graphColoring


prism2' :: (NeighborGraph g)
  => g
  -> g
  -> PageRankArray
  -> U.Vector PRType
  -> (NodeID -> g -> g -> U.Vector PRType -> PageRankArray -> PRType -> PRType -> PRType -> IO (U.Vector NodeID))
  -> V.Vector (V.Vector NodeID)
  -> U.Vector Color
  -> PRType
  -> PRType
  -> PRType
  -> V.Vector Int
  -> IO ()
prism2' g g' graphArray lenNbrs vertexFunc activationSet graphColoring threshold dampingFactor dConst colorCounts
  | V.null activationSet = return ()
  | otherwise = do
    newSpaBag <- do
        mli <- M.replicate numberOfColors V.empty
        V.imapM_ (\color count -> M.write mli color (V.replicate (fromIntegral count) (-1))) colorCounts
        indexCounts <- M.replicate numberOfColors 0

        V.mapM_ ( V.mapM_ (\node ->
            vertexFunc node g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
            U.mapM_ (\a -> mbInsert4 mli indexCounts a (graphColoring U.! fromIntegral a)) afflicted

          )) activationSet
        frzn <- V.unsafeFreeze mli
        return $ removeRedundant $ mbCollect4 frzn

    prism2' g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst colorCounts
    where
      numberOfColors = U.length graphColoring

prism' :: (NeighborGraph g)
  => g
  -> g
  -> PageRankArray
  -> U.Vector PRType
  -> (NodeID -> g -> g -> U.Vector PRType -> PageRankArray -> PRType -> PRType -> PRType -> IO (U.Vector NodeID))
  -> V.Vector (V.Vector NodeID)
  -> U.Vector Color
  -> PRType
  -> PRType
  -> PRType
  -> IO ()
prism' g g' graphArray lenNbrs vertexFunc activationSet graphColoring threshold dampingFactor dConst = do
  newSpaBag <- do
    mli <- M.replicate numberOfColors V.empty

    V.mapM_ ( V.mapM_ (\node ->
        vertexFunc node g g' lenNbrs graphArray dampingFactor dConst threshold >>= \afflicted ->
        U.mapM_ (\a -> mbInsert3 mli a (graphColoring U.! fromIntegral a)) afflicted
      )) activationSet
    frzn <- V.unsafeFreeze mli
    return $ removeRedundant $ mbCollect3 frzn

  unless (V.null activationSet) $
    prism' g g' graphArray lenNbrs vertexFunc newSpaBag graphColoring threshold dampingFactor dConst
    where
      numberOfColors = U.length graphColoring
