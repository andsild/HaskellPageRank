module VectorOperations
  where

import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.LVar.SLSet as SL
import Control.LVish
import Data.LVar.Generic
import qualified Data.HashTable.IO as H
import Data.IntSet as Set
import Data.Set as DSet
import qualified Data.Sequence as SEQ
import Prelude
import Data.IORef
import           Control.Monad         (when)
import           Control.DeepSeq                      (force, deepseq)
import           Control.Parallel                     (pseq)

type HashTable k v = H.CuckooHashTable k v

appendToSequenceStrict :: SEQ.Seq Int -> Int -> SEQ.Seq Int
appendToSequenceStrict s num
  | num == 0 = s SEQ.|> num
  | otherwise = force $ appendToSequenceStrict (s SEQ.|> num)  (pred num)

appendToSequenceStrict2 :: Int -> SEQ.Seq Int -> SEQ.Seq Int
appendToSequenceStrict2 num s 
  | num == 0 = s SEQ.|> num
  | otherwise = force $ appendToSequenceStrict2 (pred num) (s SEQ.|> num)

appendToListStrict :: [Int] -> Int -> [Int]
appendToListStrict li num
  | num == 0 = num : li
  | otherwise = force $ appendToListStrict (num : li) (pred num)

appendToVecStrict :: V.Vector Int -> Int -> V.Vector Int
appendToVecStrict vec num
  | num == 0 = V.snoc vec num
  | otherwise = force $ appendToVecStrict (V.snoc vec num) (pred num)

appendToLvarStrict :: SL.ISet s Int -> Int -> Par e s ()
appendToLvarStrict lset num
  | num == 0 =  return ()
  | otherwise = do
  SL.insert num lset
  lset `pseq` appendToLvarStrict lset (num-1)

insertToDataSetStrict :: Set.IntSet -> Int -> Set.IntSet
insertToDataSetStrict set num
  | num == 0 = Set.insert num set
  | otherwise = force $ insertToDataSetStrict (Set.insert num set) (pred num)

insertToMutableSetStrict :: HashTable Int Int -> Int -> IO ()
insertToMutableSetStrict set num 
  | num == 0 = H.insert set num num
  | otherwise = set `seq` H.insert set num num >> insertToMutableSetStrict set (pred num)

fixedListIteratedMultipleTimesStrict :: Int -> MU.IOVector Bool -> IO (U.Vector Int)
fixedListIteratedMultipleTimesStrict num li
  | num == 0 = do
    MU.write li num True
    frzn <- U.unsafeFreeze li
    newRetList <- MU.new (MU.length li)
    x <- newIORef 0
    U.mapM_ (\a -> when a $ readIORef x >>= \ix -> MU.write newRetList ix ix >> modifyIORef x succ) frzn
    U.unsafeFreeze newRetList
  | otherwise = li `deepseq` MU.write li num True >> fixedListIteratedMultipleTimesStrict (pred num) li

appendToSequence :: SEQ.Seq Int -> Int -> SEQ.Seq Int
appendToSequence s num
  | num == 0 = s SEQ.|>  num
  | otherwise = appendToSequence (s SEQ.|> num)  (pred num)

appendToSequence2 :: Int -> SEQ.Seq Int -> SEQ.Seq Int
appendToSequence2 num s
  | num == 0 = s SEQ.|>  num
  | otherwise = appendToSequence2 (pred num) (s SEQ.|> num)

appendToList :: [Int] -> Int -> [Int]
appendToList li num
  | num == 0 = num : li
  | otherwise = appendToList (num : li)  (pred num)

appendToList2 :: Int -> [Int] -> [Int]
appendToList2 num li
  | num == 0 = num : li
  | otherwise = appendToList2 (pred num) (num : li)

appendToVec :: V.Vector Int -> Int -> V.Vector Int
appendToVec vec num
  | num == 0 = V.snoc vec num
  | otherwise = appendToVec (V.snoc vec num) (pred num)

appendToVec2 :: Int ->V.Vector Int -> V.Vector Int
appendToVec2 num vec
  | num == 0 = V.snoc vec num
  | otherwise = appendToVec2 (pred num) (V.snoc vec num)

appendToLvar :: SL.ISet s Int -> Int -> Par e s ()
appendToLvar lset num 
  | num == 0 =  return ()
  | otherwise = do
  SL.insert num lset
  appendToLvar  lset  (pred num)

appendToLvar2 :: Int -> SL.ISet s Int -> Par e s ()
appendToLvar2 num lset 
  | num == 0 =  return ()
  | otherwise = do
  SL.insert num lset
  appendToLvar2 (pred num) lset 

insertToSLSet :: Int -> IO [Int]
insertToSLSet num = runParQuasiDet $ do
  this <- SL.newEmptySet
  appendToLvar this num
  frzn <- freeze this
  return (DSet.toList $ SL.fromISet frzn)

insertToSLSet2 :: Int -> IO [Int]
insertToSLSet2 num = runParQuasiDet $ do
  this <- SL.newEmptySet
  appendToLvar2 num this
  frzn <- freeze this
  return (DSet.toList $ SL.fromISet frzn)

insertToDataSet :: Set.IntSet -> Int -> Set.IntSet
insertToDataSet set num
  | num == 0 = Set.insert num set
  | otherwise = Set.insert num (insertToDataSet set (pred num))

insertToDataSet2 :: Int -> Set.IntSet ->  Set.IntSet
insertToDataSet2 num set 
  | num == 0 = Set.insert num set
  | otherwise = insertToDataSet2 (pred num) $ Set.insert num set

insertToMutableSet :: HashTable Int Int -> Int -> IO ()
insertToMutableSet set num 
  | num == 0 = H.insert set num num
  | otherwise = H.insert set num num >> insertToMutableSet set (pred num)

fixedListIteratedMultipleTimes :: Int -> MU.IOVector Bool -> IO (U.Vector Int)
fixedListIteratedMultipleTimes num li
  | num == 0 = do
    MU.write li num True
    frzn <- U.unsafeFreeze li
    newRetList <- MU.new (MU.length li)
    x <- newIORef 0
    U.mapM_ (\a -> when a $ readIORef x >>= \ix -> MU.write newRetList ix ix >> modifyIORef x succ) frzn
    U.unsafeFreeze newRetList
  | otherwise = MU.write li num True >> fixedListIteratedMultipleTimes (pred num) li

