{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- TODO: from write to unsafewrite, etc

module Data.Graph.EdgeArray
  ( EdgeArray(..)
  , EdgeGraph(..)
  , NodeID
  , amendSinks
  , frequency
  , frequencies
  , invertEdgeArray
  , mergeEdgeGraphs
  , mergeEdgeGraphsImperative
  , readEdgeArray
  , readEdgeGraph
  ) where


import           Data.Graph.NeighborGraph

import           Control.Applicative
import           Control.DeepSeq                   (NFData, deepseq, rnf)
import           Control.Monad                     (when)
import           Control.Monad.ST
import           Prelude                           hiding (last, max, min)

import           Data.List                         (group, nub, sort)
import           Data.Ord                          (max)
import qualified Data.Vector                       as V
import qualified Data.Vector.Unboxed               as U
import qualified Data.Vector.Unboxed.Mutable       as MU

import qualified Data.ByteString.Lazy.Char8             as BS
--import Data.Attoparsec.ByteString hiding (Parser, parse, maybeResult)

instance NeighborGraph EdgeArray where
  neighbors nid EdgeArray {vectorEdges} = vectorEdges V.! nid :: U.Vector NodeID
  neighborsArray EdgeArray {vectorEdges} = vectorEdges :: V.Vector (U.Vector NodeID)
  numVerts EdgeArray {vectorEdges} = V.length vectorEdges :: Int

-- | The adjacency-graph representation.
newtype EdgeArray = EdgeArray
  { vectorEdges :: V.Vector (U.Vector NodeID)
  } deriving (Show, Eq)

-- | Old version for comparing benchmarks
data EdgeGraph = EdgeGraph
  { edges            :: U.Vector (NodeID, NodeID)
  , numberOfVertices :: Int
  } deriving (Show)

instance NFData EdgeGraph where
  rnf (EdgeGraph edges numberOfVertices) = rnf edges `deepseq` rnf numberOfVertices `deepseq` ()

instance NFData EdgeArray where
  rnf (EdgeArray vectorEdges) = rnf vectorEdges `deepseq` ()

frequency :: Ord a => [(a,a)] -> [Int]
frequency l = map length $ group $ sort (map fst l)

frequencies :: U.Vector (Int,Int) -> U.Vector Int
frequencies li = U.create $ do 
  let maxVal =  maxFromTuple $ U.maximumBy (\l r -> maxFromTuple l `compare` maxFromTuple r) li
  mli <- MU.replicate (succ maxVal) (0 :: Int)
  U.mapM_ (\(src,dst) -> when (src /= dst) $ MU.modify mli succ src) li
  return mli
    where
      maxFromTuple = uncurry max

parseEdgeList4 :: U.Vector (Int,Int) -> V.Vector (U.Vector Int)
parseEdgeList4 li = runST $ do
  let counts = frequencies li
  mli <- V.mapM (runST $ return MU.new) (U.convert counts)
  mcounts <- U.unsafeThaw counts
  U.mapM_ (\(s,d) ->
    when (s /= d) $
      MU.modify mcounts pred s >>
      MU.read mcounts s >>= \index ->
        MU.write (mli V.! s) index d
    ) li
  V.mapM U.unsafeFreeze mli

parseEdgeGraph :: [String] -> EdgeGraph
parseEdgeGraph graphAsText =
  EdgeGraph
   { edges = asVector
   , numberOfVertices = numVertices
   }
  where
    edgePairs =
      map ((\[x, y] -> (read x :: Int, read y :: Int)) . words) graphAsText :: [(Int, Int)]
    asVector = U.fromList edgePairs
    numVertices = maximum $ maximum <$> edgePairs

amendSinks :: (NeighborGraph g)
  => g
  -> g
  -> (EdgeArray, EdgeArray)
-- For every node with no outbound edges (sink), copy every incoming edge to become an outbound edge.
amendSinks g g' =
  let sinks          = V.findIndices U.null (neighborsArray g)
      inboundToSinks = V.map (\x -> (x, neighbors x g')) sinks
      -- In a graph "0 -> 1", nodesWithOutboundToSinks will be [(0,1)]
      -- In a graph "0 -> 1,2 ; 1 -> 2", nodesWithOutboundToSinks will be [(0,2),(1,2)]
      nodesWithOutboundToSinks = V.concatMap (\s -> V.map (flip (,) s) (V.convert (neighbors s g'))) sinks
  in (EdgeArray { vectorEdges = V.update (neighborsArray g) inboundToSinks },
     EdgeArray { vectorEdges = V.accumulate U.snoc (neighborsArray g') nodesWithOutboundToSinks }
     )

invertEdgeArray :: EdgeArray -> EdgeArray
invertEdgeArray ea = runST $ do
  occurences <- MU.replicate (numVerts ea) 0
  V.mapM_ (U.mapM_ (MU.modify occurences succ)) (neighborsArray ea)
  iterableOccurences <- U.freeze occurences
  retList <- V.mapM (runST $ return MU.new) (V.convert iterableOccurences)

  V.imapM_ (\i adj -> U.mapM_ (\nbr ->
        MU.modify occurences pred nbr >>
        MU.read occurences nbr >>= \index ->
        MU.write (retList V.! nbr) index i
        ) adj
    ) (neighborsArray ea)

  r <- V.mapM U.unsafeFreeze retList
  return EdgeArray { vectorEdges = r }

mergeEdgeGraphs :: EdgeArray -> EdgeArray -> EdgeArray
mergeEdgeGraphs left right = EdgeArray { vectorEdges = concatedEdgeList }
  where
    concatedEdgeList = V.imap (\i e -> (U.fromList . nub . U.toList) (e U.++ neighbors i right)) (neighborsArray left)

mergeEdgeGraphsImperative :: EdgeArray -> EdgeArray -> EdgeArray
mergeEdgeGraphsImperative left right = EdgeArray { vectorEdges = concatedEdgeList }
  where
    concatedEdgeList = V.imap (\i e -> (U.fromList . nub . U.toList) (e U.++ neighbors i right)) (neighborsArray left)

readEdgeArray :: BS.ByteString -> EdgeArray
readEdgeArray fileContents =
  let edgePairs = U.fromList (map (\line ->
          let Just (left,rest) = BS.readInt line
              Just (right,_) = (BS.readInt $ BS.tail rest)
          in (left,right)
        ) (BS.lines fileContents))
        in EdgeArray (parseEdgeList4 edgePairs)

readEdgeGraph :: String -> EdgeGraph
readEdgeGraph fileContents = parseEdgeGraph $ lines fileContents

-- zipWithMissingNumbers :: (Eq a, Num a, Monoid mo) => a -> [(a, mo)] -> [(a,mo)]
-- zipWithMissingNumbers cur ((index,values):xs)
--   | cur == index = (index, values) : zipWithMissingNumbers (cur+1) xs
--   | otherwise = (cur, mempty) : zipWithMissingNumbers (cur+1) ((index,values) : xs)
-- zipWithMissingNumbers cur [] = []
