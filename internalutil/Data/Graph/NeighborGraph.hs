module Data.Graph.NeighborGraph
  where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

type NodeID = Int

class NeighborGraph g where
  neighbors :: NodeID -> g -> U.Vector NodeID
  numVerts :: g -> Int
  neighborsArray :: g -> V.Vector (U.Vector NodeID)
