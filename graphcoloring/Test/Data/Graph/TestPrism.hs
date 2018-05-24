{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Data.Graph.TestPrism
  (tests)
  where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Vector.Unboxed      as U

import           Test.HUnit

import           Data.Graph.GraphColoring
import           Data.Graph.Prism         as P
import           Data.Graph.EdgeArray
import           Data.Graph.NeighborGraph
import           Test.ColoringGraphs

acceptablePageRankDiff :: Double
acceptablePageRankDiff = 0.05

defaultLimitThreshold :: Double
defaultLimitThreshold = 0.000001

getPagerankSum :: ByteString -> [Char] -> IO ()
getPagerankSum graph name = do
  let g = readEdgeArray graph
      g' = invertEdgeArray g
      -- gForColoring = mergeEdgeGraphs g g'
      (aG, aG') = amendSinks g g'
      randomNumbers = U.enumFromN 0 (numVerts aG) --genRandomNumbers (numVerts g) 0 (numVerts g)
      graphColors = jonesPlassmanSeq aG aG' randomNumbers
      activationSet = getGraphColoring4 graphColors
  y <- P.prism5 aG aG' graphColors 0.15 defaultLimitThreshold activationSet 1.0
  let total = U.sum y

  assertBool ("prism-pagerank (" ++ name ++ ") yielded: " ++ show y ++ " with sum " ++ show total )
             ((1.0+acceptablePageRankDiff) > total && (1.0-acceptablePageRankDiff) < total)

test_simplePrismTwoWithSoloNodes :: Test
test_simplePrismTwoWithSoloNodes = TestCase $
  getPagerankSum simpleGraphOneNodeMissing "two nodes (one missing)"

test_simplePrismSquareGraph :: Test
test_simplePrismSquareGraph = TestCase $
  getPagerankSum squareGraph "square graph"

test_simplePrismDoubleSquare :: Test
test_simplePrismDoubleSquare = TestCase $
  getPagerankSum doubleSquareGraph "doubleSquareGraph"

test_simplePrismDoubleSquareReversed :: Test
test_simplePrismDoubleSquareReversed = TestCase $
  getPagerankSum doubleSquareGraphReversed "doubleSquareGraph rev"

test_simplePrismTwoNodes :: Test
test_simplePrismTwoNodes = TestCase $
  getPagerankSum singleEdgeGraph "two nodes"

test_simplePrismFav :: Test
test_simplePrismFav = TestCase $
  getPagerankSum myFavoriteGraph "fav"

test_simplePrismHub :: Test
test_simplePrismHub = TestCase $
  getPagerankSum hubGraph "hub"

test_prism_sinkAndSourceGraph :: Test
test_prism_sinkAndSourceGraph = TestCase $
  getPagerankSum sinkAndSourceGraph "sinkandSource"

test_prism_bigNumbers :: Test
test_prism_bigNumbers = TestCase $
  getPagerankSum bigNumbers "bigNumbers"

test_prism_princetonExample :: Test
test_prism_princetonExample = TestCase $
  getPagerankSum princetonExampleGraph "princeton"

test_prism_complete :: Test
test_prism_complete = TestCase $
  getPagerankSum completeGraph "complete"

tests :: Test
tests = TestList [ TestLabel "prism twonodes" test_simplePrismTwoNodes
  , TestLabel "prism solonode" test_simplePrismTwoWithSoloNodes
  , TestLabel "prism doublesquare" test_simplePrismDoubleSquare
  , TestLabel "prism rev" test_simplePrismDoubleSquareReversed
  , TestLabel "prism fav" test_simplePrismFav
  , TestLabel "prism hub" test_simplePrismHub
  , TestLabel "prism sinksource" test_prism_sinkAndSourceGraph
  , TestLabel "prism bignum" test_prism_bigNumbers
  , TestLabel "prims prince" test_prism_princetonExample
  , TestLabel "prism complete" test_prism_complete
  , TestLabel "prism square" test_simplePrismSquareGraph
  ]
