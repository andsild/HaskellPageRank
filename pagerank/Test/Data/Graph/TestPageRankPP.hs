{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Graph.TestPageRankPP
  (tests)
  where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Vector.Unboxed      as U

import           Test.HUnit

import           Data.Graph.PageRankWithPreprocessing as PRPP
import           Data.Graph.GraphColoring
import           Data.Graph.InternalPageRank
import           Data.Graph.EdgeArray
import           Data.Graph.NeighborGraph

import Test.PageRankGraphs

acceptablePageRankDiff :: PRType
acceptablePageRankDiff = 0.05

defaultLimitThreshold :: PRType
defaultLimitThreshold = 0.0001

getPagerankSumIO :: ByteString -> String -> Assertion
getPagerankSumIO graph name = do
  let g = readEdgeArray graph
      g' = invertEdgeArray g
      (aG, aG') = amendSinks g g'
      randomNumbers = U.enumFromN 0 (numVerts aG) 
      graphColors = getGraphColoring4 $ jonesPlassmanSeq aG aG' randomNumbers

  y <- PRPP.pageRankIO aG aG' defaultDampingFactor defaultLimitThreshold graphColors defaultPrValue 0
  let total = U.sum y

  assertBool ("pagerank pp: (" ++ name ++ ") yielded: " ++ show y ++ ", with sum " ++ show total ++ ", expected ~1.0")
             ((1.0+acceptablePageRankDiff) > total && (1.0-acceptablePageRankDiff) < total)

test_doubleSquare_graphWithSinkNode :: Test
test_doubleSquare_graphWithSinkNode = TestCase $
  getPagerankSumIO doubleSquareGraph "doubleSquareGraph"

test_doubleSquareRev_graphWithSinkNode :: Test
test_doubleSquareRev_graphWithSinkNode = TestCase $
  getPagerankSumIO doubleSquareGraphReversed "rev doubleSquareGraph"

test_completeGraph :: Test
test_completeGraph = TestCase $
  getPagerankSumIO completeGraph "completeGraph"

test_hubGraph :: Test
test_hubGraph = TestCase $
  getPagerankSumIO hubGraph "completeGraph"

test_favGraph :: Test
test_favGraph = TestCase $
  getPagerankSumIO myFavoriteGraph "completeGraph"

test_simplePageRankPPTwoNodes :: Test
test_simplePageRankPPTwoNodes = TestCase $ do
  getPagerankSumIO singleEdgeGraph "two nodes"

test_simplePageRankThreeNodes_OneNodeHasNoEdges :: Test
test_simplePageRankThreeNodes_OneNodeHasNoEdges = TestCase $ do
  getPagerankSumIO simpleGraphOneNodeMissing "simpleGraphWithSoloNode"

test_simplePageRankSquare :: Test
test_simplePageRankSquare = TestCase $ do
  getPagerankSumIO squareGraph "squareGraph"

tests :: Test
tests = TestList [
          TestLabel "pp1" test_simplePageRankPPTwoNodes
        , TestLabel "pp3" test_simplePageRankThreeNodes_OneNodeHasNoEdges
        , TestLabel "pp4" test_simplePageRankSquare
        , TestLabel "pp5" test_doubleSquare_graphWithSinkNode
        , TestLabel "pp rev" test_doubleSquareRev_graphWithSinkNode
        , TestLabel "pp fav" test_favGraph
        , TestLabel "pp hub" test_hubGraph
        , TestLabel "pp complete" test_completeGraph
  ]
