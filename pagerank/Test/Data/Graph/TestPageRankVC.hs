{-# LANGUAGE OverloadedStrings #-}

module Test.Data.Graph.TestPageRankVC
  (tests)
  where


import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Vector as V

import           Test.HUnit

import           Data.Graph.PageRankVC
import           Data.Graph.InternalPageRank
import           Data.Graph.EdgeArray

import Test.PageRankGraphs

acceptablePageRankDiff :: PRType
acceptablePageRankDiff = 0.01

defaultLimitThreshold :: PRType
defaultLimitThreshold = 0.0001

getPagerankSum :: ByteString -> String -> Assertion
getPagerankSum graph name = do
  let g = readEdgeArray graph
      g' = invertEdgeArray g 
      (aG, aG') = amendSinks g g'
  y <- pageRank aG aG' defaultDampingFactor defaultLimitThreshold defaultPrValue 0
  let total = V.sum y

  assertBool ("pagerank: " ++ name ++ " yielded: " ++ show y ++ ", with sum " ++ show total ++ ", expected ~1.0")
             ((1.0+acceptablePageRankDiff) > total && (1.0-acceptablePageRankDiff) < total)

test_princetonExampleGraph :: Test
test_princetonExampleGraph = TestCase $
  getPagerankSum princetonExampleGraph "princeton"

test_doubleSquare_graphWithSinkNode :: Test
test_doubleSquare_graphWithSinkNode = TestCase $
  getPagerankSum doubleSquareGraph "doubleSquare"

test_simplePageRankTwoNodes :: Test
test_simplePageRankTwoNodes = TestCase $ do
  getPagerankSum singleEdgeGraph "simpleGraph"

test_simplePageRankThreeNodes_OneNodeHasNoEdges :: Test
test_simplePageRankThreeNodes_OneNodeHasNoEdges = TestCase $
  getPagerankSum simpleGraphOneNodeMissing "three nodes directed"

test_simplePageRankSquare :: Test
test_simplePageRankSquare = TestCase $
  getPagerankSum squareGraph "four nodes square"

test_doubleSquareRev_graphWithSinkNode :: Test
test_doubleSquareRev_graphWithSinkNode = TestCase $ 
  getPagerankSum doubleSquareGraphReversed "rev doubleSquareGraph"

test_completeGraph :: Test
test_completeGraph = TestCase $ 
  getPagerankSum completeGraph "completeGraph"

test_hubGraph :: Test
test_hubGraph = TestCase $ 
  getPagerankSum hubGraph "completeGraph"

test_favGraph :: Test
test_favGraph = TestCase $ 
  getPagerankSum myFavoriteGraph "completeGraph"

tests :: Test
tests = TestList [
          TestLabel "p test1" test_simplePageRankTwoNodes
        , TestLabel "p test2" test_simplePageRankThreeNodes_OneNodeHasNoEdges
        , TestLabel "p test3" test_simplePageRankSquare
        , TestLabel "p test4" test_doubleSquare_graphWithSinkNode
        , TestLabel "p test5" test_princetonExampleGraph
        , TestLabel "p rev" test_doubleSquareRev_graphWithSinkNode
        , TestLabel "p fav" test_favGraph
        , TestLabel "p hub" test_hubGraph
        , TestLabel "p complete" test_completeGraph
  ]
