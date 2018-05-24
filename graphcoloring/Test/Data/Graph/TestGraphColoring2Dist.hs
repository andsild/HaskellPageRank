{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Data.Graph.TestGraphColoring2Dist
  (tests)
  where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Vector.Unboxed      as U
import           Test.ColoringGraphs
import           Test.HUnit

import           Data.Graph.GraphColoring
import           Data.Graph.EdgeArray

graphTest :: ByteString -> String -> [Color] -> Assertion
graphTest gFile name expected =
  let g = readEdgeArray gFile
      g' = invertEdgeArray g
      graph = mergeEdgeGraphs g g'
      y = U.toList $ graphColoring2Distance graph
        in assertEqual name expected y

test_simpleColor_singleEdge :: Test
test_simpleColor_singleEdge = TestCase $ do
  graphTest singleEdgeGraph "simpleGraph: coloring (greedy)" [1,2]

test_simpleColor_squareGraph :: Test
test_simpleColor_squareGraph = TestCase $
  graphTest squareGraph "squareGraph: coloring (greedy)"  [1,2,3,4]

test_simpleColor_doubleSquareGraph :: Test
test_simpleColor_doubleSquareGraph = TestCase $
  graphTest doubleSquareGraph "doubleSquareGraph: coloring (greedy)" [1,2,3,4,4,3,2,1]

test_simpleColor_reverseDoubleSquare :: Test
test_simpleColor_reverseDoubleSquare = TestCase $
  graphTest doubleSquareGraphReversed "doubleSquareGraphReversed: coloring (greedy)" [1,2,3,4,4,3,2,1]

test_simpleColor_hubGraph :: Test
test_simpleColor_hubGraph = TestCase $
  graphTest hubGraph "hubGraph: coloring (greedy)" [1,2,3,4]

test_simpleColor_completeGraph :: Test
test_simpleColor_completeGraph = TestCase $
  graphTest completeGraph "completeGraph: coloring (greedy)" [1,2,3,4,5]

test_simpleColor_myFavoriteGraph :: Test
test_simpleColor_myFavoriteGraph = TestCase $
  graphTest myFavoriteGraph "myFavoriteGraph: coloring (greedy)" [1,2,3,4,5,1]

tests :: Test
tests = TestList [ TestLabel "2Dist sq" test_simpleColor_squareGraph
        , TestLabel "2Dist fav" test_simpleColor_myFavoriteGraph
        , TestLabel "2Dist compl" test_simpleColor_completeGraph
        , TestLabel "2Dist hub" test_simpleColor_hubGraph
        , TestLabel "2Dist dsq" test_simpleColor_doubleSquareGraph
        , TestLabel "2Dist rev dsq" test_simpleColor_reverseDoubleSquare
        , TestLabel "2Dist singleEdge" test_simpleColor_singleEdge
  ]

