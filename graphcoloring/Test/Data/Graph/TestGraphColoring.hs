{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Data.Graph.TestGraphColoring
  (tests)
  where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Vector              as V
import qualified Data.Vector.Unboxed      as U
import           Test.HUnit
import           Test.ColoringGraphs

import           Data.Graph.GraphColoring
import           Data.Graph.EdgeArray

graphTestJP :: ByteString -> String -> [Int] -> [Color] -> Assertion
graphTestJP gFile name r expected =
  let g = readEdgeArray gFile
      g' = invertEdgeArray g
      y = jonesPlassmanSeq g g' (U.fromList r)
        in assertEqual name (U.fromList expected) y

graphTest :: ByteString -> String -> [Color] -> Assertion
graphTest gFile name expected =
  let g = readEdgeArray gFile
      g' = invertEdgeArray g
      y = U.toList $ graphColoringGreedyInputOrder g g'
        in assertEqual name expected y

test_GetGraphColoring4_Sequence_Sequence :: Test
test_GetGraphColoring4_Sequence_Sequence = TestCase $
  let colors = U.enumFromN 1 10
      gc = getGraphColoring4 colors
  in assertEqual "getGraphColoring4 sequence" (V.map V.singleton (V.enumFromN 0 10)) gc

test_GetGraphColoring4_RepeatedValue_NoLossOfValue :: Test
test_GetGraphColoring4_RepeatedValue_NoLossOfValue = TestCase $
  let colors = U.replicate 10 1
      gc = getGraphColoring4 colors
  in assertEqual "getGraphColoring4 repeated" (V.singleton . V.reverse $ V.enumFromN 0 10) gc

test_jonasPlassmanSeqSimpleOneNodeMissing :: Test
test_jonasPlassmanSeqSimpleOneNodeMissing = TestCase $
  graphTestJP simpleGraphOneNodeMissing  "three nodes (one isolated) " [1..3] [1,1,2]

test_jonasPlassmanSeqSimpleOneNodeMissing_Random :: Test
test_jonasPlassmanSeqSimpleOneNodeMissing_Random = TestCase $
  graphTestJP simpleGraphOneNodeMissing  "three nodes (one isolated) [0,1,2]" [1,0,2] [1,1,2]

test_jonasPlassmanSeqSquare :: Test
test_jonasPlassmanSeqSquare = TestCase $
  graphTestJP squareGraph  "square: coloring [1,1,1,1]" [1..4] [1,2,1,2]

test_jonasPlassmanSeq_SquareGraphAndSameValues_ValidColoring :: Test
test_jonasPlassmanSeq_SquareGraphAndSameValues_ValidColoring = TestCase $
  graphTestJP squareGraph  "square: coloring [1,1,1,1]" (take 4 $ repeat 1) [1,2,1,2]

test_jonasPlassmanSeq_Wtf_DoubleSquare :: Test
test_jonasPlassmanSeq_Wtf_DoubleSquare = TestCase $
  graphTestJP doubleSquareGraph "doublesquare: coloring rand" [3,1,1,3,5,1,5,6]  [2,1,1,2,1,2,2,1]

test_jonasPlassmanSeq_Inc_DoubleSquare :: Test
test_jonasPlassmanSeq_Inc_DoubleSquare = TestCase $
  graphTestJP doubleSquareGraph "doublesquare: coloring [1..8]" [1..8]  [1,2,2,1,2,1,1,2]

test_jonasPlassmanSeq_Dec_DoubleSquare :: Test
test_jonasPlassmanSeq_Dec_DoubleSquare = TestCase $
  graphTestJP doubleSquareGraph "doublesquare: coloring [8..1]" [8,7..1]  [2,1,1,2,1,2,2,1]

test_jonasPlassmanSeq_Repeated_DoubleSquare :: Test
test_jonasPlassmanSeq_Repeated_DoubleSquare = TestCase $
  graphTestJP doubleSquareGraph "doublesquare: coloring [1,1..1]" (take 8 $ repeat 1)  [1,2,2,1,2,1,1,2]

test_simpleColor_singleEdge :: Test
test_simpleColor_singleEdge = TestCase $ do
  graphTest singleEdgeGraph "simpleGraph: coloring (greedy)" [1,2]

test_simpleColor_squareGraph :: Test
test_simpleColor_squareGraph = TestCase $
  graphTest squareGraph "squareGraph: coloring (greedy)"  [1,2,1,2]

test_simpleColor_doubleSquareGraph :: Test
test_simpleColor_doubleSquareGraph = TestCase $
  graphTest doubleSquareGraph "doubleSquareGraph: coloring (greedy)" [1,2,2,1,2,1,1,2]

test_simpleColor_reverseDoubleSquare :: Test
test_simpleColor_reverseDoubleSquare = TestCase $
  graphTest doubleSquareGraphReversed "doubleSquareGraphReversed: coloring (greedy)" [1,2,2,1,2,1,1,2]

test_simpleColor_hubGraph :: Test
test_simpleColor_hubGraph = TestCase $
  graphTest hubGraph "hubGraph: coloring (greedy)" [1,2,2,2]

test_simpleColor_completeGraph :: Test
test_simpleColor_completeGraph = TestCase $
  graphTest completeGraph "completeGraph: coloring (greedy)" [1,2,3,4,5]

test_simpleColor_myFavoriteGraph :: Test
test_simpleColor_myFavoriteGraph = TestCase $
  graphTest myFavoriteGraph "myFavoriteGraph: coloring (greedy)" [1,2,1,3,2,3]

tests :: Test
tests = TestList [ TestLabel "JP test2" test_jonasPlassmanSeqSquare
        , TestLabel "JP test3" test_jonasPlassmanSeq_SquareGraphAndSameValues_ValidColoring
        , TestLabel "JP test4" test_jonasPlassmanSeqSimpleOneNodeMissing
        , TestLabel "coloring test7" test_GetGraphColoring4_RepeatedValue_NoLossOfValue
        , TestLabel "coloring test8" test_GetGraphColoring4_Sequence_Sequence
        , TestLabel "JP test9" test_jonasPlassmanSeq_Repeated_DoubleSquare
        , TestLabel "JP test10" test_jonasPlassmanSeq_Inc_DoubleSquare
        , TestLabel "JP test10" test_jonasPlassmanSeq_Dec_DoubleSquare
        , TestLabel "JP test10" test_jonasPlassmanSeq_Repeated_DoubleSquare
        , TestLabel "JP test11" test_jonasPlassmanSeq_Wtf_DoubleSquare
        , TestLabel "coloring test12" test_simpleColor_squareGraph
        , TestLabel "coloring test13" test_simpleColor_myFavoriteGraph
        , TestLabel "coloring test14" test_simpleColor_completeGraph
        , TestLabel "coloring test15" test_simpleColor_hubGraph
        , TestLabel "coloring test16" test_simpleColor_doubleSquareGraph
        , TestLabel "coloring test17" test_simpleColor_reverseDoubleSquare
        , TestLabel "coloring test18" test_simpleColor_singleEdge
        , TestLabel "JP  onemissing" test_jonasPlassmanSeqSimpleOneNodeMissing_Random
        -- , TestLabel "test5" test_GetGraphColoring3_Sequence_Sequence
        -- , TestLabel "test6" test_GetGraphColoring3_RepeatedValue_NoLossOfValue
  ]
