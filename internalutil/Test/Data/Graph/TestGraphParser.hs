module Data.Graph.TestGraphParser
  (tests)
  where 

import Data.List (sort)
import qualified Data.Vector.Unboxed as U

import Test.HUnit

import Data.Graph.NeighborGraph
import Data.Graph.EdgeArray

import Test.Graphs

test_EdgetoSelf_NotGivenEdgetoSelf :: Test
test_EdgetoSelf_NotGivenEdgetoSelf = TestCase $ do
  let graph = readEdgeArray edgeToSelf

  assertEqual "numberOfVertices (edgeToSelf)" 3 (numVerts graph)
  assertEqual "nbrs (edgeToSelf)" U.empty (neighbors 0 graph)

test_vertexNumberIsBig_bigNumbersDirected_doesParse :: Test
test_vertexNumberIsBig_bigNumbersDirected_doesParse = TestCase $ do
  let graph = readEdgeArray bigNumbers 

  -- This might seem counterintuitive, but its a matter of graph input requirements.
  -- Node vertices should densely occupy 0..N. Missing edges from the graph are registered
  -- as nodes without an edge. Thus, since "bignumbers" has highest vertex number 7, it 
  -- gets 8 nodes
  assertEqual "numberOfVertices (bigNumbers) directed" 8 (numVerts graph)

test_vertexNumberIsBig_bigNumbers_doesParse :: Test
test_vertexNumberIsBig_bigNumbers_doesParse = TestCase $ do
  let graph = readEdgeArray bigNumbers
  assertEqual "numberOfVertices (bigNumbers)" 8 (numVerts graph)

test_vertexNumberIsBig_bigNumbersReversed_doesParse :: Test
test_vertexNumberIsBig_bigNumbersReversed_doesParse = TestCase $ do
  let graph' = readEdgeArray bigNumbers
      graph = invertEdgeArray graph'

  assertEqual "numberOfVertices reversed (bigNumbers)" 8 (numVerts graph)

test_CountNumberVertices_SimpleGraph_NumVerts :: Test
test_CountNumberVertices_SimpleGraph_NumVerts = TestCase $ do
  let graph = readEdgeArray singleEdgeGraph
      graph' = invertEdgeArray graph
      graphMerged = mergeEdgeGraphs graph graph'
  assertEqual "numberOfVertices simple graph" 2 (numVerts graphMerged)

test_CountNumberVertices_SimpleGraphDirected_NumVerts :: Test
test_CountNumberVertices_SimpleGraphDirected_NumVerts = TestCase $ do
  let graph = readEdgeArray singleEdgeGraph 
  assertEqual "numberOfVertices simple graph directed" 2 (numVerts graph)

test_CountNumberVertices_SquareGraph_NumVerts :: Test
test_CountNumberVertices_SquareGraph_NumVerts = TestCase $ do
  let graph = readEdgeArray squareGraph
  assertEqual "numberOfVertices squaregraph" 4 (numVerts graph)

test_neighbourAssignment_SquareGraph :: Test
test_neighbourAssignment_SquareGraph = TestCase $ do
  let graph = readEdgeArray squareGraph 
  assertEqual "squareGraph" [1,3] $ sort (U.toList (neighbors 0 graph ))
  assertEqual "squareGraph" [2] $ U.toList ( neighbors 1 graph )
  assertEqual "squareGraph" [3] $ U.toList ( neighbors 2 graph )
  assertEqual "squareGraph" [] $ U.toList ( neighbors 3 graph )

test_neighbourAssignmentReversed_SquareGraph :: Test
test_neighbourAssignmentReversed_SquareGraph = TestCase $ do
  let graph' = readEdgeArray squareGraph 
      graph = invertEdgeArray graph'
  assertEqual "squareGraph" [] (sort $ U.toList ( neighbors 0 graph ))
  assertEqual "squareGraph" [0] (sort $ U.toList ( neighbors 1 graph ))
  assertEqual "squareGraph" [1] (sort $ U.toList ( neighbors 2 graph ))
  assertEqual "squareGraph" [0,2] (sort $ U.toList ( neighbors 3 graph ))

test_neighbourAssignment_SimpleGraph :: Test
test_neighbourAssignment_SimpleGraph = TestCase $ do
  let _graph = readEdgeArray singleEdgeGraph
      _graph' = invertEdgeArray _graph
      graph = mergeEdgeGraphs _graph _graph'
  assertEqual "simpleGraph" [1] $ U.toList ( neighbors 0 graph )
  assertEqual "simpleGraph" [0] $ U.toList ( neighbors 1 graph )

test_neighbourAssignmentReversed_SimpleGraph :: Test
test_neighbourAssignmentReversed_SimpleGraph = TestCase $ do
  let graph = readEdgeArray singleEdgeGraph
      graph' = invertEdgeArray graph
  assertEqual "simpleGraph" [] $ U.toList ( neighbors 0 graph' )
  assertEqual "simpleGraph" [0] $ U.toList ( neighbors 1 graph' )

test_neighbourAssignmentDirectedReversed_SimpleGraph :: Test
test_neighbourAssignmentDirectedReversed_SimpleGraph = TestCase $ do
  let graph' = readEdgeArray singleEdgeGraph 
      graph = invertEdgeArray graph'
  assertEqual "simpleGraph" [] $ U.toList ( neighbors 0 graph )
  assertEqual "simpleGraph" [0] $ U.toList ( neighbors 1 graph )

test_neighbourAssignmentDirected_SimpleGraph :: Test
test_neighbourAssignmentDirected_SimpleGraph = TestCase $ do
  let graph = readEdgeArray singleEdgeGraph 
  assertEqual "simpleGraph directed" [1] $ U.toList ( neighbors 0 graph )
  assertEqual "simpleGraph directed" [] $ U.toList ( neighbors 1 graph )

test_mergeGraph_SimpleGraph :: Test
test_mergeGraph_SimpleGraph = TestCase $ do
  let g  = readEdgeArray singleEdgeGraph 
      g' = invertEdgeArray g
      (aG, aG') = amendSinks g g'

  assertEqual "testing precondition: one neighbor for 0" [1] $ U.toList (neighbors 0 g)
  assertEqual "testing precondition: 1 inbound for 1" [0] $ U.toList (neighbors 1 g')

  assertEqual "amended neighbor  simplegraph" [1] $ U.toList (neighbors 0 aG)
  assertEqual "amended neighbor  simplegraph" [0] $ U.toList (neighbors 1 aG)
  assertEqual "amended rev neighbor  simplegraph (nbrs of 0)" [1] 
              $ U.toList (neighbors 0 aG')
  assertEqual "amended rev neighbor  simplegraph (nbrs of 1)" [0] 
              $ U.toList (neighbors 1 aG')

test_mergeGraph_SquareGraph :: Test
test_mergeGraph_SquareGraph = TestCase $ do
  let g  = readEdgeArray squareGraph 
      g' = invertEdgeArray g
      (aG, aG') = amendSinks g g'

  assertEqual "amended neighbor  squareGraph" [1,3] (sort $ U.toList (neighbors 0 aG))
  assertEqual "amended neighbor  squareGraph" [2] $ U.toList (neighbors 1 aG)
  assertEqual "amended neighbor  squareGraph" [3] $ U.toList (neighbors 2 aG)
  assertEqual "amended neighbor  squareGraph" [0,2] (sort $ U.toList (neighbors 3 aG))
  assertEqual "amended neighbor rev  squareGraph" [3] $ U.toList (neighbors 0 aG')
  assertEqual "amended neighbor rev  squareGraph" [0] $ U.toList (neighbors 1 aG')
  assertEqual "amended neighbor rev  squareGraph" [1,3] (sort $ U.toList (neighbors 2 aG'))
  assertEqual "amended neighbor rev  squareGraph" [0,2] (sort $ U.toList (neighbors 3 aG'))

test_amendGraph_BigNumbersDoesNotAmendSingleNodes :: Test
test_amendGraph_BigNumbersDoesNotAmendSingleNodes = TestCase $ do 
  let g  = readEdgeArray bigNumbers 
      g' = invertEdgeArray g
      (aG, aG') = amendSinks g g'

  assertEqual "testing precondition: no neighbor for 0 (sinkAndSource) " [] $ U.toList (neighbors 0 g)
  assertEqual "testing precondition: no neighbor for 1 (sinkAndSource) " [] $ U.toList (neighbors 1 g)
  assertEqual "testing precondition: no neighbor for 2 (sinkAndSource) " [] $ U.toList (neighbors 2 g)
  assertEqual "testing precondition: no neighbor for 3 (sinkAndSource) " [] $ U.toList (neighbors 3 g)

  assertEqual "amended: no neighbor for 0 (sinkAndSource) " [] $ U.toList (neighbors 0 aG)
  assertEqual "amended: no neighbor for 1 (sinkAndSource) " [] $ U.toList (neighbors 1 aG)
  assertEqual "amended: no neighbor for 2 (sinkAndSource) " [] $ U.toList (neighbors 2 aG)
  assertEqual "amended: no neighbor for 3 (sinkAndSource) " [] $ U.toList (neighbors 3 aG)

  assertEqual "amended: no neighbor for 0 (sinkAndSource) " [] $ U.toList (neighbors 0 aG')
  assertEqual "amended: no neighbor for 1 (sinkAndSource) " [] $ U.toList (neighbors 1 aG')
  assertEqual "amended: no neighbor for 2 (sinkAndSource) " [] $ U.toList (neighbors 2 aG')
  assertEqual "amended: no neighbor for 3 (sinkAndSource) " [] $ U.toList (neighbors 3 aG')

tests :: Test
tests = TestList [
          TestLabel "Graphparser test1" test_CountNumberVertices_SimpleGraphDirected_NumVerts
        , TestLabel "test2" test_CountNumberVertices_SquareGraph_NumVerts
        , TestLabel "test3" test_neighbourAssignmentDirectedReversed_SimpleGraph
        , TestLabel "test4" test_neighbourAssignmentDirected_SimpleGraph
        , TestLabel "test5" test_neighbourAssignmentReversed_SimpleGraph
        , TestLabel "test6" test_neighbourAssignmentReversed_SquareGraph
        , TestLabel "test7" test_neighbourAssignment_SimpleGraph
        , TestLabel "test8" test_neighbourAssignment_SquareGraph
        , TestLabel "test9" test_vertexNumberIsBig_bigNumbersDirected_doesParse
        , TestLabel "test10" test_vertexNumberIsBig_bigNumbersReversed_doesParse
        , TestLabel "test11" test_vertexNumberIsBig_bigNumbers_doesParse
        , TestLabel "test12" test_CountNumberVertices_SimpleGraph_NumVerts
        , TestLabel "test13" test_mergeGraph_SimpleGraph
        , TestLabel "test14" test_mergeGraph_SquareGraph
        , TestLabel "test15" test_amendGraph_BigNumbersDoesNotAmendSingleNodes
        , TestLabel "test16" test_EdgetoSelf_NotGivenEdgetoSelf
        ]
