{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Test.PageRankGraphs
  where

import Text.Heredoc
import Data.ByteString.Lazy.Char8 (ByteString, pack)

singleEdgeGraph :: ByteString
singleEdgeGraph = pack "0 1"

simpleGraphOneNodeMissing :: ByteString
simpleGraphOneNodeMissing = pack [str|1 2
  |] 

-- Most notable about the ``doubleSquare'' is that 0 is a source node,
-- i.e. has no incoming edges
doubleSquareGraph :: ByteString
doubleSquareGraph = pack [str|0 1
  |0 2
  |1 3
  |2 3
  |0 4
  |1 5
  |2 6
  |3 7
  |4 5
  |4 6
  |5 7
  |6 7|]

doubleSquareGraphReversed :: ByteString
doubleSquareGraphReversed = pack [str|7 6
  |7 5
  |6 4
  |5 4
  |7 3
  |6 2
  |5 1
  |4 0
  |3 2
  |3 1
  |2 0
  |1 0|]

squareGraph :: ByteString
squareGraph = pack [str|0 1
  |1 2
  |2 3
  |0 3|]

hubGraph :: ByteString
hubGraph = pack [str|0 1
  |0 2
  |0 3|]

completeGraph :: ByteString
completeGraph = pack [str|0 1
  |0 2
  |0 3
  |0 4
  |1 2
  |1 3
  |1 4
  |2 3
  |2 4
  |3 4|]

myFavoriteGraph :: ByteString
myFavoriteGraph = pack [str|0 1
  |0 3
  |1 2
  |1 3
  |2 3
  |2 5
  |2 4
  |3 4
  |4 5|]

-- http://www.cs.princeton.edu/~chazelle/courses/BIB/pagerank_files/image002.gif
princetonExampleGraph :: ByteString
princetonExampleGraph = pack [str|0 1
  |0 2
  |2 0
  |1 2
  |3 2|]
