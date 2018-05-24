{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Test.Graphs
  where

import Text.Heredoc
import Data.ByteString.Lazy.Char8 (ByteString, pack)

singleEdgeGraph :: ByteString
singleEdgeGraph = pack "0 1"

edgeToSelf :: ByteString
edgeToSelf = pack [str|0 0
  |1 2|]

squareGraph :: ByteString
squareGraph = pack [str|0 1
  |1 2
  |2 3
  |0 3|]

bigNumbers :: ByteString
bigNumbers = pack [str|4 5
  |5 6
  |6 7
  |4 7|]
