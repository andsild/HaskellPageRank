{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

-- To get compiler errors in Testfile/playground
-- import qualified Test                                 as NOTUSED

import           Data.Graph.GraphColoring
import           Data.Graph.PageRank                  as PageRank
import           Data.Graph.PageRankNonDet                  as PageRankND
import           Data.Graph.PageRankEC                as PageRankLocksEC
import qualified Data.Graph.PageRankStep              as PageRankS
import           Data.Graph.PageRankVC                as PageRankLocksVC
import qualified Data.Graph.PageRankWithPreprocessing as PageRankPP
import qualified Data.Graph.Prism                     as Prism
import           Control.DeepSeq                      (deepseq, force, ($!!))
import           Control.Monad                        (void)
import           Control.Monad.Random                 (mkStdGen)
import           Data.List
import qualified Data.Vector                          as V
import qualified Data.Vector.Unboxed                  as U
import           Data.Word
import           Data.Graph.EdgeArray
import           Data.Graph.NeighborGraph
import           Graphviz.GraphColoring
import           System.Console.CmdArgs               (Data, Typeable, cmdArgs, def, help, summary, (&=))

import           Data.ByteString.Lazy.Char8           (readFile)

import           Prelude                              hiding (readFile)

import Criterion.Measurement (secs)
import Data.Time.Clock.POSIX (getPOSIXTime)

data Options = Options
  { file :: FilePath
    , method :: String
    , graphcoloring :: String
  } deriving (Data, Typeable, Show)

time_ :: IO a -> IO Double
time_ act = do
  start <- getTime
  _ <- act
  end <- getTime
  return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

thresholdConvergence :: PageRank.PRType
thresholdConvergence = 1e-6

data GraphArguments = PlainGraphArgs 
  { aG :: EdgeArray
    , aG' :: EdgeArray
    , prValue :: PRType
    , numIters :: Word
  } 
  | GraphColoringArgs
    { ga :: GraphArguments
    , graphColors :: U.Vector Color
    , graphColoring :: GraphColoring
    } 
data PageRankOutput = ImpureResult (IO PageRankResultVector) | PureResult PageRankResultVector
data Method = PageRankWithGraphColoringMethod | PageRankMethod | GraphColorMethod | ShowGraphColoring

determineMethod :: String -> Method
determineMethod "pagerank" = PageRankMethod
determineMethod "pagerankEC" = PageRankMethod
determineMethod "pagerankIO" = PageRankMethod
determineMethod "pagerankND" = PageRankMethod
determineMethod "pagerankS" = PageRankMethod
determineMethod "pagerankVC" = PageRankMethod
determineMethod "coloringGreedy" = GraphColorMethod
determineMethod "jonesPlassman" = GraphColorMethod
determineMethod "visualize" = ShowGraphColoring
determineMethod _ = PageRankWithGraphColoringMethod

data PageRankResultVector = UnboxedVector (U.Vector PRType) | BoxedVector (V.Vector PRType)
data GraphColoringResult = PureColoring (U.Vector Color, GraphColoring) | ImpureColoring (IO (U.Vector Color, GraphColoring))

doGraphColoring :: String -> EdgeArray -> EdgeArray -> GraphColoringResult
doGraphColoring "greedy" aG aG' = PureColoring (graphColors, getGraphColoring4 graphColors)
  where
    graphColors = graphColoringGreedyInputOrder aG aG'
doGraphColoring "jonesPlassman" aG aG' = PureColoring (graphColors, getGraphColoring4 graphColors)
  where 
    randomGenerator = mkStdGen 42
    randomNumbers = genRandomNumbersWithGen randomGenerator (numVerts aG) 0 (numVerts aG) 
    graphColors =  jonesPlassmanSeq aG aG' randomNumbers
doGraphColoring "nonDeterministic" aG aG' = ImpureColoring $ graphColoringNonDeterministic aG aG' >>= \graphColors -> return (graphColors, getGraphColoring4 graphColors)
doGraphColoring _ aG aG' = doGraphColoring "greedy" aG aG'

returnGraphColoring :: String -> EdgeArray -> EdgeArray -> IO (U.Vector Color, GraphColoring)
returnGraphColoring method aG aG' = do
  (graphColoringGroupedByVertex, graphColoringGroupedByColor) <- 
    case doGraphColoring method aG aG' of
      PureColoring a -> return a
      ImpureColoring a -> a
#ifdef DEBUG
  putStrLn $ "Used " ++ show (V.length graphColoringGroupedByColor) ++ " colors"
  putStrLn $ "I have " ++ show (V.sum (V.map V.length graphColoringGroupedByColor)) ++ " nodes"
  putStrLn $ "Each bag is " ++ show (V.map V.length graphColoringGroupedByColor)
#endif
  deepseq graphColoringGroupedByColor (return ())
  V.mapM_ (\x -> x `deepseq` return ()) graphColoringGroupedByColor

  return (graphColoringGroupedByVertex, graphColoringGroupedByColor)

main :: IO ()
main = do
  Options {..} <-
    cmdArgs $
    Options
    { 
      file = def &= help "File to process" :: FilePath
    , method = def &= help "Which algorithm to use" ::  String
    , graphcoloring = def &= help "Which graph coloring method to use" ::  String
    } &=
    summary "test"

  (aG, aG') <-
    force <$> readFile file >>= \fileContents ->
    let gr = readEdgeArray fileContents
        gr'= invertEdgeArray gr
    in return $!! amendSinks gr gr'
  seq thresholdConvergence (return ())
  seq PageRank.defaultDampingFactor (return ())

  let prValue = 1.0/fromIntegral (numVerts aG)
  executionTime <- time_ (
    case determineMethod method of
      PageRankMethod ->
        summarizeVector $ parse method (PlainGraphArgs aG aG' prValue 5)
      PageRankWithGraphColoringMethod -> do
        (graphColorsByVertex, graphColoringByColor) <- returnGraphColoring graphcoloring aG aG'
        summarizeVector $ parse method (GraphColoringArgs (PlainGraphArgs aG aG' prValue 5) graphColorsByVertex graphColoringByColor)
      GraphColorMethod -> 
        void $ returnGraphColoring method aG aG'
      ShowGraphColoring -> do
        (graphColorsByVertex, _) <- returnGraphColoring graphcoloring aG aG'
        colorGraph aG graphColorsByVertex
        )
  putStrLn . secs $ executionTime

summarizeVector :: PageRankOutput  -> IO ()
summarizeVector v = 
  case v of 
    ImpureResult a -> a >>= printSum
    PureResult a ->   printSum a
  where
    printSum vec = 
      let sumOfVec = case vec of 
            BoxedVector a ->  V.sum a
            UnboxedVector a -> U.sum a
        in putStrLn $ "sum is: " ++ show sumOfVec

parse :: String -> GraphArguments -> PageRankOutput
parse "pagerank" (PlainGraphArgs aG aG' prValue numIters) = ImpureResult $ PageRank.pageRank aG aG' PageRank.defaultDampingFactor thresholdConvergence prValue numIters >>= \x -> return (UnboxedVector x)
parse "pagerankVC" (PlainGraphArgs aG aG' prValue numIters) = ImpureResult $ PageRankLocksVC.pageRank aG aG' PageRank.defaultDampingFactor thresholdConvergence prValue numIters >>= \x -> return (BoxedVector x)
parse "pagerankEC" (PlainGraphArgs aG aG' prValue numIters) = ImpureResult $ PageRankLocksEC.pageRank aG aG' PageRank.defaultDampingFactor thresholdConvergence prValue numIters >>= \x -> return (BoxedVector x)
parse "pagerankS" (PlainGraphArgs aG aG' prValue numIters) = ImpureResult $ PageRankS.pageRank aG aG' PageRank.defaultDampingFactor thresholdConvergence prValue numIters >>= \x -> return (UnboxedVector x)
parse "pagerankND" (PlainGraphArgs aG aG' prValue numIters) = ImpureResult $  PageRankND.pageRank aG aG' PageRank.defaultDampingFactor thresholdConvergence prValue numIters >>= \x -> return (UnboxedVector x)
parse "pagerankPP" (GraphColoringArgs (PlainGraphArgs aG aG' prValue numIters) _ gc') = ImpureResult $ PageRankPP.pageRankIO aG aG' PageRank.defaultDampingFactor thresholdConvergence gc' prValue numIters >>= \x -> return (UnboxedVector x)
parse "prism" (GraphColoringArgs (PlainGraphArgs aG aG' prValue _) gc gc') = ImpureResult $ Prism.prism5 aG aG' gc PageRank.defaultDampingFactor thresholdConvergence gc' prValue >>= \x -> return (UnboxedVector x)
parse _ _ = PureResult (BoxedVector V.empty)
