import VectorOperations
import           Data.Graph.GraphColoring
import qualified Data.Graph.PageRank                  as PageRank
-- import qualified Data.Graph.PageRankEC                  as PageRankLocksEC
import qualified Data.Graph.PageRankVC                  as PageRankLocksVC
import qualified Data.Graph.PageRankWithPreprocessing as PageRankPP
import qualified Data.Graph.PageRankStep as PageRankStep
import qualified Data.Graph.Prism                  as Prism
import           Criterion.Main
import           Criterion.Types                      (Config (..), Verbosity (..))
import           Data.List.Split                      (splitOn)
import           Data.Graph.EdgeArray                      (EdgeArray (..), amendSinks, readEdgeArray, invertEdgeArray)
import           Data.Graph.NeighborGraph
import Prelude
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashTable.IO as H
import           Control.DeepSeq                      (force, ($!!))

import qualified Data.Foldable as DF
import qualified Data.Sequence as SEQ
import Data.IntSet as Set
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

defaultNumberOfIterations :: Word
defaultNumberOfIterations = 5

-- https://hackage.haskell.org/package/criterion-1.1.4.0/docs/Criterion-Types.html#t:Config
benchConfig :: Config
benchConfig = Config {
      confInterval = 0.95
    , forceGC      = True
    , timeLimit    = 30
    , resamples    = 1000
    , regressions  = []
    , rawDataFile  = Nothing
    , reportFile   = Nothing
    , csvFile      = Just "../dist/benchmark.csv"
    -- , jsonFile     = Nothing
    , junitFile    = Nothing
    , verbosity    = Normal
    , template     = "default"
    }

inputPrefix :: FilePath
inputPrefix = "./input/"

setupColorEnv :: FilePath -> IO (EdgeArray, EdgeArray)
setupColorEnv filename =
  force <$> BS.readFile (inputPrefix ++ filename) >>= \fileContents ->
  let _ea = readEdgeArray fileContents
      _ea' = invertEdgeArray _ea
        in return $!! (_ea, _ea')

setupGraphEnvironment :: FilePath -> IO (EdgeArray, EdgeArray, PageRank.PRType, PageRank.PRType, GraphColoring, U.Vector Color)
setupGraphEnvironment filename = do
  fileContents <- force <$> BS.readFile (inputPrefix ++ filename)
  let (aG, aG') =
            let gr = readEdgeArray fileContents
                gr' = invertEdgeArray gr
            in force $ amendSinks gr gr'
  graphColorByVertex <- jonesPlassmanSeq aG aG' <$> genRandomNumbers (numVerts aG) 0 (numVerts aG')
  let graphColoringByColor = getGraphColoring4 graphColorByVertex
      thresholdConvergence = 1e-9
      prValue = 1.0/fromIntegral (numVerts aG)

  return (aG, aG', prValue, thresholdConvergence, graphColoringByColor, graphColorByVertex)

setupColoring :: FilePath -> Benchmark
setupColoring graphFile = env (setupColorEnv graphFile) $ \ ~(aG,aG') -> bgroup "coloring" [
    bench "coloring4" $ nfIO $
      getGraphColoring4 . jonesPlassmanSeq aG aG' <$> genRandomNumbers (numVerts aG) 0 (numVerts aG')

    , bench "greedy" $ nf (getGraphColoring4 . graphColoringGreedyInputOrder aG) aG'

    , bench "greedyNonDet" $ nfIO $ getGraphColoring4 <$> graphColoringNonDeterministic aG aG'
  ]

setupPrisms :: FilePath -> Benchmark
setupPrisms graphFile = bgroup graphName [
  env (setupGraphEnvironment graphFile) $ \ ~(aG, aG', pv, t, gc, c) -> bgroup "" [
      -- bench "prism1"  $ nfIO (Prism.prism  aG aG' c PageRank.defaultDampingFactor t gc pv)
    -- bench "prism2" $ nfIO (Prism.prism2 aG aG' c PageRank.defaultDampingFactor t gc pv)
    bench "prism3" $ nfIO (Prism.prism3 aG aG' c PageRank.defaultDampingFactor t gc pv)
    , bench "prism4" $ nfIO (Prism.prism4 aG aG' c PageRank.defaultDampingFactor t gc pv)
    , bench "prism5" $ nfIO (Prism.prism5 aG aG' c PageRank.defaultDampingFactor t gc pv)
    , bench "prism6" $ nfIO (Prism.prism6 aG aG' c PageRank.defaultDampingFactor t gc pv)
    ]
  ]
  where
    graphName = head $ splitOn "." $ last $ splitOn "/" graphFile

setupAlgorithms :: FilePath ->Benchmark
setupAlgorithms graphFile = bgroup graphName [
  env (setupGraphEnvironment graphFile) $ \ ~(aG, aG', pv, t, gc, c) -> bgroup "" [
      bench "pagerank" $ nfIO (PageRank.pageRank aG aG' PageRank.defaultDampingFactor t pv defaultNumberOfIterations)
    , bench "withPreprocessing" $ nfIO (PageRankPP.pageRankIO aG aG' PageRank.defaultDampingFactor t gc pv defaultNumberOfIterations)
    , bench "withSteps" $ nfIO (PageRankStep.pageRank aG aG' PageRank.defaultDampingFactor t pv defaultNumberOfIterations)
    , bench "vertexConsistent" $ nfIO (PageRankLocksVC.pageRank aG aG' PageRank.defaultDampingFactor t pv defaultNumberOfIterations)
    , bench "prism" $ nfIO (Prism.prism5 aG aG' c PageRank.defaultDampingFactor t gc pv)
    -- Unfortunetaly, my implementation of edge consistent locking is infeasibly slow
    -- , bench "edgeConsistent" $ nfIO (PageRankLocksEC.pageRank aG aG' PageRank.defaultDampingFactor t pv defaultNumberOfIterations)
    ]
  ]
  where
    graphName = head $ splitOn "." $ last $ splitOn "/" graphFile

nub' :: [Int] -> [Int]
nub' = go Set.empty
  where go _ [] = []
        go s (x:xs) | Set.member x s = go s xs
                    | otherwise    = x : go (Set.insert x s) xs

main :: IO ()
main = defaultMainWith benchConfig [
  bgroup "vectors" [ bench "first" $ whnf V.sum $ V.enumFromN 1 10000000
    , bench "snd" $ whnf V.sum (V.map V.sum (V.map (`V.enumFromN` 1000) (V.enumFromStepN  0 1000 10000)))
  ]

  , let num = floor 1e6 in bgroup "insertion_lazy" [
      bench "sequence" $ nf (nub' . DF.toList . appendToSequence2 num) (SEQ.singleton 1)
    , bench "slset" $ nfIO (insertToSLSet2 num)
    , bench "list" $ nf (nub' . appendToList2 num) []
    , bench "normset" $ nf (Set.toList . insertToDataSet Set.empty) num
    , bench "mutableHash" $ nfIO (H.new >>= \set -> insertToMutableSet set num >> H.toList set )
    , bench "justHaveItFixed" $ nfIO (MU.new (succ num) >>= \mli -> fixedListIteratedMultipleTimes num mli)
    
    -- Too slow! Regards both version 1 and 2 , bench "vecToNub" $ nf (V.fromList . nub' . V.toList . appendToVec2 num) (V.singleton 0)
  ]

  , bgroup "coloring" [ 
    setupColoring "200nodes4color.txt"
  ]

  , let num = floor 1e6 in bgroup "insertion_strict" [
      bench "Strict_normset" $ nf (Set.toList . insertToDataSetStrict Set.empty) num
      , bench "Strict_mutableHash" $ nfIO (H.new >>= \set -> insertToMutableSetStrict set num >> H.toList set )
      , bench "Strict_justHaveItFixed" $ nfIO (MU.new (succ num) >>= \mli -> fixedListIteratedMultipleTimesStrict num mli)

     -- sloow
      -- , bench "Strict_vecTonub'" $ nf (V.fromList . nub' . V.toList . appendToVecStrict (V.singleton 0)) num
      -- bench "Strict_sequence" $ nf (nub' . DF.toList . appendToSequenceStrict (SEQ.singleton 1)) num
      -- bench "Strict_list" $ nf (nub' . appendToListStrict [1]) num
  ]

  , bgroup "goodtimes" [ setupPrisms "socialNetwork/gowalla/loc-gowalla_edges.txt"
  ]

 -- Trivial graphs
  , bgroup "small" [ setupAlgorithms "doubleSquareEA.txt"
    , setupAlgorithms "200nodes4color.txt"
    , setupAlgorithms "500nodes4color.txt"
    , setupAlgorithms "rmatTest.txt"
  ]

  -- Stuff a good computer can handle
  , bgroup "big" [ 
      setupAlgorithms "p2p/p2p-Gnutella31.txt"
      , setupAlgorithms "web/as-skitter.txt"
      , setupAlgorithms "web/cit-Patents.txt"
      , setupAlgorithms "web/so/sx-stackoverflow.txt"
      , setupAlgorithms "web/web-Google.txt"
      , setupAlgorithms "web/wiki-Talk.txt"
      , setupAlgorithms "amazon/amazon0601.txt"
      , setupAlgorithms "amazon/amazon0312.txt"
      , setupAlgorithms "socialNetwork/gowalla/loc-gowalla_edges.txt"
    ]
 ]
