module Graphviz.GraphColoring (
  colorGraph
  )
  where

import Data.Graph.NeighborGraph

import qualified Data.Vector.Unboxed as U
import qualified Data.Graph.Inductive as IG
import qualified Data.Text.Lazy       as L
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised()
import Data.GraphViz.Printing
import Data.GraphViz.Types.Monadic()

insertSingleNode :: (IG.DynGraph gr) => IG.Node -> gr L.Text b -> gr L.Text b
insertSingleNode x = IG.insNode (d, e)
  where d = x :: IG.Node
        e = L.pack (show x)

insertNodes :: (IG.DynGraph gr) => [Int] -> gr L.Text b -> gr L.Text b
insertNodes [] gg = gg
insertNodes [x] gg = insertSingleNode x gg
insertNodes (x:xs) gg = insertNodes xs $ insertSingleNode x gg

insertSingleEdge :: (IG.DynGraph gr) => (IG.Node,IG.Node) -> gr L.Text L.Text -> gr L.Text L.Text
insertSingleEdge x = IG.insEdge (src, dst, label)
  where src = fst x
        dst = snd x
        label = L.pack ""

insertManyEdge :: (IG.DynGraph gr) => [(IG.Node,IG.Node)] -> gr L.Text L.Text -> gr L.Text L.Text
insertManyEdge [] gg = gg
insertManyEdge [x] gg = insertSingleEdge x gg
insertManyEdge (x:xs) gg = insertManyEdge xs $ insertSingleEdge x gg

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

myColorCL :: (Integral a) => a -> ColorList
myColorCL n | n == 1 = c $ RGB 127 108 138
            | n == 2 = c $ RGB 175 177 112
            | n == 3 = c $ RGB 226 206 179
            | n == 4 = c $ RGB 172 126 100
            | otherwise = c $ RGB 0 0 0
            where c rgb = toColorList [rgb]

giveColor :: (Eq a, Num a) => a -> X11Color
giveColor n
  | n == 1 = Data.GraphViz.Green
  | n == 2 = Data.GraphViz.Blue
  | n == 3 = Data.GraphViz.Red
  | n == 4 = Data.GraphViz.Yellow
  | n == 5 = Data.GraphViz.SandyBrown
  | n == 6 = Data.GraphViz.MistyRose
  | n == 7 = Data.GraphViz.VioletRed1
  | n == 8 = Data.GraphViz.Black
  | n == 9 = Data.GraphViz.OrangeRed1
  | n == 10 = Data.GraphViz.Brown
  | otherwise = Data.GraphViz.White
  -- where
  --   n' = fromIntegral n

ex1Params :: (Eq a, Num a, U.Unbox a) => U.Vector a -> GraphvizParams Int L.Text L.Text Int L.Text
ex1Params colors = blankParams  {
      isDirected = True
    , globalAttributes = ga
    , isDotCluster = const False
    , clusterBy = clustBy
    , clusterID = Num . Int
    , fmtCluster       = clFmt
    , fmtNode          = fn
    , fmtEdge          = fe

    }
      where fn (n,l)   = [ fillColor (giveColor (colors U.! n))
                        , textLabel l]
            fe (_,_,el) = [textLabel el]
            clFmt m = [GraphAttrs [
                      FillColor (myColorCL m),
                        toLabel $ "n == " ++ show m ++ " (mod 2)"]
                        ]
            clustBy (n,l) = C (n `mod` 2) $ N (n,l)
            ga = [ GraphAttrs [ RankDir   FromLeft
                , BgColor   [toWColor White]
                ]
                , NodeAttrs  [ shape     BoxShape
                , FillColor (myColorCL (2 :: Int))
                , style     filled
                ]]

exAnd :: IG.Gr L.Text L.Text
exAnd = IG.mkGraph [ ] []

colorGraph :: (NeighborGraph g, Eq a, Num a, U.Unbox a) => g -> U.Vector a -> IO ()
colorGraph graph colors = do
  let numberOfVertices = numVerts graph - 1
      graphWithNodes = insertNodes [0..numberOfVertices] exAnd
      edgePairs = flatten $ map (\x -> zip (repeat x) $ U.toList $ neighbors x graph) [0..numberOfVertices]
      inputGraph = insertManyEdge edgePairs graphWithNodes
      fuckingColors = colors

  writeFile "dist/graphviz.dot" $ L.unpack $ renderDot $ toDot $
          graphToDot (ex1Params  fuckingColors) inputGraph

  putStrLn ""
