{-
Transform Criterion CSV output, e.g. :

Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB
small/pagerank/doubleSquareEA,3.310678832690055e-4,3.2898699398306244e-4,3.3911583154518635e-4,4.8356440105859514e-5,4.414152136037782e-5,5.386431857164159e-5
"small/pagerank/parmatSmall default",1.4184731744502663e-2,1.4184731744502663e-2,1.4184731744502663e-2,1.4176441494508764e-3,1.4176441494508764e-3,1.4176441494508764e-3
small/coloring/doubleSquareEA,2.0168348830941688e-4,2.0168348830941688e-4,2.0168348830941688e-4,2.3026198216427727e-5,2.3026198216427727e-5,2.3026198216427727e-5

to become:

graphname,pagerank,coloring,withPreprocessing,id
doubleSquareEA,12224822.47615195,12120350.197539225,3,0
parmatSmall,30575568.537983067,30647908.833372578,4,1

(data is now grouped by graphname)
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import           Control.Applicative   ((<*>))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Search as LS
import qualified Data.ByteString.Lazy  as BL
import           Data.Csv              (DefaultOrdered (..), FromNamedRecord (..), ToNamedRecord (..),
                                        decodeByName, encodeDefaultOrderedByName, header, namedRecord, toNamedRecord,
                                        (.:), (.=))
import           Data.String.Utils     (replace)
import           Data.List             (find, group, sort, sortBy, genericLength)
import           Data.List.Split       (splitOn)
import qualified Data.Vector           as V
import Prelude hiding (id)

data CsvData = CsvData
    { name     :: !String
    , mean     :: !Double
    , meanLB   :: !Double
    , meanUB   :: !Double
    , stddev   :: !Double
    , stddevLB :: !Double
    , stddevUB :: !Double
    }

data LatexParsable = LatexParsable
  { graphname         :: !String
  , pagerank          :: !Double
  , coloring          :: !Double
  , coloringPrism     :: !Double
  , withPreprocessing :: !Double
  , prism :: !Double
  , edgeConsistent :: !Double
  , vertexConsistent :: !Double
  , withSteps :: !Double
  , secondsPagerank   :: !Double
  , id                :: !Int
  }
  deriving (Show)

getGraphname :: String -> String
getGraphname s = replace "_" "-" $ splitOn "/" s !! 1

instance Eq CsvData where
  (CsvData n1 _ _ _ _ _ _) == (CsvData n2 _ _ _ _ _ _) = getGraphname n1 == getGraphname n2

instance Ord CsvData where
  (CsvData n1 _ _ _ _ _ _) `compare` (CsvData n2 _ _ _ _ _ _) = getGraphname n1 `compare` getGraphname n2

instance FromNamedRecord CsvData where
  parseNamedRecord r = CsvData <$> r .: B8.pack "Name"
                        <*> r .: B8.pack "Mean"
                        <*> r .: B8.pack "MeanLB"
                        <*> r .: B8.pack "MeanUB"
                        <*> r .: B8.pack "Stddev"
                        <*> r .: B8.pack "StddevLB"
                        <*> r .: B8.pack "StddevUB"

instance DefaultOrdered LatexParsable where
  headerOrder _ = header (map B8.pack ["graphname", "pagerank", "coloring", "coloringPrism", "withPreprocessing", "prism", "edgeConsistent", "vertexConsistent", "withSteps", "secondsPagerank", "id"])
instance ToNamedRecord LatexParsable where
  toNamedRecord (LatexParsable g p c cp w p' ec vc s' s i) = namedRecord [
    B8.pack "graphname" .= g, B8.pack "pagerank" .= p,B8.pack  "coloring" .= c, B8.pack "coloringPrism" .= cp, B8.pack "withPreprocessing" .= w, B8.pack "prism" .= p', B8.pack "edgeConsistent" .= ec, B8.pack "vertexConsistent" .= vc, B8.pack "withSteps" .= s', B8.pack "secondsPagerank" .= s, B8.pack "id" .= i]

parseCsvRecord :: BL.ByteString -> V.Vector CsvData
parseCsvRecord csvData =
  case decodeByName csvData of
      Left err     -> error err
      Right (_, v) -> v

csvDataToLatexParsable :: [CsvData] -> Int -> LatexParsable
csvDataToLatexParsable li = LatexParsable g p c cp w pr ec vc st p
  where
    g   = getGraphname $ name (head li)
    p  = getData "pagerank"
    c  = getData "greedyNonDet"
    cp = getData "greedyNonDet"
    w  = getData "withPreprocessing"
    pr = getData "prism"
    st = getData "withSteps"
    ec = getData "edgeConsistent"
    vc   = getData "vertexConsistent"
    li' = map getAlgorithmAndMeantime li
    getAlgorithmAndMeantime (CsvData n m _ _ _ _ _) = (last $ splitOn "/" n, m)
    getData it = maybe 0 snd (find (\ (n, _) -> n == it) li')

updatePr :: LatexParsable -> Double -> LatexParsable
updatePr lp d = lp { pagerank = d }
updateC :: LatexParsable -> Double -> LatexParsable
updateC lp d  = lp { coloring = d }
updateW :: LatexParsable -> Double -> LatexParsable
updateW lp d  = lp { withPreprocessing = d }
updatePrism :: LatexParsable -> Double -> LatexParsable
updatePrism lp d = lp { prism = d }
updateSt :: LatexParsable -> Double -> LatexParsable
updateSt lp d = lp { withSteps = d }
updateVc :: LatexParsable -> Double -> LatexParsable
updateVc lp d = lp { vertexConsistent = d }

featureScaleLatexParsable :: [LatexParsable] -> [LatexParsable]
featureScaleLatexParsable li = 
  let
      maxVal     = maximum (map withSteps li ++ map coloringPrism li ++ map coloring li)
      minVal     = minimum (map withSteps li ++ map coloring li ++ map coloringPrism li)

      np         = featureScale pagerank li maxVal minVal
      nc         = featureScale coloring li maxVal minVal
      ncP        = featureScale coloringPrism li maxVal minVal
      nWp        = featureScale withPreprocessing li maxVal minVal
      nPri       = featureScale prism li maxVal minVal
      ns         = featureScale withSteps li maxVal minVal
      nVc        = featureScale vertexConsistent li maxVal minVal
      graphNames = map graphname li
      secondsss  = map secondsPagerank li
      idsss      = map id li
  in go np nc ncP nWp nPri ns nVc graphNames idsss secondsss []
  where
    go np' nc' ncP' nWp' nPri' ns' nVc' graphNames idsss seconds' li'
      | length np' == 1 = [LatexParsable (head graphNames) (head np') (head nc') (head ncP') (head nWp') (head nPri') (1/0) (head nVc') (head ns') (head seconds') (head idsss)]
      | otherwise =  LatexParsable (head graphNames) (head np') (head nc') (head ncP') (head nWp') (head nPri') (1/0) (head nVc') (head ns') (head seconds') (head idsss) : go (drop 1 np') (drop 1 nc') (drop 1 ncP') (drop 1 nWp') (drop 1 nPri') (drop 1 ns') (drop 1 nVc')  (drop 1 graphNames) (drop 1 idsss) (drop 1 seconds') li'
    featureScale :: (LatexParsable -> Double) -> [LatexParsable] -> Double -> Double ->[Double]
    featureScale f li' max' min' = 
      let normalize = map (\val -> (val - min') / (max' - min'))
      in normalize (map f li')

normalizeL2LatexParsable :: [LatexParsable] -> [LatexParsable]
normalizeL2LatexParsable li = 
  let np         = normalizeFields pagerank li
      nc         = normalizeFields coloring li
      ncP        = normalizeFields coloringPrism li
      nWp        = normalizeFields withPreprocessing li
      nPri       = normalizeFields prism li
      ns         = normalizeFields withSteps li
      nVc        = normalizeFields vertexConsistent li
      graphNames = map graphname li
      secondsss  = map secondsPagerank li
      idsss      = map id li
  in go np nc ncP nWp nPri ns nVc graphNames idsss secondsss []
  where
    go np' nc' ncP' nWp' nPri' ns' nVc' graphNames idsss seconds' li'
      | length np' == 1 = [LatexParsable (head graphNames) (head np') (head nc') (head ncP') (head nWp') (head nPri') (1/0) (head nVc') (head ns') (head seconds') (head idsss)]
      | otherwise =  LatexParsable (head graphNames) (head np') (head nc') (head ncP') (head nWp') (head nPri') (1/0) (head nVc') (head ns') (head seconds') (head idsss) : go (drop 1 np') (drop 1 nc') (drop 1 ncP') (drop 1 nWp') (drop 1 nPri') (drop 1 ns') (drop 1 nVc')  (drop 1 graphNames) (drop 1 idsss) (drop 1 seconds') li'
    normalizeFields :: (LatexParsable -> Double) -> [LatexParsable] -> [Double]
    normalizeFields f li' = 
      let normalize vec = map (/ (sqrt $ sum (map (^2) vec))) vec
          fieldMapped = normalize (map f li')
      in fieldMapped

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

averageResults :: [LatexParsable] -> [LatexParsable]
averageResults li = 
  let np        = average $ map pagerank li
      nc        = average $ map coloring li
      ncP       = average $ map coloringPrism li
      nWp       = average $ map withPreprocessing li
      nPri      = average $ map prism li
      ns        = average $ map withSteps li
      nVc       = average $ map vertexConsistent li
      graphName = "average"
      nId       = succ . id $ head li
  in li ++ [LatexParsable graphName np nc ncP nWp nPri 9001 nVc ns 9001 nId]
    
normalizeForPageRank :: LatexParsable -> LatexParsable
normalizeForPageRank (LatexParsable g p c cp w pr ec vc st s i) = LatexParsable g p' c' cp' w' pr' ec' vc' st' s i
  where
    p'  = normalize p
    c'  = normalize $ w+c
    cp'  = normalize $ pr+cp
    w'  = normalize w
    pr' = normalize pr
    ec' = normalize ec
    vc' = normalize vc
    st' =  normalize st
    normalize val = val

sortGT :: LatexParsable -> LatexParsable -> Ordering
sortGT l r
  | withSteps l < withSteps r = GT
  | withSteps l > withSteps r = LT
  | withSteps l == withSteps r = GT
  | otherwise = error "Something unexpected happened"

sortLatexParsable :: [LatexParsable] -> [LatexParsable]
sortLatexParsable = sortBy sortGT

modifyId :: Int -> [LatexParsable] -> [LatexParsable]
modifyId num (LatexParsable g p c cp w pr ec vc st s _ : xs) = LatexParsable g p c cp w pr ec vc st s num : modifyId (pred num) xs 
modifyId _ [] = []

main :: IO ()
main =
  BL.readFile "./dist/benchmark.csv" >>= \file -> 
    let filteredFile = LS.replace (B8.pack "//") (B8.pack "/") file
        parsed = parseCsvRecord filteredFile
        grouped = group . sort $ V.toList parsed
        lp' = map (normalizeForPageRank . (\(i,e) -> csvDataToLatexParsable e i)) (zip [0..] grouped)
        lengthLp = length lp'
        lp = averageResults $ modifyId lengthLp . sortLatexParsable $ lp'
        this = encodeDefaultOrderedByName lp
      in BL.writeFile "./dist/parsedBenchmark.dat" this
