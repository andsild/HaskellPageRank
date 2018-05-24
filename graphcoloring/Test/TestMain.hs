module Main
  where

import Control.Monad
import qualified Test.Data.Graph.TestPrism as TP
import qualified Test.Data.Graph.TestGraphColoring as TGC
import qualified Test.Data.Graph.TestGraphColoring2Dist as TestGraphColoring2Dist
import qualified Test.Data.Graph.TestGraphColoringNonDet as TestGraphColoringNonDet
import Test.HUnit
import System.Exit (exitFailure)

getNumberOfFailures :: Counts -> Int
getNumberOfFailures (Counts _ _ _ f) = f

main :: IO ()
main = do
  -- runTest[T]ext(to)[T]erminal
  c <- runTestTT $ TestList [
    TGC.tests
    , TP.tests
    , TestGraphColoring2Dist.tests
    , TestGraphColoringNonDet.tests
    ]

  when (getNumberOfFailures c > 0)
    exitFailure
