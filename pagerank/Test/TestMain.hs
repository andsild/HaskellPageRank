module Main
  where

import Control.Monad
import qualified Test.Data.Graph.TestPageRank as TPR
import qualified Test.Data.Graph.TestPageRankNonDet as TPRND
import qualified Test.Data.Graph.TestPageRankStep as TPSteps
import qualified Test.Data.Graph.TestPageRankPP as TPRPP
import qualified Test.Data.Graph.TestPageRankEC as TPLocksEC
import qualified Test.Data.Graph.TestPageRankVC as TPLocksVC
import Test.HUnit
import System.Exit (exitFailure)

getNumberOfFailures :: Counts -> Int
getNumberOfFailures (Counts _ _ _ f) = f

main :: IO ()
main = do
  -- runTest[T]ext(to)[T]erminal
  c <- runTestTT $ TestList [
    TPRPP.tests
    , TPR.tests
    , TPLocksEC.tests
    , TPLocksVC.tests
    , TPSteps.tests
    , TPRND.tests
    ]

  when (getNumberOfFailures c > 0)
    exitFailure
