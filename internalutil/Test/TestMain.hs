module Main
  where

import Control.Monad
import qualified Data.Graph.TestGraphParser as TGP
import Test.HUnit
import System.Exit (exitFailure)

getNumberOfFailures :: Counts -> Int
getNumberOfFailures (Counts _ _ _ f) = f

main :: IO ()
main = do
  -- runTest[T]ext(to)[T]erminal
  c <- runTestTT $ TestList [
    TGP.tests
    ]

  when (getNumberOfFailures c > 0)
    exitFailure
