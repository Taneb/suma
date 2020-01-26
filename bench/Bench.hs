module Main where

import Criterion.Main
import qualified Data.Text.IO as TIO
import System.Exit (die)
import System.FilePath

import Dimacs (parseDimacsCnf)
import Suma.Types (Formula)
import Suma (sat)

import Paths_suma (getDataFileName)

blocksworldDir :: FilePath
blocksworldDir = "examples" </> "blocksworld"

blocksworldBenchmark :: String -> Benchmark
blocksworldBenchmark name =
  env setup $ \formula -> bench name $ whnf sat formula
  where
    setup :: IO Formula
    setup = do
      testFile <- getDataFileName $ blocksworldDir </> name <.> "cnf"
      testCase <- TIO.readFile testFile
      case parseDimacsCnf testCase of
        Nothing -> die "Failed to parse DIMACS file"
        Just formula -> pure formula

main = defaultMain
  [ blocksworldBenchmark "anomaly"
  , blocksworldBenchmark "medium"
  ]
