module Main where

import Control.Monad
import Test.QuickCheck
import Test.Hspec

import Suma
import Suma.Types

-- generate known-to-be-satisfiable formulas by coming up with an assignment of
-- variables and putting one of the variables from that assignment in each
-- clause.
genSatisfiableFormula :: Gen Formula
genSatisfiableFormula = do
  nVars <- arbitrarySizedNatural `suchThat` (> 0)
  assignments <- zip [0..] <$> replicateM nVars arbitraryBoundedEnum
  nClauses <- arbitrarySizedNatural `suchThat` (> 0)
  replicateM nClauses $ do
    nPre <- scale (\n -> round (sqrt (fromIntegral n) / 2))
      arbitrarySizedNatural
    pre  <- replicateM nPre $
      (,) <$> choose (0, nVars - 1) <*> arbitraryBoundedEnum
    var <- elements assignments
    nPost <- scale (\n -> round (sqrt (fromIntegral n) / 2))
      arbitrarySizedNatural
    post <- replicateM nPost $
      (,) <$> choose (0, nVars - 1) <*> arbitraryBoundedEnum
    pure $ pre ++ [var] ++ post

satSatisfiable :: Property
satSatisfiable = forAll genSatisfiableFormula sat

main :: IO ()
main = hspec $ do
  describe "sat" $ do
    it "Satisfiable formulas are satisfiable" $
      property satSatisfiable
