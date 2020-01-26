module Main where

import Control.Monad
import Data.Maybe
import qualified Data.IntMap as M
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
satSatisfiable = forAll genSatisfiableFormula (isJust . sat)

-- generate some formula. We don't care whether it's satisfiable
genFormula :: Gen Formula
genFormula = do
  nVars <- arbitrarySizedNatural `suchThat` (> 0)
  nClauses <- arbitrarySizedNatural `suchThat` (> 0)
  replicateM nClauses $ do
    nLit <- scale (round . sqrt . fromIntegral) arbitrarySizedNatural
    replicateM nLit $ (,) <$> choose (0, nVars - 1) <*> arbitraryBoundedEnum

satValid :: Property
satValid = forAll genFormula (\formula -> maybe True (satisfies formula) $ sat formula)
  where
    satisfies :: Formula -> Assignment -> Bool
    satisfies formula assignment
      = all (or . mapMaybe (\(v, p) -> (== p) <$> M.lookup v assignment)) formula

main :: IO ()
main = hspec $
  describe "sat" $ do
    it "Satisfiable formulas are satisfiable" $
      property satSatisfiable
    it "The assignments we generate satisfy their formulas" $
      property satValid
