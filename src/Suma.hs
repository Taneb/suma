module Suma where

import Control.Applicative
import Control.Monad
import Data.Foldable
import qualified Data.IntMap.Strict as M
import Data.Maybe

import Suma.Types

-- Find all clauses which consist of a single literal.
findOneLiteralClauses :: Assignment -> Formula -> [Literal]
findOneLiteralClauses assignment formula = do
  clause <- formula
  -- if anything has been assigned correctly in the clause then the clause is
  -- no longer active
  --
  -- otherwise we want all but one of the literals to be assigned incorrectly
  Just [literal] <- pure $ foldrM f [] clause

  pure literal
  where
    f :: Literal -> [Literal] -> Maybe [Literal]
    f (var, p) lits = case M.lookup var assignment of
      Nothing -> Just $ (var, p):lits
      Just q | p == q -> Nothing
             | otherwise -> Just lits

-- Check that the formula is still consistent with the current assignment of
-- values to variables. That is to say, all clauses still could be true in some
-- refinement of the current assignment.
checkConsistent :: Assignment -> Formula -> Bool
checkConsistent assignment = all checkConsistent'
  where
    checkConsistent' :: Clause -> Bool
    checkConsistent' = any $ \(var,p) -> M.lookup var assignment /= Just (not p)

-- Given a variable, we consider both the case where we assume it is true, and
-- the case where we assume it is false.
splitOnVariable :: Var -> Assignment -> Formula -> Maybe Assignment
splitOnVariable variable assignment formula = branchOn True <|> branchOn False
  where
    branchOn :: Bool -> Maybe Assignment
    branchOn value =
      do
        let assignment' = M.insert variable value assignment
        guard $ checkConsistent assignment' formula
        solve formula assignment'

-- Determine whether the assignment is a witness that the formula is
-- satisfiable or is inconsistent with the formula. If it is neither, return an
-- unknown variable which could give us more information.
--
-- The return type here is chosen specifically to take advantage of
-- short-circuiting in the Either monad instance.
--
-- Left Nothing means we have found a contradiction. We abort this branch of
-- the computation.
-- Left (Just v) means we have found a clause which is not determined yet. We
-- can then split on the variable v.
-- Right () means the formula is fully determined and satisdied. We can
-- terminate.
evaluateFormula :: Formula -> Assignment -> Either (Maybe Var) ()
evaluateFormula formula assignment = mapM_ f formula
  where
    f :: Clause -> Either (Maybe Var) ()
    f [] = Left Nothing -- not satisfied and no unknowns
    f ((var, p):r) = case M.lookup var assignment of
      Nothing -> g var r
      Just q | p == q -> Right () -- this clause is satisfied
             | otherwise -> f r

    g :: Var -> Clause -> Either (Maybe Var) ()
    g v [] = Left (Just v)
    g v ((var, p):r) = case M.lookup var assignment of
      Just q | p == q -> Right ()
      _ -> g v r

-- Given a formula, reduce it as far as we can by:
-- * finding and eliminating clauses consisting of a single literal
-- * assign a truth value to an arbitrary unassagned variable, backtracking on
--   contradiction
--
-- This is approximately the DPLL algorithm, with the infelicity that we skip
-- the second rule ("pure" variables) as it is implied by the third rule and is
-- somewhat difficult to determine if it is an option.
solve :: Formula -> Assignment -> Maybe Assignment
solve formula assignment =
  case findOneLiteralClauses assignment formula of
    (literal:_) -> do
      let assignment' = uncurry M.insert literal assignment
      guard $ checkConsistent assignment' formula
      solve formula assignment'
    [] -> case evaluateFormula formula assignment of
      Right () -> pure assignment -- we're done!
      Left Nothing -> empty -- no solution
      -- split on some unassigned variable
      Left (Just var) -> splitOnVariable var assignment formula

-- Determine whether a formula is satisfiable. This happens when 'solve'
-- returns a non-empty list. We also return an assignment that satisfies the
-- formula if one exists.
sat :: Formula -> Maybe Assignment
sat = flip solve M.empty
