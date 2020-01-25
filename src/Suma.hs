module Suma where

import Suma.Types

-- Find all clauses which consist of a single literal.
findOneLiteralClauses :: Formula -> [Literal]
findOneLiteralClauses formula = [literal | [literal] <- formula]

-- Given a single literal, we assume that it's true and adjust the formula
-- given accordingly.
--
-- If this leads to a contradiction, this returns the empty list, otherwise
-- it returns a singleton list containing the new formula.
eliminateLiteral :: Literal -> Formula -> [Formula]
eliminateLiteral (var, parity) [] = [[]]
eliminateLiteral (var, parity) (clause:formula) = case clause of
  [(var', parity')]
    | var == var' && parity /= parity' -> [] -- contradiction
  _ | (var, parity) `elem` clause -> eliminateLiteral (var, parity) formula
    | otherwise -> fmap (filter (\(var', _) -> var /= var') clause :)
      (eliminateLiteral (var, parity) formula)

-- Given a variable, we consider both the case where we assume it is true, and
-- the case where we assume it is false.
splitOnVariable :: Var -> Formula -> [Formula]
splitOnVariable variable formula =
  eliminateLiteral (variable, True) formula ++
  eliminateLiteral (variable, False) formula

-- Given a formula, reduce it as far as we can by:
-- * finding and eliminating clauses consisting of a single literal
-- * assign a truth value to an arbitrary unassagned variable, backtracking on
--   contradiction
--
-- This is approximately the DPLL algorithm, with the infelicity that we skip
-- the second rule ("pure" variables) as it is implied by the third rule and is
-- somewhat difficult to determine if it is an option.
solve :: Formula -> [Formula]
solve formula =
  case findOneLiteralClauses formula of
    (literal:_) -> eliminateLiteral literal formula >>= solve
    [] -> case formula of
      [] -> pure [] -- we're done!
      ([]:_) -> [] -- no solution
      -- split on the first variable we see
      (((var,_):_):_) -> splitOnVariable var formula >>= solve

-- Determine whether a formula is satisfiable. This happens when 'solve'
-- returns a non-empty list.
sat :: Formula -> Bool
sat = not . null . solve
