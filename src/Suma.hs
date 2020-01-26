module Suma where

import Control.Monad
import Data.Foldable
import qualified Data.IntMap.Strict as M
import Data.Maybe
import qualified Data.IntSet as S

import Suma.Types

-- Find some clause which consists of a single literal, and return its index
-- and the single literal's variable
findOneLiteralClause :: Readys -> Maybe ((Int, Literal), Readys)
findOneLiteralClause = M.minViewWithKey

-- Check that the formula is still consistent with the current assignment of
-- values to variables. That is to say, all clauses still could be true in some
-- refinement of the current assignment.
checkConsistent :: Formula -> Assignment -> Bool
checkConsistent formula assignment = all checkConsistent' formula
  where
    checkConsistent' :: Clause -> Bool
    checkConsistent' = any $ \(var,p) -> M.lookup var assignment /= Just (not p)

-- Given a variable, we consider both the case where we assume it is true, and
-- the case where we assume it is false.
splitOnVariable :: Formula -> Var -> Assignment -> [Assignment]
splitOnVariable formula variable assignment = do
  value <- [False, True]
  let assignment' = M.insert variable value assignment
  guard $ checkConsistent formula assignment'
  pure assignment'

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
solve :: Formula -> Readys -> OccurrenceLists -> Assignment -> [Assignment]
solve formula readys occLists assignment =
  case findOneLiteralClause readys of
    Just ((clauseIndex, lit), readys') -> do
      -- let assignment' = uncurry M.insert literal assignment
      (occLists', assignment') <- maybeToList $ assign lit formula readys' occLists assignment
      guard $ checkConsistent formula assignment'
      solve formula readys' occLists' assignment'
    Nothing -> case evaluateFormula formula assignment of
      Right () -> pure assignment -- we're done!
      Left Nothing -> [] -- no solution
      -- split on some unassigned variable
      Left (Just var) -> splitOnVariable formula var assignment >>= solve formula readys occLists

data ClauseStatus
  = CTrue
  | CUnready
  | CReady
  | CFalse

assign
  :: Literal
  -> Formula
  -> Readys
  -> OccurrenceLists
  -> Assignment
  -> Maybe (OccurrenceLists, Assignment)
assign lit@(var, p) formula readys occLists assignment = do
  let assignment' = uncurry M.insert lit assignment
  let occList = fromJust $ M.lookup var occLists
  -- For each clause (index) in occList:
  --   If assigning lit to true makes the clause true
  --     Remove the clause from all occurrence lists
  --     (The clause may have been in readys, so this needs to be accounted for at some point)
  --   Else if assigning lit to true makes the clause have 2 or more unassigned literals
  --     (Do nothing)
  --   Else if assigning lit to true makes the clause have a single unassigned literal
  --     Add the clause to readys
  --   Else (if assigning lit to true makes the clause have no unassigned literals)
  --     Fail
  -- Remove occList from occLists
  (readys', occLists') <- foldlM (updateState assignment') (readys, occLists) occList
  (occLists, assignment')

  where
    updateState :: Assignment -> Int -> (Readys, OccurrenceLists) -> Maybe (Readys, OccurrenceLists)
    updateState assignment' i (readys, occLists) =
      case evaluateClause (formula !! i) assignment' of
        CTrue ->
          let occLists' = M.map (S.delete i) occLists in
          let readys' = M.delete i readys in
          Just (readys', occLists')
        CUnready ->
          Just (readys, occLists)
        CReady ->
          Just (M.insert i readys, occLists)
        CFalse -> Nothing

makeOccurrenceLists :: Formula -> OccurrenceLists
makeOccurrenceLists = foldr insertVars M.empty . zip [0..]
  where
    insertVars :: (Int, Clause) -> OccurrenceLists -> OccurrenceLists
    insertVars (i, clause) occLists = foldr (insertVar i) occLists clause

    -- i is the index of the clause
    insertVar :: Int -> (Var, Parity) -> OccurrenceLists -> OccurrenceLists
    insertVar i (var, _p) = M.insertWith S.union var (S.singleton i)

-- Determine whether a formula is satisfiable. This happens when 'solve'
-- returns a non-empty list. We also return an assignment that satisfies the
-- formula if one exists.
sat :: Formula -> Maybe Assignment
sat = listToMaybe . flip solve M.empty
